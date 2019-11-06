%ifdef prometheus.counter.register
# Monitor a given source. Monitored metrics are:
# `input_latency`: Latency for producing content,
# `peak_input_latency`: Input latency peak over `latency_window`,
# `max_input_latency`: Max input latency,
# `output_latency`: Latency for outputing content,
# `peak_output_latency`: Output latency peak over `latency_window`
# `max_output_latency`: Max output latency,
# `last_output`: Last time a frame was produced,
# @category Interaction
# @param ~latency_window window of (real) time over which mean latency is computed
def prometheus.monitor(~latency_window=5.,s) =
  def mean(l) =
    n = list.length(l)
    if n == 0 then 0. else
      list.fold_right(fun (x,cur) -> cur+snd(x),0.,l)/float_of_int(n)
    end
  end

  def track_latency(mode,id) =
    labels = [("source",id)]
    prometheus.gauge.register(labels=labels,help="Mean #{mode} latency for source #{id}","#{mode}_latency")
    prometheus.gauge.register(labels=labels,help="Peak #{mode} latency for source #{id}","#{mode}_peak_latency")
    prometheus.gauge.register(labels=labels,help="Max #{mode} latency for source #{id}","#{mode}_max_latency")

    latencies = ref []
    max_latency = ref -1.

    def add_latency(l) =
      t = gettimeofday()
      latencies := list.append(!latencies,[(t,l)])

      latencies := list.filter(fun (el) ->
        t-latency_window <= fst(el),!latencies)

      def max(cur,el) =
        if cur <= snd(el) then
          snd(el)
        else
          cur
        end
      end

      peak = list.fold(max,0.,!latencies)

      if !max_latency <= peak then
        max_latency := peak
      end

      prometheus.gauge.set("#{mode}_latency", mean(!latencies))
      prometheus.gauge.set("#{mode}_peak_latency",peak)
      prometheus.gauge.set("#{mode}_max_latency",!max_latency)
    end

    add_latency
  end

  id = string.replace(pattern="\\.",fun (_) -> "",source.id(s))

  frame_duration = get(default=0.,"frame.duration")

  add_input_latency = track_latency("input",id)
  add_output_latency =  track_latency("output",id)
  add_overall_latency = track_latency("overall",id)

  last_start_time = ref 0.
  last_end_time = ref 0.

  labels = [("source",id)]
  prometheus.gauge.register(labels=labels,help="Last time source #{id} produced some data","last_input") 

  def get_frame(~start_time,~start_position,
                ~end_time,~end_position,
                ~is_partial,~metadata) =
    last_start_time := start_time
    last_end_time := end_time
    prometheus.gauge.set("last_input",end_time)
    encoded_time = seconds_of_master(end_position-start_position)
    latency = (end_time - start_time) / encoded_time
    add_input_latency(latency)
  end

  def after_output() =
    current_time = gettimeofday()
    add_output_latency((current_time-!last_end_time) / frame_duration)
    add_overall_latency((current_time-!last_start_time) / frame_duration)
  end

  source.monitor(get_frame=get_frame,
                 after_output=after_output,s)
end
%endif
