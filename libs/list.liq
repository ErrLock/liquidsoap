# Add an element at the top of a list.
# @category List
def list.cons(x,l) = list.add(x,l) end

# Return the head (first element) of a list, or `default` if the list is empty.
# @category List
# @param ~default Default value if key does not exist.
def list.hd(~default, l)
  list.case(l, default, fun (x, _) -> x)
end

# Return the list without its first element.
# @category List
def list.tl(l)
  list.case(l, [], fun (_, l) -> l)
end

# Initialize a list.
# @category List
# @param n Number of elements in the list.
# @param f Function such that `f i` is the `i`th element.
def list.init(n, f)
  def rec aux(i)
    if i == n then [] else
      list.add(f(i), aux(i+1))
    end
  end
  aux(0)
end

# Get the n-th element of a list (the first element is at position 0), or `default` if element does not exist.
# @category List
def rec list.nth(~default, l, n)
  list.case(l, default, fun (x, l) -> if n == 0 then x else list.nth(default=default, l, n-1) end)
end

# Determing whether a list is empty or not.
# @category List
def list.is_empty(l)
  list.case(l, true, fun (_, _) -> false)
end

# Compute the length of a list, i.e., the number of its elements.
# @category List
def list.length(l)
  list.ind(l, 0, fun (_, _, r) -> r+1)
end

# Check whether an element belongs to a list.
# @category List
def rec list.mem(x, l)
  list.case(l, false, fun (y, l) -> x == y or list.mem(x, l))
end

# Call a function on every element of a list.
# @category List
def rec list.iter(f, l)
  list.case(l, (), fun (x, l) -> begin f(x); list.iter(f, l) end)
end

# Map a function on every element of a list.
# @category List
def rec list.map(f, l)
  list.case(l, [], fun (x, l) -> list.cons(f(x), list.map(f, l)))
end

# Map a function on every element of a list, along with its index.
# @category List
def rec list.mapi(f, l)
  n = ref(0)
  def f(x) =
    i = !n
    n := i + 1
    f(i, x)
  end
  list.map(f, l)
end

# Fold a function on every element of a list: `list.fold(f,x1,[e1,..,en]) is f(...f(f(x1,e1),e2)...,en)`.
# @category List
# @param f Function `f` for which `f(x,e)` which will be called on every element `e` with the current value of `x`, returning the new value of `x`.
# @param x Initial value x1, to be updated by successive calls of `f(x,e)`.
def rec list.fold(f, x, l)
  list.case(l, x, fun (e, l) -> list.fold(f, f(x, e), l))
end

# Fold a function on every element of a list. Similar to `list.fold` but
# iterates from the right of the list. It is slighly more efficient than
# `list.fold`.
# @category List
# @param f Function `f` for which `f(x,e)` which will be called on every element `e` with the current value of `x`, returning the new value of `x`.
# @param x Initial value x1, to be updated by successive calls of `f(x,e)`.
def list.fold_right(f, x, l)
  list.ind(l, x, fun (e, l, r) -> f(e, r))
end

# Filter a list according to a predicate. The order in which elements are
# handled is not specified (and is currently implemented from the right).
# @category List
def list.filter(p, l)
  # list.case(l, [], fun (x, l) -> if p(x) then list.cons(x, list.filter(p, l)) else list.filter(p, l) end)
  list.ind(l, [], fun(x, _, l) -> if p(x) then list.cons(x, l) else l end)
end

# Remove the first occurrence of a value from a list.
# @category List
def list.remove(x, l)
  def rec aux(k, l)
    list.case(l, k([]),
    fun (y, l) -> if x == y then k(l) else aux(fun (l) -> k(list.cons(y, l)), l) end)
  end
  aux(fun (l) -> l, l)
end

# Concatenate two lists.
# @category List
def list.append(l, m)
  list.ind(l, m, fun (x, l, r) -> list.cons(x, r))
end

# Add a new last element to the list.
# @category List
# @flag hidden
def list.snoc(y, l)
  list.ind(l, list.cons(y, []), fun (x, l, r) -> list.cons(x, r))
end

# Revert list order.
# @category List
def list.rev(l)
  list.ind(l, [], fun (x, l, r) -> list.snoc(x, r))
end

# Associate a value to a key in an association list.
# @category List
# @param ~default Value returned if the key is not found
def rec list.assoc(~default, key, l)
  def f(x, l)
    let (k, v) = x
    if k == key then v else list.assoc(default=default, key, l) end
  end
  list.case(l, default, f)
end

# Remove the first pair from an associative list.
# @category List
# @param key Key of pair to be removed.
# @param l List of pairs (key,value).
def rec list.remove_assoc(key, l)
  def f(x, l)
    let (k, v) = x
    if k == key then l else list.cons((k,v), list.remove_assoc(key, l)) end
  end
  list.case(l, [], f)
end

# Check that a predicate is satisfied for every element in a list.
# @category List
# @param p Predicate.
# @param l List.
def rec list.for_all(p, l)
  def f(x, l)
    if not p(x) then
      false
    else
      list.for_all(p, l)
    end
  end
  list.case(l, true, f)
end

# Check that a predicate is satisfied for some element in a list.
# @category List
# @param p Predicate.
# @param l List.
def rec list.exists(p, l)
  def f(x, l)
    if p(x) then
      true
    else
      list.exists(p, l)
    end
  end
  list.case(l, false, f)
end

# list.mem_assoc(key,l) returns true if l contains a pair (key,value).
# @category List
# @param a Key to look for
# @param l List of pairs (key,value)
def list.mem_assoc(a,l)
  def f(cur, el) =
    if not cur then
      fst(el) == a
    else
      cur
    end
  end
  list.fold(f, false, l)
end

# list.filter_assoc(key,l) returns all the elements of the form (key, value) from l.
# @category List
# @param k Key to look for
# @param l List of pairs (key,value)
def list.filter_assoc(k,l)
  list.filter(fun (el) -> fst(el) == k, l)
end
