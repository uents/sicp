
## Pair Constructors and Selectors

def pair?(p)
  p.is_a?(Array) && p.length >= 2
end

def null?(x)
  x == nil
end

def cons(a, d)
  [a].push(d)
end

def car(p)
  p[0]
end

def cdr(p)
  p[1..-1][0]
end

def list(*v)
  if v.empty?
    nil
  else
    p "@@@"
    cons(v.shift, list(v))
  end
end


## Pair Accessor Shorthands

def caar(p)
  car(car(p))
end

def cadr(p)
  car(cdr(p))
end

def cddr(p)
  cdr(cdr(p))
end

def caddr(p)
  car(cdr(cdr(p)))
end


