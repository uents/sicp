# coding: utf-8

## Extend modules and classes

module Enumerable
  def foldr(*args, &block)
    case args.length
    when 2
      init, proc = args
    when 1
      if block_given?
        init = args.first
      else
        init = :nil
        proc = args.first
      end
    when 0
      init = :nil
    else
      raise "..."
    end

    lst = self.dup
    if init == :nil
      init = self.last
      lst.pop
    end

#    p "lst:" + lst.to_s + " init:" + init.to_s + " proc:" + proc.to_s + " block:" + block.to_s

    memo = init
    lst.reverse_each do |item|
      if proc
        memo = item.send(proc, memo)
      else
        memo = block.call(item, memo)
      end
    end      
    memo
  end
end


## Emulate racket/base module

module Base

  ## 4.1 Booleans and Equality

  def not(v)
    v == false
  end
  
  def eq?(v1, v2)
    v1.equal?(v2)
  end

  ## 4.2 Numbers

  def number?(exp)
    exp.is_a?(Numeric)
  end

  ## 4.3 Strings
  
  def string?(exp)
    exp.is_a?(String)
  end

  ## 4.6 Symbols?
  
  def symbol?(exp)
    exp.is_a?(Symbol)
  end

  ## 4.9 Pairs and Lists
  
  ### Pair Constructors and Selectors

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
    if (pair?(p))
      p[0]
    else
      raise "car: invalid argument: " + p.to_s
    end
  end

  def cdr(p)
    if (pair?(p))
      p[1..-1][0]
    else
      raise "cdr: invalid argument: " + p.to_s
    end
  end

  def list(*v)
    v.foldr(nil) { |x, y| cons(x, y) }
  end

  ### List iteration

  def apply(proc, lst)
    iter = ->(proc, memo, rest) do
      if not(pair?(rest))
        memo
      else
        iter(proc, proc.call(memo, car(rest)), cdr(rest))
      end
    end
    iter.call(proc, car(lst), cdr(lst))
  end

  def mono_map(proc, lst)
    if null?(lst)
      nil
    else
      cons(proc.call(car(lst)),
           mono_map(proc, cdr(lst)))
    end
  end

  def map(proc, *v)
    iter = ->(proc, lst) do
      if null?(car(lst))
        nil
      else
        cons(apply(proc, mono_map(method(:car), lst)),
             iter.call(proc, mono_map(method(:cdr), lst)))
      end
    end
    
    if (v.length <= 0)
      raise "map: arity mismatch"
    else
      lst = v.foldr(nil) { |x, y| cons(x, y) }
      iter.call(proc, lst)
    end
  end

  ### Pair Accessor Shorthands

  def caar(p)
    car(car(p))
  end

  def cadr(p)
    car(cdr(p))
  end

  def cdar(p)
    cdr(car(p))
  end

  def cddr(p)
    cdr(cdr(p))
  end

  def caaar(p)
    car(car(car(p)))
  end

  def caadr(p)
    car(car(cdr(p)))
  end

  def cadar(p)
    car(cdr(car(p)))
  end

  def caddr(p)
    car(cdr(cdr(p)))
  end

  def cdaar(p)
    cdr(car(car(p)))
  end

  def cdadr(p)
    cdr(car(cdr(p)))
  end

  def cddar(p)
    cdr(cdr(car(p)))
  end

  def cdddr(p)
    cdr(cdr(cdr(p)))
  end

  def caaaar(p)
    car(car(car(car(p))))
  end

  def caaadr(p)
    car(car(car(cdr(p))))
  end

  def caadar(p)
    car(car(cdr(car(p))))
  end

  def caaddr(p)
    car(car(cdr(cdr(p))))
  end

  def cadaar(p)
    car(cdr(car(car(p))))
  end

  def cadadr(p)
    car(cdr(car(cdr(p))))
  end

  def caddar(p)
    car(cdr(cdr(car(p))))
  end

  def cadddr(p)
    car(cdr(cdr(cdr(p))))
  end

  def cdaaar(p)
    cdr(car(car(car(p))))
  end
  
  def cdaadr(p)
    cdr(car(car(cdr(p))))
  end

  def cdadar(p)
    cdr(car(cdr(car(p))))
  end

  def cdaddr(p)
    cdr(car(cdr(cdr(p))))
  end

  def cddaar(p)
    cdr(cdr(car(car(p))))
  end

  def cddadr(p)
    cdr(cdr(car(cdr(p))))
  end

  def cdddar(p)
    cdr(cdr(cdr(car(p))))
  end

  def cddddr(p)
    cdr(cdr(cdr(cdr(p))))
  end

end
