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

    lst = self.clone
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


module Base

  def eq?(a, b)
    a == b
  end

  def true?(exp)
    exp == :true
  end

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
    v.foldr(nil) { |x, y| cons(x, y) }
  end

  ## Pair Accessor Shorthands

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

  ###

  def number?(exp)
    exp.is_a?(Numeric)
  end

  def string?(exp)
    exp.is_a?(String)
  end

  def symbol?(exp)
    exp.is_a?(Symbol)
  end

end
