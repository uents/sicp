# -*- coding: utf-8 -*-

load "misc.rb"
load "builtin.rb"

begin
  PrimProc.class_eval { remove_const(:CATALOG) }
rescue
end

module PrimProc
  class IsEqual
    def self.apply(arguments)
      arguments[0] == arguments[1]
    end
  end

  class IsLessThan
    def self.apply(arguments)
      arguments[0] < arguments[1]
    end
  end

  class IsGreaterThan
    def self.apply(arguments)
      arguments[0] > arguments[1]
    end
  end

  class Add
    def self.apply(arguments)
      arguments.reduce(:+)
    end
  end

  class Sub
    def self.apply(arguments)
      arguments.reduce(:-)
    end
  end

  class Multiply
    def self.apply(arguments)
      arguments.reduce(:*)
    end
  end

  class Devide
    def self.apply(arguments)
      arguments.reduce(:/)
    end
  end

  class Cons
    def self.apply(arguments)
      if arguments.length != 2
        raise "cons: airty mistatch; " + arguments.to_s
      end
      Builtin::Pair.new(arguments[0], arguments[1])
    end
  end

  class Car
    def self.apply(arguments)
      begin
        arguments[0].car
      rescue
        raise "car: contract violation; " + arguments.to_s
      end
    end
  end

  class Cdr
    def self.apply(arguments)
      begin
        arguments[0].cdr
      rescue
        raise "cdr: contract violation; " + arguments.to_s
      end
    end
  end

  class List
    def self.apply(arguments)
      arguments.foldr(nil) { |x, y| Builtin::Pair.new(x, y) }
    end
  end

  class Print
    def self.apply(arguments)
      arguments.each { |arg| print arg.to_s + " "}
      print "\n"
      nil
    end
  end

  class Apply
    def self.apply(arguments)
      proc = arguments[0] 
      list = arguments[-1].class == Builtin::Pair ? 
               arguments[1..-2] + arguments[-1].to_a :
               arguments[1..-1]
      proc.apply(list)
    end
  end

  class Map
    def self.apply(arguments)
      if arguments.length < 2
        raise "map: airty mistatch; " + arguments.to_s
      end
      proc = arguments[0]
      lists = arguments[1..-1]
      self.iter(proc, lists)
    end

    private
    def self.iter(proc, lists)
      if lists[0] == nil
        nil
      else
        firsts = lists.map { |list| list.car }
        rests = lists.map { |list| list.cdr }
        Builtin::Pair.new(proc.apply(firsts),
                          self.iter(proc, rests))
      end
    end
  end
  
  CATALOG = {
    "=" => IsEqual,
    "<" => IsLessThan,
    ">" => IsGreaterThan,    
    "+" => Add,
    "-" => Sub,
    "*" => Multiply,
    "/" => Devide,
    "cons" => Cons,
    "car" => Car,
    "cdr" => Cdr,
    "list" => List,
    "apply" => Apply,
    "map" => Map,
    "print" => Print,
  }
end

