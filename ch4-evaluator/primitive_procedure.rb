# -*- coding: utf-8 -*-

load "misc.rb"
load "builtin.rb"

begin
  PrimProc.class_eval { remove_const(:CATALOG) }
rescue
end

module PrimProc
  class Equal
    def self.apply(arguments)
      arguments[0] == arguments[1]
    end
  end

  class LessThan
    def self.apply(arguments)
      arguments[0] < arguments[1]
    end
  end

  class GreaterThan
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
      begin
        BuiltIn::Pair.new(arguments[0], arguments[1])
      rescue
        raise "cons: airty mistatch; " + arguments.to_s
      end
    end
  end

  class Car
    def self.apply(arguments)
      begin
        arguments[0].first
      rescue
        raise "car: contract violation; " + arguments.to_s
      end
    end
  end

  class Cdr
    def self.apply(arguments)
      begin
        arguments[0].last
      rescue
        raise "cdr: contract violation; " + arguments.to_s
      end
    end
  end

  class List
    def self.apply(arguments)
      arguments.foldr(nil) { |x, y| Cons.apply([x, y]) }
    end
  end

  CATALOG = {
    "=" => Equal,
    "<" => LessThan,
    ">" => GreaterThan,    
    "+" => Add,
    "-" => Sub,
    "*" => Multiply,
    "/" => Devide,
    "cons" => Cons,
    "car" => Car,
    "cdr" => Cdr,
    "list" => List,
  }
end

