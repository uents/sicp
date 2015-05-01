# -*- coding: utf-8 -*-

load "misc.rb"
load "builtin.rb"

begin
  PrimProc.class_eval { remove_const(:CATALOG) }
rescue
end

module PrimProc
  class Equal
    def self.apply(operands)
      operands[0] == operands[1]
    end
  end
  
  class Add
    def self.apply(operands)
      operands.reduce(:+)
    end
  end

  class Sub
    def self.apply(operands)
      operands.reduce(:-)
    end
  end

  class Multiply
    def self.apply(operands)
      operands.reduce(:*)
    end
  end

  class Devide
    def self.apply(operands)
      operands.reduce(:/)
    end
  end

  class Cons
    def self.apply(operands)
      begin
        BuiltIn::Pair.new(operands[0], operands[1])
      rescue
        raise "cons: airty mistatch; " + operands.to_s
      end
    end
  end

  class Car
    def self.apply(operands)
      begin
        operands[0].first
      rescue
        raise "car: contract violation; " + operands.to_s
      end
    end
  end

  class Cdr
    def self.apply(operands)
      begin
        operands[0].last
      rescue
        raise "cdr: contract violation; " + operands.to_s
      end
    end
  end

  class List
    def self.apply(operands)
      operands.foldr(nil) { |x, y| Cons.apply([x, y]) }
    end
  end

  CATALOG = {
    "=" => Equal,
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

