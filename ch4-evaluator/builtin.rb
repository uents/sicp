# -*- coding: utf-8 -*-

module Builtin
  class Number
    def initialize(value)
      @value = Number::numeric(value)
    end

    def eval(env)
      @value
    end

    def to_s()
      @value.to_s
    end

    private
    def self.numeric(str)
      begin
        return Integer(str)
      rescue
        begin
          return Float(str)
        rescue
          return str
        end
      end
    end
  end
      
  class String
    def initialize(value)
      @value = value.gsub(/\"/, '')
    end

    def eval(env)
      @value
    end

    def to_s()
      @value
    end
  end

  class Variable
    attr_reader :name

    def initialize(name)
      @name = name
    end

    def eval(env)
      env.lookup_variable_value(@name)
    end

    def to_s()
      @name.to_s
    end
  end

  class Pair
    def initialize(first, rest)
      @first = first
      @rest = rest
    end

    def car()
      @first
    end

    def cdr()
      @rest
    end

    def to_a()
      list = []
      if self.car.class == Pair
        list.push(self.car.to_a)
      else
        list.push(self.car)
      end
      if self.cdr == nil
        # do nothing
      elsif self.cdr.class == Pair
        list += self.cdr.to_a
      else
        list.push(self.cdr.to_a)
      end
      pp list
      list
    end

    def to_s(paren=true)
      str = ''
      str += '(' if paren
      if self.car.class == Pair
        str += self.car.to_s(true)
      else
        str += self.car.to_s
      end
      if self.cdr == nil
        # do nothing
      elsif self.cdr.class == Pair
        str += ' ' + self.cdr.to_s(false)
      else
        str += ' . ' + self.cdr.to_s
      end
      str += ')' if paren
      str
    end
  end
end
