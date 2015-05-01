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
    attr_reader :first, :last

    def initialize(first, last)
      @first = first
      @last = last
    end

    def to_s(paren=true)
      str = ''
      str += '(' if paren
      if self.first.is_a?(Cell)
        str += self.first.to_s(true)
      else
        str += self.first.to_s
      end
      if self.last == nil
        # do nothing
      elsif self.last.is_a?(Cell)
        str += ' ' + self.last.to_s(false)
      else
        str += ' . ' + self.last.to_s
      end
      str += ')' if paren
      str
    end
  end
end
