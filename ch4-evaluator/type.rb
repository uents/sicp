#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

module Type
  class Object
#    attr_getter: type

    def eval_sequence(seq, env)
      seq.map { |exp| exp.eval(env) }.last
    end
  end
  
  class Number < Object
    def initialize(value)
      @value = value
    end

    def eval(env)
      @value
    end
  end

  class String < Object
    def initialize(value)
      @value = value
    end

    def eval(env)
      @value
    end
  end

  class Variable < Object
    def initialize(name)
      @name = name
    end

    def eval(env)
      @name
    end    
  end

  class Quote < Object
    def initialize(operands)
      @list = operands[0].map { |item| Mapper.map(item) }
    end

    def eval(env)
      @list
    end
  end

  class Assignment < Object
    def initialize(operands)
      @variable = Mapper.map(operands[0])
      @value = Mapper.map(operands[1])      
    end

    def eval(env)
      # todo
    end
  end

  class Definition < Object
    def initialize(operands)
      @variable = Mapper.map(operands[0])
      @value = Mapper.map(operands[1])      
    end

    def eval(env)
      # todo
    end
  end

  class If < Object
    def initialize(operands)
      @predicate = Mapper.map(operands[0])
      @consequent = Mapper.map(operands[1])
      @alternative = Mapper.map(operands[2])
    end

    def eval(env)
      # todo
    end
  end

  class Lambda < Object
    def initialize(operands)
      @params = operands[0].map { |param| Mapper.map(param) }
      @body = operands[1..-1].map { |exp| Mapper.map(exp) }
    end

    def eval(env)
      Procedure.new(@params, @body, env)
    end
  end

  class Begin < Object
    def initialize(operands)
      @exps = operands[0].map { |operand| Mapper.map(operand) }
    end

    def eval(env)
      # todo
    end
  end

  class Application < Object
    def initialize(operator, operands)
      @operator = operator
      @operands = operands.map { |operand| Mapper.map(operand) }
    end

    def eval(env)
      procedure = @operator.eval(env)
      arguments = @operands.map { |operand| operand.eval(env) }
      self.apply(procedure, arguments)
    end

    def apply(procedure, arguments)
      procedure.eval(arguments)
    end
  end

  class Procedure < Object
    def initialize(params, body, env)
      @params = params
      @body = body
      @env = env
    end

    def eval(arguments)
#      env.extend(@params, arguments)
      self.eval_sequence(@body, @env)
    end
  end
end

class Mapper
  include Type
  
  def self.map(nodes)
    if nodes.class == Hash
      case nodes.keys[0]
      when :NUMBER
        return Type::Number.new(nodes.values[0])
      when :STRING
        return Type::String.new(nodes.values[0])
      when :SYMBOL
        return Type::Variable.new(nodes.values[0])
      else
        raise "map: unknown atom type: " + nodes.to_s
      end
    elsif nodes.class == Array
      if nodes[0].class == Hash
        operator = nodes[0].values[0]
      elsif nodes[0].class == Array # 即時関数パターンの場合
        operator = Type::Lambda.new(nodes[0][1..-1])
      else
        raise "map: unknown list type: " + nodes.to_s
      end
      operands = nodes[1..-1]

      case operator
      when "quote"
        return Type::Quote.new(operands)
      when "set!"
        return Type::Assignment.new(operands)
      when "define"
        return Type::Definition.new(operands)
      when "if"
        return Type::If.new(operands)
      when "lambda"
        return Type::Lambda.new(operands)
      when "begin"
        return Type::Begin.new(operands)
      else
        return Type::Application.new(operator, operands)
      end
    else
      raise "map: illgual expression: " + nodes.to_s
    end
  end
end

