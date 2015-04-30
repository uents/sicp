#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

load "base.rb"

module Type
  class Number
    def initialize(value)
      @value = Number::numeric(value)
    end

    def eval(env)
      @value
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
  end

  class Variable
    attr_reader :name

    def initialize(name)
      @name = name
    end

    def eval(env)
      env.lookup_variable_value(@name)
    end    
  end
end


module Form
  class Base
    def eval_sequence(seq, env)
      seq.map { |exp| exp.eval(env) }.last
    end
  end
  
  class Quote < Base
    def initialize(operands)
      @list = operands[0].map { |item| Generator.generate(item) }
    end

    def eval(env)
      @list
    end
  end

  class Assignment < Base
    def initialize(operands)
      @variable = Generator.generate(operands[0])
      @value = Generator.generate(operands[1])      
    end

    def eval(env)
      env.set_variable_value!(@variable.name, @value.eval(env))
      nil
    end
  end

  class Definition < Base
    def initialize(operands)
      @variable = Generator.generate(operands[0])
      @value = Generator.generate(operands[1])      
    end

    def eval(env)
      env.define_variable!(@variable.name, @value.eval(env))
      nil
    end
  end

  class If < Base
    def initialize(operands)
      @predicate = Generator.generate(operands[0])
      @consequent = Generator.generate(operands[1])
      @alternative = Generator.generate(operands[2])
    end

    def eval(env)
      if @predicate.eval(env)
        @consequent.eval(env)
      else
        @alternative.eval(env)
      end
    end
  end

  class Lambda < Base
    def initialize(operands)
      @params = operands[0].map { |param| Generator.generate(param) }
      @body = operands[1..-1].map { |exp| Generator.generate(exp) }
    end

    def eval(env)
      Procedure.new(@params, @body, env)
    end
  end

  class Begin < Base
    def initialize(operands)
      @exps = operands[0].map { |operand| Generator.generate(operand) }
    end

    def eval(env)
      self.eval_sequence(@exps, env)
    end
  end

  class Application < Base
    def initialize(operator, operands)
      if operator.class == Parser::Node
        @operator = Type::Variable.new(operator.value)
      else
        @operator = Form::Lambda.new(operator[1..-1])
      end
      @operands = operands.map { |operand| Generator.generate(operand) }
    end

    def eval(env)
      procedure = @operator.eval(env)
      arguments = @operands.map { |operand| operand.eval(env) }
      procedure.apply(arguments)
    end
  end

  class Procedure < Base
    def initialize(params, body, env)
      @params = params
      @body = body
      @env = env
    end

    def apply(arguments)
      env = @env.extend_environment(@params.map { |param| param.name },
                                    arguments)
      self.eval_sequence(@body, env)
    end
  end
end


begin
  Primitive.class_eval { remove_const(:CATALOG) }
rescue
end

module Primitive
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
        first = operands[0]
        last = operands[1]
        [first, last]
      rescue
        raise "cons: airty mistatch; " + operands.to_s
      end
    end
  end

  class Car
    def self.apply(operands)
      begin
        operands[0][0]
      rescue
        raise "car: contract violation; " + operands.to_s
      end
    end
  end

  class Cdr
    def self.apply(operands)
      begin
        operands[0][1]
      rescue
        raise "car: contract violation; " + operands.to_s
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


class Generator
  @@TYPES = {
    :NUMBER => Type::Number,
    :STRING => Type::String,
    :SYMBOL => Type::Variable
  }

  @@FORMS = {
    # special forms
    "quote" => Form::Quote,
    "set!" =>  Form::Assignment,
    "define" =>  Form::Definition,
    "if" => Form::If,
    "lambda" => Form::Lambda,
    "begin" => Form::Begin
  }

  def self.generate(node)
    begin
      @@TYPES[node.key].new(node.value)
    rescue
      operator = node[0]
      operands = node[1..-1]
      begin
        @@FORMS[operator.value].new(operands)
      rescue
        Form::Application.new(operator, operands)
      end
    end        
  end
end


