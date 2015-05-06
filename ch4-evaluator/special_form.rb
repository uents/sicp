# -*- coding: utf-8 -*-

module SpecialForm
  class Base
    def eval_sequence(seq, env)
      if seq.class == Array
        seq.map { |exp| exp.eval(env) }.last
      else
        seq.eval(env)
      end
    end
  end
  
  class Quote < Base
    def initialize(list)
      @list = list
    end

    def eval(env)
      @list
    end
  end

  class Assignment < Base
    def initialize(var, value)
      @variable = var
      @value = value
    end

    def eval(env)
      env.set_variable_value!(@variable.name, @value.eval(env))
      nil
    end
  end

  class Definition < Base
    def initialize(var, value)
      @variable = var
      @value = value
    end

    def eval(env)
      env.define_variable!(@variable.name, @value.eval(env))
      nil
    end
  end

  class If < Base
    def initialize(predicate, consequent, alternative)
      @predicate = predicate
      @consequent = consequent
      @alternative = alternative
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
    def initialize(params, body)
      @params = params
      @body = body
    end

    def eval(env)
      Procedure.new(@params, @body, env)
    end
  end

  class Begin < Base
    def initialize(exps)
      @exps = exps
    end

    def eval(env)
      self.eval_sequence(@exps, env)
    end
  end

  class And < Base
    def initialize(predicates)
      @predicates = predicates
    end

    def eval(env)
      if @predicates.empty?
        true
      else
        @predicates.each do |predicate|
          return false if predicate.eval(env) == false
        end
        @predicates.last
      end
    end
  end

  class Or < Base
    def initialize(predicates)
      @predicates = predicates
    end

    def eval(env)
      if @predicates.empty?
        false
      else
        @predicates.each do |predicate|
          return predicate if predicate.eval(env) != false
        end
        @predicates.last
      end
    end
  end

  class Application < Base
    def initialize(procedure, arguments)
      @procedure = procedure
      @arguments = arguments
    end

    def eval(env)
      procedure = @procedure.eval(env)
      arguments = @arguments.map { |arg| arg.eval(env) }
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

