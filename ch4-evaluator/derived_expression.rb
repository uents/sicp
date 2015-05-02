# -*- coding: utf-8 -*-

load "special_form.rb"

module DerivedExp
  class Cond
    def initialize(predicates, sequences)
      @clauses = predicates.zip(sequences)
    end

    def eval(env)
      cond_to_if(@clauses).eval(env)
    end

    private
    def cond_to_if(clauses)
      predicate = clauses[0][0]
      sequence = clauses[0][1]

      if predicate == nil
        false
      elsif predicate.name == "else"
        sequence
      else
        SpecialForm::If.new(predicate,
                            sequence,
                            cond_to_if(clauses[1..-1]))
      end
    end
  end

  class Let
    def initialize(variables, expressions, body)
      @variables = variables
      @expressions = expressions
      @body = body
    end

    def eval(env)
      let_to_combination().eval(env)
    end

    private
    def let_to_combination()
      SpecialForm::Application.new(SpecialForm::Lambda.new(@variables, @body),
                                   @expressions)
    end
  end
end
