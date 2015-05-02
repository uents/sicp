# -*- coding: utf-8 -*-

load "special_form.rb"

module DerivedExp
  class Cond
    def initialize(clauses)
      @clauses = clauses
    end

    def eval(env)
      cond_to_if(@clauses, env).eval(env)
    end

    private
    def cond_to_if(clauses, env)
      predicate = clauses[0][0]
      sequence = clauses[0][1]

      if predicate == nil
        false
      elsif predicate.name == "else"
        sequence
      else
        SpecialForm::If.new(predicate,
                            sequence,
                            cond_to_if(clauses[1..-1], env))
      end
    end
  end
end
