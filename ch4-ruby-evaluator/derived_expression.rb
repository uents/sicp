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
    def initialize(name, variables, expressions, body)
      @name = name
      @variables = variables
      @expressions = expressions
      @body = body
    end

    def eval(env)
      let_to_combination().eval(env)
    end

    private
    def let_to_combination()
      if @name != nil
        SpecialForm::Begin.new(
          [SpecialForm::Definition.new(@name,
                                       SpecialForm::Lambda.new(@variables, @body)),
           SpecialForm::Application.new(@name, @expressions)])
      else
        SpecialForm::Application.new(SpecialForm::Lambda.new(@variables, @body),
                                     @expressions)
      end
    end
  end

  class LetAster
    def initialize(variables, expressions, body)
      @clauses = variables.zip(expressions)
      @body = body
    end

    def eval(env)
      let_aster_to_nested_let(@clauses).eval(env)
    end

    private
    def let_aster_to_nested_let(clauses)
      if clauses.empty?
        @body
      else
        variable = clauses[0][0]
        expression = clauses[0][1]
        DerivedExp::Let.new([variable],
                            [expression],
                            let_aster_to_nested_let(clauses[1..-1]))
      end
    end
  end

  class While
    def initialize(predicate, body)
      @predicate = predicate
      @body = body
    end

    def eval(env)
      while_to_named_let().eval(env)
    end

    private
    def while_to_named_let()
      DerivedExp::Let.new(Builtin::Variable.new("loop"),
                          [],
                          [],
                          SpecialForm::If.new(
                            @predicate,
                            SpecialForm::Begin.new(
                              @body + [SpecialForm::Application.new(
                                        Builtin::Variable.new("loop"),
                                        [])]
                            ),
                            Builtin::Variable.new("false")))
    end
  end
end
