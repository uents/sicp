# -*- coding: utf-8 -*-

load "builtin.rb"
load "special_form.rb"
load "derived_expression.rb"

class Generator
  def self.generate(node)
    begin
      case node.key
      when :NUMBER
        return Builtin::Number.new(node.value)
      when :STRING
        return Builtin::String.new(node.value)
      when :SYMBOL
        return Builtin::Variable.new(node.value)
      else
        raise "generate: unknown builtin type; " + node.to_s
      end
    rescue
      operator = node[0]
      operands = node[1..-1]

      case operator.value
      when "quote"
        list = operands[0].map { |item| self.generate(item) }
        return SpecialForm::Quote.new(list)
      when "set!"
        variable = self.generate(operands[0])
        value = self.generate(operands[1])
        return SpecialForm::Assignment.new(variable, value)
      when "define"
        variable = self.generate(operands[0])
        value = self.generate(operands[1])
        return SpecialForm::Definition.new(variable, value)
      when "if"
        predicate = self.generate(operands[0])
        consequent = self.generate(operands[1])
        alternative = self.generate(operands[2])
        return SpecialForm::If.new(predicate, consequent, alternative)
      when "lambda"
        p operands[0]
        p operands[1..-1]
        params = operands[0].map { |param| self.generate(param) }
        body = operands[1..-1].map { |exp| self.generate(exp) }
        return SpecialForm::Lambda.new(params, body)
      when "begin"
        exps = operands[0].map { |operand| self.generate(operand) }
        return SpecialForm::Begin.new(exps)
      when "and"
        predicates = operands.map { |predicate| self.generate(predicate) }
        return SpecialForm::And.new(predicates)
      when "or"
        predicates = operands.map { |predicate| self.generate(predicate) }
        return SpecialForm::Or.new(predicates)
      else
        if operator.class == Parser::Node
          procedure = Builtin::Variable.new(operator.value)
        else
          params = operator[1].map { |param| self.generate(param) }
          body = operator[2..-1].map { |exp| self.generate(exp) }
          procedure = SpecialForm::Lambda.new(params, body)
        end
        arguments = operands.map { |operand| self.generate(operand) }
        return SpecialForm::Application.new(procedure, arguments)
      end
    end        
  end
end


