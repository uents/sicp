#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require_relative "./base"

class Evaluator
  ## 4.1.1 評価機の核

  #### eval
  def eval(exp, env)
    if self_evaluating?(exp)
      exp
    elsif variable?(exp)
      lookup_variable_value(exp, env)
    elsif quoted?(exp)
      text_of_quotation(exp)
    elsif assignment?(exp)
      eval_assignment(exp, env)
    elsif definition?(exp)
      eval_definition(exp, env)
    elsif if?(exp)
      eval_if(exp, env)
    elsif lambda?(exp)
      params = lambda_parameters(exp)
      body = lambda_body(exp)
      make_procedure(params, body, env)
    elsif let?(exp) ## ex 4.6
      eval(let_to_combination(exp), env)
    elsif lets?(exp) ## ex 4.7
      eval(lets_to_nested_let(exp), env)
    elsif begin?(exp)
      exps = begin_actions(exp)
      eval_sequence(exps, env)
    elsif cond?(exp)
      eval(cond_to_if(exp), env)
    elsif and?(exp) ## ex 4.4
      eval_and(exp, env)
    elsif or?(exp)  ## ex 4.4
      eval_or(exp, env)
    elsif application?(exp)
      procedure = eval(operator(exp), env)
      arguments = list_of_values(operands(exp), env)
      apply(procedure, arguments)
    else
      raise "eval: unknown expression type: " + exp.to_s
    end
  end

  #### apply
  def apply(procedure, arguments)
    if primitive_procedure?(procedure)
      apply_primitive_prodecure(procedure, arguments)
    elsif compound_procedure?(procedure)
      body = procedure_body(procedure)
      params = procedure_parameter(procedure)
      env = procedure_environment(procedure)
      eval_sequence(body, extend_environment(params, arguments, env))
    else
      raise "apply: unknown procedure type: " + procedure
    end
  end

  #### 手続きの引数
  def list_of_values(exps, env)
    if no_operands?(exps)
      nil
    else
      exps.map { |exp| eval(exp, env) }
    end
  end

  #### if
  def eval_if(exp, env)
    if eval(if_predicate(exp), env)
      eval(if_consequent(exp), env)
    else
      eval(if_alternative(exp), env)
    end
  end

  #### 並び
  def eval_sequence(exps, env)
    if last_exp?(exps)
      eval(first_exp(exps), env)
    else
      eval(first_exp(exps), env)
      eval_sequence(rest_exps(exps), env)
    end
  end

  #### 代入
  def eval_assignment(exp, env)
    var = assignment_variable(exp)
    value = eval(assignment_value(exp), env)
    set_variable_value!(var, value, env)
    :ok
  end

  #### 定義
  def eval_definition(exp, env)
    var = definition_variable(exp)
    value = eval(definition_value(exp), env)
    define_variable!(var, value, env)
    :ok
  end


  ## 4.1.2 式の定義

  #### 数値/文字列
  def self_evaluating?(exp)
    if exp.is_a?(Numeric)
      true
    elsif exp.is_a?(String)
      true
    else
      false
    end
  end

  #### 変数
  def variable?(exp)
    exp.is_a?(Symbol)
  end

  #### タグ付きリストのチェック
  def tagged_list?(exp, tag)
    exp.is_a?(Array) && exp[0] == tag
  end

  #### クオート
  def quoted?(exp)
    tagged_list?(exp, :quote)
  end

  def text_of_quotation(exp)
    exp.rest
  end

  #### 代入
  def assignment?(exp)
    tagged_list?(exp, :set!)
  end  

  def assignment_variable(exp)
    exp[1]
  end

  def assignment_value(exp)
    exp[2]
  end

  #### 定義
  def definition?(exp)
    tagged_list?(exp, :define)
  end

  def definition_variable(exp)
    if symbol?(exp.rest.first) # 変数定義
      exp[1]
    else                       # 手続きを定義
      exp[1][0]
    end
  end

  def definition_value(exp)
    if symbol?(exp.first)
      exp[2]
    else
      params = exp[1][1..-1]
      body = exp[2]
      make_lambda(params, body)
    end
  end

  #### lambda
  def lambda?(exp)
    tagged_list?(exp, :lambda)
  end

  def lambda_parameters(exp)
    exp[1]
  end

  def lambda_body(exp)
    exp[2]
  end

  def make_lambda(params, body)
    [:lambda, params, body]
  end

  #### if
  def if?(exp)
    tagged_list?(exp, :if)
  end

  def if_predicate(exp)
    exp[1]
  end

  def if_consequent(exp)
    exp[2]
  end

  def if_alternative(exp)
    if exp[3]
      exp[3]
    else
      false
    end
  end

  def make_if(predicate, consequent, alternative)
    [:if, predicate, consequent, alternative]
  end

  #### 並び
  def last_exp?(exps)
    exps.empty?
  end

  def first_exp(exps)
    exps[0]
  end

  def rest_exps(exps)
    exps[1..-1]
  end

  #### begin
  def begin?(exp)
    tagged_list?(exp, :begin)
  end

  def begin_actions(exp)
    exp[1..-1]
  end

  def sequence_to_exp(seq)
    if seq == nil
      seq
    elsif last_exp?(seq)
      first_exp(seq)
    else
      make_begin(seq)
    end
  end

  def make_begin(seq)
    [:begin] + seq
  end

  #### 手続きの適用
  def application?(exp)
    exp.is_a?(Array) && exp.length >= 2
  end

  def operator(exp)
    exp[0]
  end

  def operands(exp)
    exp[1..-1]
  end

  def no_operands?(ops)
    ops.empty?
  end

  def first_operand(ops)
    ops[0]
  end

  def rest_operands(exp)
    ops[1..-1]
  end
  
  #### cond
  def cond?(exp)
    tagged_list?(exp, :cond)
  end

  def cond_clauses(exp)
    exp[1..-1]
  end

  def cond_else_clause?(clause)
    cond_predicate(clause) == :else
  end

  def cond_predicate(clause)
    clause[0]
  end

  def cond_actions(clause)
    clause[1..-1]
  end

  def cond_to_if(exp)
    expand_clauses(cond_clauses(exp))
  end

  def expand_clauses(clauses)
    if clauses.empty?
      false                # no else clause
    else
      first = clauses[0]
      rest = clauses[1..-1]

      if cond_else_clause?(first)
        if rest.empty?
          sequence_to_exp(cond_actions(first))
        else
          raise "cond_to_if: else clauses isn't last " +
                clauses.to_s
        end
      else
        predicate = cond_predicate(first)
        consequent = sequence_to_exp(cond_actions(first))
        alternative = expand_clauses(rest)
        make_if(predicate, consequent, alternative)
      end
    end
  end

  #### ex 4.4
  def and?(exp)
    tagged_list?(exp, :and)
  end

  def and_clauses(exp)
    exp[1..-1]
  end
  
  def eval_and(exp, env)
    clauses = and_clauses(exp)
    
    if clauses.empty?
      true
    else
      clauses.each do |clause|
        if false?(eval(clause, env))
          return false
        end
      end
      eval(clauses.last, env)
    end
  end
    
  def or?(exp)
    tagged_list?(exp, :or)
  end

  def or_clauses(exp)
    exp[1..-1]
  end

  def eval_or(exp, env)
    clauses = or_clauses(exp)

    if clauses.empty?
      false
    else
      clauses.each do |clause|
        unless false?(eval(clause, env))
          return eval(clause, env)
        end
      end
      false
    end
  end

  #### ex 4.6
  def let?(exp)
    tagged_list?(exp, :let)
  end

  def let_bindings(exp)
    exp[1]
  end

  def let_variables(exp)
    bindings = let_bindings(exp)
    bindings.map do |pair|
      pair[0]
    end
  end

  def let_expressions(exp)
    bindings = let_bindings(exp)
    bindings.map do |pair|
      pair[1]
    end
  end

  def let_body(exp)
    exp[2]
  end

  def let_to_combination(exp)
    variables = let_variables(exp)
    expressions = let_expressions(exp)
    body = let_body(exp)
    [make_lambda(variables, body)] + expressions
  end

  #### ex 4.7
  def lets?(exp)
    tagged_list?(exp, :lets)
  end

  def lets_bindngs(exp)
    exp[1]
  end

  def lets_body(exp)
    exp[2]
  end

  def lets_to_nested_let(exp)
    bindings = lets_bindngs(exp)
    body = lets_body(exp)

    expand = ->(bindings) do
      if bindings.empty?
        body
      else
        [:let, [bindings[0]],
         expand.call(bindings[1..-1])]
      end
    end
    expand.call(bindings)
  end
  
  #### ex 4.8
  def let?(exp)
    tagged_list?(exp, :let)
  end

  def named_let?(exp)
    !exp[1].is_a?(Array)
  end

  def let_bindings(exp)
    if named_let?(exp)
      exp[2]
    else
      exp[1]
    end
  end

  def let_variables(exp)
    bindings = let_bindings(exp)
    bindings.map do |pair|
      pair[0]
    end
  end

  def let_expressions(exp)
    bindings = let_bindings(exp)
    bindings.map do |pair|
      pair[1]
    end
  end

  def let_body(exp)
    if named_let?(exp)
      exp[3]
    else
      exp[2]
    end
  end

  def let_var(exp)
    if named_let?(exp)
      exp[1]
    else
      false # no var clause
    end
  end

  def let_to_combination(exp)
    variables = let_variables(exp)
    expressions = let_expressions(exp)
    body = let_body(exp)
    var = let_var(exp)

    if named_let?(exp)
      [:begin,
       [:define, [var] + variables, body],
       [var] + expressions]
    else
      [make_lambda(variables, body)] + expressions
    end
  end
  
  ## 4.1.3

  ### 述語のテスト
  
  def true?(x)
    !false?(x)
  end

  def false?(x)
    x.equal?(false)
  end

  def lookup_variable_value(exp, env)
    # @@@TODO 暫定
    case exp
    when :true
      true
    when :false
      false
    else
      exp
    end
  end
  
end
