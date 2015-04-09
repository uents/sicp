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
    elsif begin?(exp)
      exps = begin_actions(exp)
      eval_sequence(exps, env)
    elsif cond?(exp)
      eval(cond_to_if(exp), env)
    #### ex 4.4
    elsif and?(exp)
      eval_and(exp, env)
    elsif or?(exp)
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
    exp.is_a?(Array) && exp.first == tag
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
    exp.rest.first
  end

  def assignment_value(exp)
    exp.rest.rest.first
  end

  #### 定義
  def definition?(exp)
    tagged_list?(exp, :define)
  end

  def definition_variable(exp)
    if symbol?(exp.rest.first) # 変数定義
      exp.rest.first
    else                       # 手続きを定義
      exp.rest.first.first
    end
  end

  def definition_value(exp)
    if symbol?(exp.first)
      exp.rest.first
    else
      params = exp.rest.first.rest
      body = exp.rest.rest.first
      make_lambda(params, body)
    end
  end

  #### lambda
  def lambda?(exp)
    tagged_list?(exp, :lambda)
  end

  def lambda_parameters(exp)
    exp.rest.first
  end

  def lambda_body(exp)
    exp.rest.rest.first
  end

  def make_lambda(params, body)
    [:lambda, params, body]
  end

  #### if
  def if?(exp)
    tagged_list?(exp, :if)
  end

  def if_predicate(exp)
    exp.rest.first
  end

  def if_consequent(exp)
    exp.rest.rest.first
  end

  def if_alternative(exp)
    if exp.rest.rest.rest.first
      exp.rest.rest.rest.first
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
    exps.first
  end

  def rest_exps(exps)
    exps.rest
  end

  #### begin
  def begin?(exp)
    tagged_list?(exp, :begin)
  end

  def begin_actions(exp)
    exp.rest
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
    exp.first
  end

  def operands(exp)
    exp.rest
  end

  def no_operands?(ops)
    ops.empty?
  end

  def first_operand(ops)
    ops.first
  end

  def rest_operands(exp)
    ops.rest
  end
  
  #### cond
  def cond?(exp)
    tagged_list?(exp, :cond)
  end

  def cond_clauses(exp)
    exp.rest
  end

  def cond_else_clause?(clause)
    cond_predicate(clause) == :else
  end

  def cond_predicate(clause)
    clause.first
  end

  def cond_actions(clause)
    clause.rest
  end

  def cond_to_if(exp)
    expand_clauses(cond_clauses(exp))
  end

  def expand_clauses(clauses)
    if clauses.empty?
      false                # no else clause
    else
      first = clauses.first
      rest = clauses.rest

      if cond_else_clause?(first)
        if rest.empty?
          sequence_to_exp(cond_actions(first))
        else
          raise "else clauses isn't last: cond_to_if " + clauses.to_s
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
  
  def eval_and(exps, env)
    if exps.rest.empty?
      eval(:true, env)
    else
      exps.rest.each do |exp|
        if false?(eval(exp, env))
          return eval(:false, env)
        end
      end
      exps.rest.last
    end
  end
    
  def or?(exp)
    tagged_list?(exp, :or)
  end

  def eval_or(exps, env)
    if exps.rest.empty?
      eval(:false, env)
    else
      exps.rest.each do |exp|
        unless false?(eval(exp, env))
          return eval(exp, env)
        end
      end
      eval(:false, env)
    end
  end

  
  ## 4.1.3

  ### 述語のテスト
  
  def true?(x)
    !false?(x)
  end

  def false?(x)
    x.equal?(:false)
  end

  def lookup_variable_value(exp, env)
    exp
  end
  
end
