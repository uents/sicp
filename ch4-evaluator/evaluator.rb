#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

load "base.rb"

class Evaluator
  include Base

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
    # ex 4.4
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

  # 手続きの引数
  def list_of_values(exps, env)
    if no_operands?(exps)
      nil
    else
      cons(eval(first_operand(exps), env),
           list_of_values(rest_operands(exps), env))
    end
  end

  # if文
  def eval_if(exp, env)
    if true?(eval(if_predicate(exp), env))
      eval(if_consequent(exp), env)
    else
      eval(if_alternative(exp), env)
    end
  end

  # 並び
  def eval_sequence(exps, env)
    if last_exp?(exps)
      eval(first_exp(exps), env)
    else
      eval(first_exp(exps), env)
      eval_sequence(rest_exps(exps), env)
    end
  end

  # 代入
  def eval_assignment(exp, env)
    var = assignment_variable(exp)
    value = eval(assignment_value(exp), env)
    set_variable_value!(var, value, env)
    :ok
  end

  # 定義
  def eval_definition(exp, env)
    var = definition_variable(exp)
    value = eval(definition_value(exp), env)
    define_variable!(var, value, env)
    :ok
  end

  # 数値/文字列
  def self_evaluating?(exp)
    if number?(exp)
      true
    elsif string?(exp)
      true
    else
      false
    end
  end

  # 変数
  def variable?(exp)
    symbol?(exp)
  end

  # タグ付きリストのチェック
  def tagged_list?(exp, tag)
    pair?(exp) && car(exp) == tag
  end

  # クオート
  def quoted?(exp)
    tagged_list?(exp, :quote)
  end

  def text_of_quotation(exp)
    cadr(exp)
  end

  # 代入
  def assignment?(exp)
    tagged_list?(exp, :set!)
  end  

  def assignment_variable(exp)
    cadr(exp)
  end

  def assignment_value(exp)
    caddr(exp)
  end

  # 定義
  def definition?(exp)
    tagged_list?(exp, :define)
  end

  def definition_variable(exp)
    if symbol?(cadr(exp))
      cadr(exp)
    else
      caddr(exp)
    end
  end

  def definition_value(exp)
    if symbol?(cadr(exp))
      caddr(exp)
    else
      params = cdadr(exp)
      body = cddr(exp)
      make_lambda(params, body)
    end
  end

  # lambda式
  def lambda?(exp)
    tagged_list?(exp, :lambda)
  end

  def lambda_parameters(exp)
    cadr(exp)
  end

  def lambda_body(exp)
    cddr(exp)
  end

  def make_lambda(params, body)
    cons(:lambda, cons(params, body))
  end

  # if式
  def if?(exp)
    tagged_list?(exp, :if)
  end

  def if_predicate(exp)
    cadr(exp)
  end

  def if_consequent(exp)
    caddr(exp)
  end

  def if_alternative(exp)
    unless null?(cdddr(exp))
      cadddr(exp)
    else
      :false
    end
  end

  def make_if(predicate, consequent, alternative)
    list(:if, predicate, consequent, alternative)
  end

  # begin式
  def begin?(exp)
    tagged_list?(exp, :begin)
  end

  def begin_actions(exp)
    cdr(exp)
  end

  def last_exp?(seq)
    null?(cdr(seq))
  end

  def first_exp(seq)
    car(seq)
  end

  def rest_exps(seq)
    cdr(seq)
  end

  def sequence_to_exp(seq)
    if null?(seq)
      seq
    elsif last_exp?(seq)
      first_exp(seq)
    else
      make_begin(seq)
    end
  end

  def make_begin(seq)
    cons(:begin, seq)
  end

  # 手続きの適用
  # 式(expression)のcarはオペレータ、cdrはオペランドのリスト
  def application?(exp)
    pair?(exp)
  end

  def operator(exp)
    car(exp)
  end

  def operands(exp)
    cdr(exp)
  end

  def no_operands?(ops)
    null?(ops)
  end

  def first_operand(ops)
    car(ops)
  end

  def rest_operands(ops)
    cdr(ops)
  end


  # 派生式
  # cond式はif式から派生できる

  def cond?(exp)
    tagged_list?(exp, :cond)
  end

  def cond_clauses(exp)
    cdr(exp)
  end

  def cond_else_clause?(clause)
    eq?(cond_predicate(clause), :else)
  end

  def cond_predicate(clause)
    car(clause)
  end

  def cond_actions(clause)
    cdr(clause)
  end

  def cond_to_if(exp)
    expand_clauses(cond_clauses(exp))
  end

  def expand_clauses(clauses)
    if null?(clauses)
      :false                # no else clause
    else
      first = car(clauses)
      rest = cdr(clauses)
      if cond_else_clause?(first)
        if null?(rest)
          sequence_to_exp(cond_actions(first))
        else
          raise "else clauses isn't last: cond_to_if " + clauses.to_s
        end

        predicate = cond_predicate(first)
        consequent = sequence_to_exp(cond_actions(first))
        alternative = expand_clauses(rest)
        make_if(predicate, consequent, alternative)
      end                              
    end
  end

  ### ex 4.3

  # and式
  def and?(exp)
    tagged_list?(exp, :and)
  end
  
  def eval_and(exp, env)
    def iter(exp, memo)
      if null?(exp)
        memo
      else
        if true?(car(exp))
          iter(cdr(exp), car(exp))
        else
          :false
        end
      end
    end
    iter(exp, :true)
  end
  
  
  ### stub

  def lookup_variable_value(var, env)
    var
  end
end
