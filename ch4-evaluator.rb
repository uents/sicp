#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

def _eval(exp, env)
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
    exp_if = cond_to_if(exp)
    eval_if(exp_if, env)
  elsif application?(exp)
    procedure = _eval(operator(exp) env)
    arguments = list_of_values(operands(exp) env)
    _apply(procedure, arguments)
  else
    raise "eval: unknown expression type: " + exp
  end
end

def _apply(procedure, arguments)
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

