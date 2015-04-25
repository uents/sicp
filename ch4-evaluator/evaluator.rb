#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

class Evaluator
  def initialize()
    @environment = Environment.new
  end

  def eval(object)
    object.eval(@environment)
  end
end

class Environment
  def initialize()

  end
end

