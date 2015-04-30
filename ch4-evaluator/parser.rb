#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

class Parser
  class Node
    attr_reader :key, :value
    
    def initialize(token)
      @key = token.keys[0]
      @value = token.values[0]
    end
  end
  
  def self.tokenize(input)
    tokens = input.strip
             .gsub(/\n/, ' ')
             .gsub('\'(', '(quote (')
             .gsub('(', '( ')
             .gsub(')', ' )')
             .split(' ')

    tokens.map do |token|
      case token
      when '('
        :LEFT_PAREN
      when ')'
        :RIGHT_PAREN
      when /^[+-]?[0-9]*[\.]?[0-9]+$/
        { :NUMBER => token }        
      when /\"/
        { :STRING => token }
      else
        { :SYMBOL => token }        
      end
    end
  end

  def self.parse(t)
    tokens = t.dup
    token = tokens.shift
    case token
    when :LEFT_PAREN
      return make_nodes(tokens)
    when :RIGHT_PAREN
      raise "parse: unexpected tokens; " + t.to_s
    else
      return Node.new(token)
    end
  end

  private
  def self.make_nodes(tokens)
    nodes = []
    token = tokens.shift

    while token != nil
      case token
      when :LEFT_PAREN
        nodes.push(make_nodes(tokens))
      when :RIGHT_PAREN
        return nodes
      else
        nodes.push(Node.new(token))
      end
      token = tokens.shift
    end
    nodes
  end
end

