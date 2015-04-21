#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

class Parser
  def self.tokenize(input)
    tokens = input.strip()
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
        { :NUMBER => numeric(token) }
      when /\"/
        { :STRING => token.gsub(/\"/, '') }
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
      raise "parse: unexpected tokens " + t.to_s
    else
      return token
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
        nodes.push(token)
      end
      token = tokens.shift
    end
    nodes
  end

  def self.numeric(str)
    begin
      return Integer(str)
    rescue
      begin
        return Float(str)
      rescue
        return str
      end
    end
  end
end

