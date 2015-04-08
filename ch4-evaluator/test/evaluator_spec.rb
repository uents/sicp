#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require_relative "../evaluator"

describe Evaluator do
  before do
    @e = Evaluator.new
  end

  it "number and string" do
    expect(@e.eval(1, {})).to eq(1)
    expect(@e.eval("foo", {})).to eq("foo")
  end

  it "varible" do
    # @@@TODO
  end

  it "quote" do
    expect(@e.eval([:quote, 1, 2, 3], {})).to eq([1, 2, 3])
  end

  it "assignment" do
    # @@@TODO
  end

  it "definition" do
    # @@@TODO
  end

  it "if expression" do
    exp = [:if, [:eq?, :foo, true],
           "foo",
           "bar"]

    predicate = @e.if_predicate(exp)
    consequent = @e.if_consequent(exp)
    alternative = @e.if_alternative(exp)
    
    expect(@e.if?(exp)).to eq(true)
    expect(predicate).to eq([:eq?, :foo, true])
    expect(consequent).to eq("foo")
    expect(alternative).to eq("bar")
    expect(@e.make_if(predicate, consequent, alternative)).to eq(exp)
  end

  it "cond_to_if" do
    exp = [:cond,
           [[:eq?, :foo, true], "foo"],
           [[:eq?, :bar, true], "bar"],
           [[:eq?, :baz, true], "baz"]]

    derived = [:if,
               [:eq?, :foo, true],
               [:begin, "foo"],
               [:if,
                [:eq?, :bar, true],
                [:begin, "bar"],
                [:if,
                 [:eq?, :baz, true],
                 [:begin, "baz"],
                 false]]]
    
    expect(@e.cond_to_if(exp)).to eq(derived)
    
    exp = [:cond,
           [[:eq?, :foo, true], "foo"],
           [[:eq?, :bar, true], "bar"],
           [[:eq?, :baz, true], "baz"],
           [:else, "otherwise"]]

    
    derived = [:if,
               [:eq?, :foo, true],
               [:begin, "foo"],
               [:if,
                [:eq?, :bar, true],
                [:begin, "bar"],
                [:if, [:eq?, :baz, true],
                 [:begin, "baz"],
                 [:begin, "otherwise"]]]]
    
    expect(@e.cond_to_if(exp)).to eq(derived)

  end

  after do
  end
end


