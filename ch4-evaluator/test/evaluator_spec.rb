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
    exp = [:if, [:eq?, :foo, :true],
           "foo",
           "bar"]

    predicate = @e.if_predicate(exp)
    consequent = @e.if_consequent(exp)
    alternative = @e.if_alternative(exp)
    
    expect(@e.if?(exp)).to eq(true)
    expect(predicate).to eq([:eq?, :foo, :true])
    expect(consequent).to eq("foo")
    expect(alternative).to eq("bar")
    expect(@e.make_if(predicate, consequent, alternative)).to eq(exp)
  end

  it "cond_to_if" do
    exp = [:cond,
           [[:eq?, :foo, :true], "foo"],
           [[:eq?, :bar, :true], "bar"],
           [[:eq?, :baz, :true], "baz"]]

    derived = [:if,
               [:eq?, :foo, :true],
               [:begin, "foo"],
               [:if,
                [:eq?, :bar, :true],
                [:begin, "bar"],
                [:if,
                 [:eq?, :baz, :true],
                 [:begin, "baz"],
                 false]]]
    
    expect(@e.cond_to_if(exp)).to eq(derived)
    
    exp = [:cond,
           [[:eq?, :foo, :true], "foo"],
           [[:eq?, :bar, :true], "bar"],
           [[:eq?, :baz, :true], "baz"],
           [:else, "otherwise"]]

    
    derived = [:if,
               [:eq?, :foo, :true],
               [:begin, "foo"],
               [:if,
                [:eq?, :bar, :true],
                [:begin, "bar"],
                [:if, [:eq?, :baz, :true],
                 [:begin, "baz"],
                 [:begin, "otherwise"]]]]
    
    expect(@e.cond_to_if(exp)).to eq(derived)
  end

  it "eval_and" do
    env = {}

    exp = [:and]
    expect(@e.eval_and(exp, env)).to eq(true)

    exp = [:and, 1]
    expect(@e.eval_and(exp, env)).to eq(1)

    exp = [:and, :true, 5]
    expect(@e.eval_and(exp, env)).to eq(5)

    exp = [:and, :false]
    expect(@e.eval_and(exp, env)).to eq(false)
  end

  it "eval_or" do
    env = {}

    exp = [:or]
    expect(@e.eval_or(exp, env)).to eq(false)

    exp = [:or, 1]
    expect(@e.eval_or(exp, env)).to eq(1)

    exp = [:or, :false, 5]
    expect(@e.eval_or(exp, env)).to eq(5)

    exp = [:or, :false]
    expect(@e.eval_or(exp, env)).to eq(false)
  end

  it "let_to_combination" do
    exp = [:let,
           [[:var1, :exp1],
            [:var2, :exp2],
            [:var3, :exp3]],
           :body]

    derived = [[:lambda, [:var1, :var2, :var3],
                :body],
               :exp1, :exp2, :exp3]

    expect(@e.let_to_combination(exp)).to eq(derived)
  end

  it "lets_to_nested_let" do
    exp = [:lets,
           [[:var1, :exp1],
            [:var2, :exp2],
            [:var3, :exp3]],
           :body]

    derived = [:let, [[:var1, :exp1]],
               [:let, [[:var2, :exp2]],
                [:let, [[:var3, :exp3]],
                 :body]]]

    expect(@e.lets_to_nested_let(exp)).to eq(derived)
  end

  after do
  end
end


