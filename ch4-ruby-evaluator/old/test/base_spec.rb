#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require_relative "../base"

describe Enumerable do

  before do
    @lst = [1,2,3]
  end

  it "foldr add" do
    expect(@lst.foldr(:+)).to eq(6)
    expect(@lst.foldr(4, :+)).to eq(10)
    expect(@lst.foldr(4) {|x, y| x + y }).to eq(10)

    # 可換則が成り立つ場合、foldrとinjectの結果は一致する
    expect(@lst.foldr(:+)).to eq(@lst.inject(:+))
  end

  it "foldr mul" do
    expect(@lst.foldr(1, :*)).to eq(6)
    expect(@lst.foldr(4, :*)).to eq(24)
    expect(@lst.foldr(4) {|x, y| x * y }).to eq(24)

    # 可換則が成り立つ場合、foldrとinjectの結果は一致する
    expect(@lst.foldr(:*)).to eq(@lst.inject(:*))
  end

  it "foldr sub" do
    expect(@lst.foldr(:-)).to eq(2)
    expect(@lst.foldr(4,:-)).to eq(-2)
    expect(@lst.foldr(4) {|x, y| x - y }).to eq(-2)
  end

  after do
  end
end

include Base

describe Kernel do
  before do
  end

  it "not" do
    expect(not(false)).to eq(true)
    expect(not(true)).to eq(false)
    expect(not(:we_have_no_bananas)).to eq(false)
  end

  it "eq?" do
    expect(eq?(:yes, :yes)).to eq(true)
    expect(eq?(:yes, :no)).to eq(false)
    v = cons(1, 2)
    expect(eq?(v, v)).to eq(true)
    u = cons(1, 2)
    expect(eq?(v, u)).to eq(false)
    expect(eq?("zzz", "zzz")).to eq(false)
  end

  it "number?" do
    expect(number?(1)).to eq(true)
    expect(number?("hello")).to eq(false)
  end

  it "string?" do
    expect(string?("Apple")).to eq(true)
    expect(string?(:apple)).to eq(false)
  end

  it "symbol?" do
    expect(symbol?(:Apple)).to eq(true)
    expect(symbol?(10)).to eq(false)
  end

  it "pair?" do
    expect(pair?(1)).to eq(false)
    expect(pair?(cons(1, 2))).to eq(true)
    expect(pair?(list(1, 2))).to eq(true)
    expect(pair?(nil)).to eq(false)
  end

  it "null?" do
    expect(null?(1)).to eq(false)
    expect(null?(cons(1, 2))).to eq(false)
    expect(null?(nil)).to eq(true)
    expect(null?(cdr(list(1)))).to eq(true)
  end

  it "cons" do
    expect(cons(1, 2)).to eq([1, 2])
    expect(cons(1, nil)).to eq([1, nil])
  end

  it "car" do
    expect(car(cons(1, 2))).to eq(1)
    expect(car(cons(2, 3))).to eq(2)
  end

  it "cdr" do
    expect(cdr(list(1, 2))).to eq([2, nil])
    expect(cdr(cons(1, nil))).to eq(nil)
  end

  it "list" do
    x = list(1, 2, 3, 4)
    y = cons(1, cons(2, cons(3, cons(4, nil))))
    expect(x).to eq(y)

    x = list(list(1, 2), list(3, 4))
    y = [[1, [2, nil]], [[3, [4, nil]], nil]]
    expect(x).to eq(y)    
  end

  it "caar" do
    x = caar(list(list(1, 2), 3, 4))
    y = 1
    expect(x).to eq(y)
  end

  it "cadr" do
    x = cadr(list(list(1, 2), 3, 4))
    y = 3
    expect(x).to eq(y)
  end

  it "cdar" do
    x = cdar(list(list(7, 6, 5, 4, 3, 2, 1), 8, 9))
    y = list(6, 5, 4, 3, 2, 1)
    expect(x).to eq(y)
  end

  it "cddr" do
    x = cddr(list(2, 1))
    y = nil
    expect(x).to eq(y)
  end

  it "caaar" do
    x = caaar(list(list(list(6, 5, 4, 3, 2, 1), 7), 8, 9))
    y = 6
    expect(x).to eq(y)
  end

  it "caadr" do
    x = caadr(list(9, list(7, 6, 5, 4, 3, 2, 1), 8))
    y = 7
    expect(x).to eq(y)
  end

  it "cadar" do
    x = cadar(list(list(7, 6, 5, 4, 3, 2, 1), 8, 9))
    y = 6
    expect(x).to eq(y)
  end

  it "caddr" do
    x = caddr(list(3, 2, 1))
    y = 1
    expect(x).to eq(y)
  end

  it "cdaar" do
    x = cdaar(list(list(list(6, 5, 4, 3, 2, 1), 7), 8, 9))
    y = list(5, 4, 3, 2, 1)
    expect(x).to eq(y)    
  end

  it "cdadr" do
    x = cdadr(list(9, list(7, 6, 5, 4, 3, 2, 1), 8))
    y = list(6, 5, 4, 3, 2, 1)
    expect(x).to eq(y)
  end

  it "cddar" do
    x = cddar(list(list(7, 6, 5, 4, 3, 2, 1), 8, 9))
    y = list(5, 4, 3, 2, 1)
    expect(x).to eq(y)    
  end

  it "cdddr" do
    x = cdddr(list(3, 2, 1))
    y = nil
    expect(x).to eq(y)
  end

  it "caaaar" do
    x = caaaar(list(list(list(list(5, 4, 3, 2, 1), 6), 7), 8, 9))
    y = 5
    expect(x).to eq(y)
  end

  it "caaadr" do
    x = caaadr(list(9, list(list(6, 5, 4, 3, 2, 1), 7), 8))
    y = 6
    expect(x).to eq(y)    
  end

  it "caadar" do
    x = caadar(list(list(7, list(5, 4, 3, 2, 1), 6), 8, 9))
    y = 5
    expect(x).to eq(y)
  end

  it "caaddr" do
    x = caaddr(list(9, 8, list(6, 5, 4, 3, 2, 1), 7))
    y = 6
    expect(x).to eq(y)
  end

  it "cadaar" do
    x = cadaar(list(list(list(6, 5, 4, 3, 2, 1), 7), 8, 9))
    y = 5
    expect(x).to eq(y)
  end

  it "cadadr" do
    x = cadadr(list(9, list(7, 6, 5, 4, 3, 2, 1), 8))
    y = 6
    expect(x).to eq(y)
  end

  it "caddar" do
    x = caddar(list(list(7, 6, 5, 4, 3, 2, 1), 8, 9))
    y = 5
    expect(x).to eq(y)
  end

  it "cadddr" do
    x = cadddr(list(4, 3, 2, 1))
    y = 1
    expect(x).to eq(y)    
  end

  it "cdaaar" do
    x = cdaaar(list(list(list(list(5, 4, 3, 2, 1), 6), 7), 8, 9))
    y = list(4, 3, 2, 1)
    expect(x).to eq(y)
  end

  it "cdaadr" do
    x = cdaadr(list(9, list(list(6, 5, 4, 3, 2, 1), 7), 8))
    y = list(5, 4, 3, 2, 1)
    expect(x).to eq(y)
  end

  it "cdaddr" do
    x = cdaddr(list(9, 8, list(6, 5, 4, 3, 2, 1), 7))
    y = list(5, 4, 3, 2, 1)
    expect(x).to eq(y)    
  end

  it "cddaar" do
    x = cddaar(list(list(list(6, 5, 4, 3, 2, 1), 7), 8, 9))
    y = list(4, 3, 2, 1)
    expect(x).to eq(y)
  end

  it "cdddar" do
    x = cdddar(list(list(7, 6, 5, 4, 3, 2, 1), 8, 9))
    y = list(4, 3, 2, 1)
    expect(x).to eq(y)    
  end
  
  it "cddddr" do
    x = cddddr(list(4, 3, 2, 1))
    y = nil
    expect(x).to eq(y)    
  end
  
  after do
  end
end
