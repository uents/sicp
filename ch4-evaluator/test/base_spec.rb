load "./base.rb"

describe Kernel do
  before do
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
    expect(cons(1, nil)).to eq([1])
  end

  it "car" do
    expect(car(cons(1, 2))).to eq(1)
    expect(car(cons(2, 3))).to eq(2)
  end

  after do
  end
end
