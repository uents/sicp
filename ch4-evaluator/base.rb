# coding: utf-8

## Extend modules and classes

module Enumerable
  def foldr(*args, &block)
    case args.length
    when 2
      init, proc = args
    when 1
      if block_given?
        init = args.first
      else
        init = :nil
        proc = args.first
      end
    when 0
      init = :nil
    else
      raise "..."
    end

    lst = self.dup
    if init == :nil
      init = self.last
      lst.pop
    end

#    pp "lst:" + lst.to_s + " init:" + init.to_s + " proc:" + proc.to_s + " block:" + block.to_s

    memo = init
    lst.reverse_each do |item|
      if proc
        memo = item.send(proc, memo)
      else
        memo = block.call(item, memo)
      end
    end      
    memo
  end
end
