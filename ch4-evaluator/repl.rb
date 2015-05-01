# -*- coding: utf-8 -*-

load "parser.rb"
load "generator.rb"
load "evaluator.rb"

class REPLServer
  IN_PROMPT = '> '
  OUT_PROMPT = '=> '

  def initialize()
    @evaluator = Evaluator.new
  end

  def run()
    while true
      input = read_line()
      if input == "quit\n"
        return "good bye!!"
      end
      
      begin
        tokens = Parser.tokenize(input)
        nodes = Parser.parse(tokens)
#        pp "nodes : " + nodes.to_s
        object = Generator.generate(nodes)
#        pp "object : " + object.to_s

        output = @evaluator.eval(object)
        
      rescue Exception => e
        p e.to_s
        redo
      end

      pretty_print(output)
    end
  end

  def debug(*args)
    case args.length
    when 1
      symbol = args.first
      case symbol
      when :env
        @evaluator.environment
      else
        self.inspect
      end
    else
      self.inspect
    end
  end

  private
  def read_line()
    print IN_PROMPT
    input = gets or return
    while (count = input.count('(') - input.count(')')) > 0
      print "  " * (1 + count)
      next_input = gets or return
      input += next_input
    end

    input
  end

  def pretty_print(output)
    if output != nil
      print OUT_PROMPT
      print output.to_s + "\n"
    end
  end
end
