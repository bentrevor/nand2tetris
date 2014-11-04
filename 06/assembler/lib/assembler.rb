class Assembler
  attr_accessor :parser, :outfile, :symbol_table, :binary

  def initialize(name)
    @parser = Parser.new("./#{name}.asm")
    @outfile = File.new("./#{name}.hack", 'w')
    @symbol_table = SymbolTable.new
    @binary = ''
  end

  def translate
    add_labels_to_symbol_table
    translate_lines
    @outfile.write(@binary)
  end

  def add_labels_to_symbol_table
    rom_addr = 0

    while @parser.has_more_commands?
      @parser.advance

      if [:a, :c].include?(@parser.command_type)
        rom_addr += 1
      else
        @symbol_table.add_entry(@parser.symbol, rom_addr)
      end
    end

    @parser.reset_position
  end

  def translate_lines
    next_available_ram = 16

    while @parser.has_more_commands?
      @parser.advance
      binary_line = ''

      case @parser.command_type
      when :a
        if @parser.symbol.match(/^\d+$/)
          binary_line = dec_to_bin16(@parser.symbol)
        elsif @symbol_table.contains?(@parser.symbol)
          binary_line = dec_to_bin16(@symbol_table.get_address(@parser.symbol))
        else
          @symbol_table.add_entry(@parser.symbol, next_available_ram)
          binary_line = dec_to_bin16(next_available_ram)
          next_available_ram += 1
        end
      when :l
        ""
      when :c
        c_code = Code.comp(@parser.comp)
        d_code = Code.dest(@parser.dest)
        j_code = Code.jump(@parser.jump)

        raise StandardError, "c_code is #{c_code}" if c_code.nil? || c_code.length != 7
        raise StandardError, "d_code is #{d_code}" if d_code.nil? || d_code.length != 3
        raise StandardError, "j_code is #{j_code}" if j_code.nil? || j_code.length != 3

        binary_line = "111#{c_code}#{d_code}#{j_code}"
      end

      @binary << (binary_line + "\r\n") unless binary_line.empty?
    end
  end

  def dec_to_bin16(n)
    ("%016b" % n)
  end
end
