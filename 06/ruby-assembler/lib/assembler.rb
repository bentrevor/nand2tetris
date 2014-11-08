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
    @parser.reset_position

    translate_lines
    @outfile.write(@binary)
  end

  private

  def add_labels_to_symbol_table
    rom_addr = 0

    @parser.each do |command|
      if [:a, :c].include?(command.type)
        rom_addr += 1
      else
        @symbol_table.add_entry(command.symbol, rom_addr)
      end
    end
  end

  def translate_lines
    next_available_ram = 16

    @parser.each do |command|
      symbol = command.symbol
      binary_line = ''

      case command.type
      when :a
        if symbol.match(/^\d+$/)
          binary_line = dec_to_bin16(symbol)
        elsif @symbol_table.contains?(symbol)
          binary_line = dec_to_bin16(@symbol_table.get_address(symbol))
        else
          @symbol_table.add_entry(symbol, next_available_ram)
          binary_line = dec_to_bin16(next_available_ram)
          next_available_ram += 1
        end
      when :c
        binary_line = "111#{Code.comp(command.comp)}#{Code.dest(command.dest)}#{Code.jump(command.jump)}"
      end

      @binary << (binary_line + "\r\n") unless binary_line.empty?
    end
  end

  def dec_to_bin16(n)
    ("%016b" % n)
  end
end
