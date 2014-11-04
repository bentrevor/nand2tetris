require 'spec_helper'

describe Assembler do
  let(:asm_code) { "(LOOP)\r\n        @symb\r\n        D=M\r\n        @100\r\n        D=A" }

  let(:assembler) { Assembler.new 'file' }

  before do
    allow(File).to receive(:new).and_return(double('file', :write => nil))
    allow(File).to receive(:write)
    allow(File).to receive(:read).and_return(asm_code)
  end

  it 'parses a .asm file and writes to a .hack file' do
    outfile = double('file', :write => nil)
    allow(File).to receive(:new).and_return(outfile)

    expect(assembler.parser.lines.length).to eq 5

    expect(outfile).to receive(:write)
    assembler.translate
  end

  context 'first pass' do
    it 'puts labels and their memory addresses into the symbol table before parsing' do
      expect(assembler.symbol_table.contains?('LOOP')).to be false

      assembler.add_labels_to_symbol_table

      expect(assembler.symbol_table.contains?('LOOP')).to be true
      expect(assembler.symbol_table.get_address('LOOP')).to eq 0
    end

    it 'resets the position' do
      expect(assembler.parser).to receive(:reset_position)
      assembler.add_labels_to_symbol_table
    end
  end

  context 'second pass' do
    describe 'translating A-instructions' do
      it 'translates a number symbol to binary' do
        allow(File).to receive(:read).and_return("        @100")

        assembler.translate

        expect(assembler.binary).to eq "0000000001100100\r\n"
      end

      it 'adds to the symbol table when the symbol is not defined yet' do
        allow(File).to receive(:read).and_return("        @yolo\r\n        @swag")

        expect(assembler.symbol_table.contains?('yolo')).to be false
        expect(assembler.symbol_table.contains?('swag')).to be false
        assembler.translate

        expect(assembler.symbol_table.contains?('yolo')).to be true
        expect(assembler.symbol_table.contains?('swag')).to be true
        expect(assembler.symbol_table.get_address('yolo')).to eq 16
        expect(assembler.symbol_table.get_address('swag')).to eq 17
      end

      it 'substitutes a value if a symbol is already defined' do
        allow(File).to receive(:read).and_return("        @yolo")
        assembler.symbol_table.add_entry('yolo', 16)

        assembler.translate

        expect(assembler.binary).to eq "0000000000010000\r\n"
      end
    end

    describe 'translating C-instructions' do
      before do
        allow(File).to receive(:read).and_return("        AMD=1;JMP")
        assembler.translate
      end

      it 'starts with 111' do
        expect(assembler.binary[0..2]).to eq '111'
      end

      it 'encodes the comp, dest, and jump' do
        expect(assembler.binary[3..9]).to eq '0111111'
        expect(assembler.binary[10..12]).to eq '111'
        expect(assembler.binary[13..15]).to eq '111'
      end
    end

    it 'translates nothing for L-instructions' do
      allow(File).to receive(:read).and_return("        (HEY)")

      assembler.translate

      expect(assembler.binary).to eq ''
    end
  end
end
