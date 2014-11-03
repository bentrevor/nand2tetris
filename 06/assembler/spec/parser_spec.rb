require 'spec_helper'

describe Parser do
  let(:parser) { Parser.new('filename') }
  let(:a_command) { "        @symbol" }
  let(:c_command) { "        dest=comp;jump" }
  let(:l_command) { "        (SYMBOL)" }

  let(:binary_a_command) { '0000000000010000' } # user-defined variables start at 16

  before :each do
    allow(File).to receive(:read).and_return("#{a_command}\r\n#{c_command}\r\n#{l_command}")
  end

  it 'reads an asm file' do
    expect(File).to receive(:read).with('filename')

    p = Parser.new('filename')
  end

  it 'strips comments' do
    allow(File).to receive(:read).and_return("// bla\r\n@bla  // yo")
    parser.advance
    expect(parser.current_command).not_to eq '// bla'
    expect(parser.current_command).to eq '@bla'
  end

  it 'knows when it is done reading a file' do
    expect(parser.has_more_commands?).to be true
    parser.asm_code = ''
    expect(parser.has_more_commands?).to be false
  end

  it 'strips whitespace from the current command' do
    expect(parser.current_command).to be nil

    parser.advance
    expect(parser.current_command).not_to be nil
    expect(parser.current_command).to eq a_command.strip
  end

  it 'knows the current command type' do
    parser.advance
    expect(parser.command_type).to eq :a

    parser.advance
    expect(parser.command_type).to eq :c

    parser.advance
    expect(parser.command_type).to eq :l

    parser.current_command = nil
    expect(parser.command_type).to eq nil
  end

  it 'knows the current symbol' do
    parser.advance
    expect(parser.symbol).to eq 'symbol'

    parser.advance
    expect(parser.symbol).to be nil

    parser.advance
    expect(parser.symbol).to eq 'SYMBOL'
  end

  it 'knows the dest' do
    parser.advance
    expect(parser.dest).to be nil

    parser.advance
    expect(parser.dest).to eq 'dest'

    parser.advance
    expect(parser.dest).to be nil
  end

  it 'knows the comp' do
    parser.advance
    expect(parser.comp).to be nil

    parser.advance
    expect(parser.comp).to eq 'comp'

    parser.advance
    expect(parser.comp).to be nil
  end

  it 'knows the jump' do
    parser.advance
    expect(parser.jump).to be nil

    parser.advance
    expect(parser.jump).to eq 'jump'

    parser.advance
    expect(parser.jump).to be nil
  end

  it 'knows when there is no dest or jump' do
    allow(File).to receive(:read).and_return('justcomp')

    parser.advance
    expect(parser.dest).to be nil
    expect(parser.jump).to be nil
    expect(parser.comp).to eq 'justcomp'
  end
end
