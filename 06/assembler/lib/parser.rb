class Parser
  attr_accessor :current_command, :command_index, :lines

  class Command < Struct.new(:type, :symbol, :dest, :comp, :jump)
    def self.parse(line)
      return self.new(nil, nil, nil, nil, nil) if line.nil?

      type = symbol = dest = comp = jump = nil

      command = line.split('//').first.strip

      if command =~ /^@(.*)/
        type = :a
        symbol = command[1..-1]
      elsif command =~ /\(.*\)/
        type = :l
        symbol = command[1..-2]
      elsif !command.empty?
        type = :c
        dest = command.gsub(/=.*$/, '') if command.include?('=')
        comp = command.gsub(/^.*=/, '').gsub(/;.*$/, '')
        jump = command.gsub(/^.*;/, '') if command.include?(';')
      end

      self.new(type, symbol, dest, comp, jump)
    end
  end

  def initialize(filename)
    @command_index = 0
    @current_command = Command.parse(nil)
    @lines = File.read(filename).split("\r\n").select { |line| line.strip[0..1] != '//' && !line.empty? }
  end

  def has_more_commands?
    @command_index != @lines.length
  end

  def reset_position
    @command_index = 0
    @current_command = Command.parse(nil)
  end

  def advance
    @current_command = Command.parse(@lines[@command_index])
    @command_index += 1
  end

  def command_type
    @current_command.type
  end

  def symbol
    @current_command.symbol
  end

  def dest
    @current_command.dest
  end

  def comp
    @current_command.comp
  end

  def jump
    @current_command.jump
  end
end
