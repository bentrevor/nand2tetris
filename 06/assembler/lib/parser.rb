class Parser
  attr_accessor :asm_code, :current_command

  def initialize(filename)
    whole_file = File.read(filename)
    @asm_code = whole_file.split("\r\n").select { |line| line.strip[0..1] != '//' && !line.empty? }.join("\r\n")
  end

  def has_more_commands?
    ! @asm_code.empty?
  end

  def advance
    @current_command = asm_code.split("\r\n").first.strip.split("//").first.strip
    @asm_code = asm_code.split("\r\n")[1..-1].join("\r\n")
  end

  def command_type
    case @current_command
    when /^@(.*)/
      :a
    when /\(.*\)/
      :l
    when nil
      nil
    else
      :c
    end
  end

  def symbol
    case command_type
    when :a
      @current_command[1..-1]
    when :l
      @current_command[1..-2]
    end
  end

  def dest
    if command_type == :c
      return unless @current_command.include?('=')

      @current_command.gsub(/=.*$/, '')
    end
  end

  def comp
    if command_type == :c
      @current_command.gsub(/^.*=/, '').gsub(/;.*$/, '')
    end
  end

  def jump
    if command_type == :c
      return unless @current_command.include?(';')

      @current_command.gsub(/^.*;/, '')
    end
  end
end
