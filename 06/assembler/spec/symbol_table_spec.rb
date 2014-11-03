require 'spec_helper'

describe SymbolTable do
  it 'wraps a hash' do
    st = SymbolTable.new

    st.add_entry('a', 1)

    expect(st.contains?('b')).to be false
    expect(st.contains?('a')).to be true
    expect(st.get_address('a')).to eq 1
  end
end
