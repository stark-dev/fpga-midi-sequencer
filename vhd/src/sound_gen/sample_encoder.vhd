library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SAMPLE_ENCODER is
port (
  i_enable    : in  std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);
  i_data      : in  t_table_idx;
  o_output    : out t_sample_idx
);
end entity;

architecture BHV of SAMPLE_ENCODER is
  signal s_idx : t_sample_idx;

begin

  p_output: process(s_idx)
  begin
    for i in 0 to MAX_POLYPHONY - 1 loop
      o_output(i) <= s_idx(i);
    end loop;
  end process;

  p_test: process(i_enable, i_data)
    variable idx : integer range 0 to MAX_POLYPHONY;
  begin
    s_idx <= (others => (others => '0'));
    idx := 0;

    for i in 0 to 2**SEQ_NOTE_SIZE - 1 loop
      if i_enable(i) = '1' then
        s_idx(idx) <= i_data(i);
        idx := idx + 1;
      end if;
    end loop;

  end process;

end architecture;
