library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SAMPLE_ENCODER is
port (
  i_clk       : in  std_logic;
  i_reset_n   : in  std_logic;
  i_enable    : in  std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);
  i_data      : in  t_sound_table;
  o_enable    : out std_logic_vector(MAX_POLYPHONY - 1 downto 0);
  o_output    : out t_sound_gen_out
);
end entity;

architecture BHV of SAMPLE_ENCODER is
  signal s_en  : std_logic_vector(MAX_POLYPHONY - 1 downto 0);
  signal s_out : t_sound_gen_out;

begin

  o_enable <= s_en;
  o_output <= s_out;

  p_out:  process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      o_enable <= (others => '0');
      o_output <= (others => (others => '0'));
    elsif i_clk'event and i_clk = '1' then
      o_enable <= s_en;
      o_output <= s_out;
    end if;
  end process;

  p_data: process(i_enable, i_data)
    variable v_idx : integer range 0 to MAX_POLYPHONY;
  begin
    s_en  <= (others => '0');
    s_out <= (others => (others => '0'));

    v_idx := 0;

    for i in 0 to 2**SEQ_NOTE_SIZE - 1 loop
      if i_enable(i) = '1' then
        s_en(v_idx)  <= '1';
        s_out(v_idx) <= i_data(i);
        v_idx := v_idx + 1;
      end if;
    end loop;

  end process;

end architecture;
