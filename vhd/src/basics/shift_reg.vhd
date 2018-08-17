library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity SHIFT_REGISTER is
generic (
  N           : integer := 16);
port (
  i_clk       : in   std_logic;
  i_reset_n   : in   std_logic;
  i_shift_en  : in   std_logic;
  i_serial_in : in   std_logic;
  o_par_out   : out  std_logic_vector(N-1 downto 0));
end entity;

architecture BHV of SHIFT_REGISTER is

signal s_data : std_logic_vector(N-1 downto 0);

begin

  o_par_out <= s_data;

  shift: process(i_clk, i_reset_n, i_shift_en)
  begin
    if i_reset_n = '0' then
      s_data <= (others => '0');
    elsif i_shift_en = '1' then
      if i_clk'event and i_clk = '1' then
        s_data(N-1 downto 1) <= s_data(N-2 downto 0);
        s_data(0) <= i_serial_in;
      end if;
    end if;
  end process;

end architecture;
