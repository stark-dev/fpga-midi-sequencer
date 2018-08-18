library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity PISO_REGISTER is
generic (
  N           : integer := 16);
port (
  i_clk         : in   std_logic;
  i_reset_n     : in   std_logic;
  i_par_load    : in   std_logic;
  i_shift_en    : in   std_logic;
  i_par_in      : in   std_logic_vector(N-1 downto 0);
  o_serial_out  : out  std_logic);
end entity;

architecture BHV of PISO_REGISTER is

signal s_data : std_logic_vector(N-1 downto 0);

begin

  o_serial_out <= s_data(0);

  piso: process(i_clk, i_reset_n, i_par_load, i_shift_en)
  begin
    if i_reset_n = '0' then
      s_data <= (others => '0');
    elsif i_clk'event and i_clk = '1' then
      if i_par_load = '1' then
        s_data <= i_par_in;
      elsif i_shift_en = '1' then
        s_data(N-2 downto 0) <= s_data(N-1 downto 1);
        s_data(N-1) <= '0';
      end if;
    end if;
  end process;

end architecture;
