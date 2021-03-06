library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity REGISTER_N is
generic (
  N           : integer := 16);
port (
  i_clk         : in   std_logic;
  i_reset_n     : in   std_logic;
  i_load_en     : in   std_logic;
  i_par_in      : in   std_logic_vector(N-1 downto 0);
  o_par_out     : out  std_logic_vector(N-1 downto 0));
end entity;

architecture BHV of REGISTER_N is

signal s_data : std_logic_vector(N-1 downto 0);

begin

  o_par_out <= s_data;

  piso: process(i_clk, i_reset_n, i_load_en)
  begin
    if i_reset_n = '0' then
      s_data <= (others => '0');
    elsif i_clk'event and i_clk = '1' then
      if i_load_en = '1' then
        s_data <= i_par_in;
      end if;
    end if;
  end process;

end architecture;
