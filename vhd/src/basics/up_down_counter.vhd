library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity UP_DOWN_COUNTER is
  generic (
    N           : integer := 16);
  port (
    i_clk       : in   std_logic;
    i_reset_n   : in   std_logic;
    i_en        : in   std_logic;
    i_tc_value  : in   unsigned(N-1 downto 0);
    i_up_down   : in   std_logic;  -- 0 up, 1 down
    o_q         : out  unsigned(N-1 downto 0);
    o_tc        : out  std_logic);
end entity;

architecture BHV of UP_DOWN_COUNTER is

  -- signals
  signal s_q_temp   : unsigned(n-1 downto 0);
  signal s_tc_temp  : std_logic;

begin

  count: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_q_temp <= to_unsigned(0, n);
    elsif i_clk'event and i_clk='1' then
      if s_tc_temp = '1' then
        s_q_temp <= to_unsigned(0, n);
      elsif i_en='1' then
        if i_up_down = '0' then
          s_q_temp <= s_q_temp + 1;
        else
          s_q_temp <= s_q_temp - 1;
        end if;
      end if;
    end if;
  end process;

  o_q <= s_q_temp;

  s_tc_temp <= '1' when s_q_temp = i_tc_value else
               '0';

  o_tc <= s_tc_temp;

end architecture;
