library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity UP_COUNTER_MOD is
  generic (
    N           : integer := 16
  );
  port (
    i_clk       : in   std_logic;
    i_reset_n   : in   std_logic;
    i_en        : in   std_logic;
    i_tc_value  : in   unsigned(N-1 downto 0);
    o_cnt_out   : out  unsigned(N-1 downto 0);
    o_chain_en  : out  std_logic
  );
end entity;

architecture BHV of UP_COUNTER_MOD is

  -- signals
  signal s_cnt_out  : unsigned(n-1 downto 0);
  signal s_tc       : std_logic;
  signal s_chain_en : std_logic;

begin

  count: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_cnt_out <= (others => '0');
    elsif i_clk'event and i_clk='1' then
      if i_en = '1' then
        if s_tc = '1' then
          s_cnt_out <= (others => '0');
        else
          s_cnt_out <= s_cnt_out + 1;
        end if;
      end if;
    end if;
  end process;

  o_cnt_out   <= s_cnt_out;
  o_chain_en  <= s_chain_en;

  s_tc <= '1' when s_cnt_out = i_tc_value else '0';
  s_chain_en <= '1' when (i_en = '1' and s_tc = '1') else '0';

end architecture;
