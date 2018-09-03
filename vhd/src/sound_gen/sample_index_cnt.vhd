library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SAMPLE_ID_COUNTER is
generic (
  g_size  : integer := 16
);
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;
  i_enable        : in  std_logic;
  i_tc_value      : in  std_logic_vector(g_size - 1 downto 0);
  i_sample_inc    : in  std_logic_vector(g_size - 1 downto 0);

  o_tc            : out std_logic;
  o_sample_id     : out std_logic_vector(g_size - 1 downto 0)
);
end entity;

architecture BHV of SAMPLE_ID_COUNTER is

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  signal s_count      : unsigned(g_size - 1 downto 0);
  signal s_tc         : std_logic;


begin
  -- output signal assignment
  o_tc        <= s_tc;
  o_sample_id <= std_logic_vector(s_count);

  -- process
  pcount: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_count <= (others => '0');
    elsif i_clk'event and i_clk='1' then
      if i_enable = '1' then
        if s_tc = '1' then
          s_count <= (others => '0');
        else
          s_count <= s_count + unsigned(i_sample_inc);
        end if;
      end if;
    end if;
  end process;

  s_tc <= '1' when s_count = unsigned(i_tc_value) else '0';

end architecture;
