library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SAMPLE_CLOCK is
generic (
  g_ext_clock     : integer := 50000000;
  g_sample_freq   : integer := 44100
);
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;

  i_clk_enable    : in  std_logic;

  o_sample_clk    : out std_logic
);
end entity;

architecture BHV of SAMPLE_CLOCK is

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  constant c_sample_clk_div   : integer := g_ext_clock / g_sample_freq;
  constant c_sample_half_clk  : integer := c_sample_clk_div / 2;
  constant c_sample_clk_cnt   : integer := up_int_log2(c_sample_half_clk);

--------------------------------------------------------------------------------
-- components
--------------------------------------------------------------------------------
component UP_COUNTER_MOD is
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
end component;

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- sample clock
  signal s_sample_clk       : std_logic;
  signal s_sample_clk_c     : unsigned(c_sample_clk_cnt - 1 downto 0);
  signal s_sample_clk_c_en  : std_logic;
  signal s_sample_clk_c_rst : std_logic;
  signal s_sample_clk_c_tc  : std_logic;
  signal s_sample_clk_c_end : unsigned(c_sample_clk_cnt - 1 downto 0);

begin
  -- output signal assignment
  o_sample_clk        <= s_sample_clk;

  -- internal signal assignment
  s_sample_clk_c_en   <= i_clk_enable;
  s_sample_clk_c_rst  <= i_reset_n;

  s_sample_clk_c_end  <= to_unsigned(c_sample_half_clk, c_sample_clk_cnt);

  -- component
  SAMPLE_CLK_CNT : UP_COUNTER_MOD
  generic map ( c_sample_clk_cnt )
  port map (
    i_clk       => i_clk,
    i_reset_n   => s_sample_clk_c_rst,
    i_en        => s_sample_clk_c_en,
    i_tc_value  => s_sample_clk_c_end,
    o_cnt_out   => s_sample_clk_c,
    o_chain_en  => s_sample_clk_c_tc
  );

  -- process
  p_sample_clk: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_sample_clk <= '0';
    elsif i_clk'event and i_clk = '1' then
      if s_sample_clk_c_tc = '1' then
        s_sample_clk <= not s_sample_clk;
      end if;
    end if;
  end process;

end architecture;
