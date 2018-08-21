library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TIMESTAMP_GEN is
generic (
  g_clock_div   : integer := 12207
);
port (
  -- system inputs
  i_clk         : in  std_logic;
  i_reset_n     : in  std_logic;
  -- play control
  i_play_stop_n : in  std_logic;
  i_restart     : in  std_logic;
  -- timestamp output
  o_ts_fraction : out std_logic_vector(ST_TSF_SIZE-1 downto 0);
  o_ts_seconds  : out std_logic_vector(ST_TSS_SIZE-1 downto 0)
  );
end entity;

architecture BHV of TIMESTAMP_GEN is

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  constant c_clk_half_cnt   : integer := g_clock_div / 2;

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- timestamp clock
  signal s_clk_c_e          : std_logic;
  signal s_clk_c_rst        : std_logic;
  signal s_clk_c_tc         : std_logic;
  signal s_clk_c_tc_val     : unsigned(up_int_log2(g_clock_div) - 1 downto 0);
  signal s_clk_c_out        : unsigned(up_int_log2(g_clock_div) - 1 downto 0);
  -- current timestamp seconds counter
  signal s_tss_c_e          : std_logic;
  signal s_tss_c_rst        : std_logic;
  signal s_tss_c_tc         : std_logic;
  signal s_tss_c_tc_val     : unsigned(ST_TSS_SIZE - 1 downto 0);
  signal s_tss_c_out        : unsigned(ST_TSS_SIZE - 1 downto 0);
  -- current timestamp fraction counter
  signal s_tsf_c_e          : std_logic;
  signal s_tsf_c_rst        : std_logic;
  signal s_tsf_c_tc         : std_logic;
  signal s_tsf_c_tc_val     : unsigned(ST_TSF_SIZE - 1 downto 0);
  signal s_tsf_c_out        : unsigned(ST_TSF_SIZE - 1 downto 0);

  -- timestamp clock
  signal s_ts_clock         : std_logic;

--------------------------------------------------------------------------------
-- components
--------------------------------------------------------------------------------
component REGISTER_N is
generic (
  N           : integer := 16);
port (
  i_clk         : in   std_logic;
  i_reset_n     : in   std_logic;
  i_load_en     : in   std_logic;
  i_par_in      : in   std_logic_vector(N-1 downto 0);
  o_par_out     : out  std_logic_vector(N-1 downto 0));
end component;

component UP_COUNTER is
  generic (
    N           : integer := 16);
  port (
    i_clk       : in   std_logic;
    i_reset_n   : in   std_logic;
    i_en        : in   std_logic;
    i_tc_value  : in   unsigned(N-1 downto 0);
    o_q         : out  unsigned(N-1 downto 0);
    o_tc        : out  std_logic);
end component;

begin

  -- components
  TSF_CLOCK_C : UP_COUNTER
  generic map (up_int_log2(g_clock_div))
  port map(
    i_clk       => i_clk,
    i_reset_n   => s_clk_c_rst,
    i_en        => s_clk_c_e,
    i_tc_value  => s_clk_c_tc_val,
    o_q         => s_clk_c_out,
    o_tc        => s_clk_c_tc
  );

  TSF_CURRENT_C : UP_COUNTER
  generic map (ST_TSF_SIZE)
  port map(
    i_clk       => i_clk,
    i_reset_n   => s_tsf_c_rst,
    i_en        => s_tsf_c_e,
    i_tc_value  => s_tsf_c_tc_val,
    o_q         => s_tsf_c_out,
    o_tc        => s_tsf_c_tc
  );

  TSS_CURRENT_C : UP_COUNTER
  generic map (ST_TSS_SIZE)
  port map(
    i_clk       => s_ts_clock,
    i_reset_n   => s_tss_c_rst,
    i_en        => s_tss_c_e,
    i_tc_value  => s_tss_c_tc_val,
    o_q         => s_tss_c_out,
    o_tc        => s_tss_c_tc
  );

  s_clk_c_e       <= i_play_stop_n;

  s_clk_c_rst     <= i_reset_n and not(i_restart) and i_play_stop_n;
  s_tsf_c_rst     <= i_reset_n and not(i_restart);
  s_tss_c_rst     <= i_reset_n and not(i_restart);

  s_clk_c_tc_val  <= to_unsigned(g_clock_div, up_int_log2(g_clock_div));
  s_tsf_c_tc_val  <= (others => '1');
  s_tss_c_tc_val  <= (others => '1');

  s_clk_c_e       <= i_play_stop_n;   -- when i_play_stop_n is active, run
  s_tsf_c_e       <= s_clk_c_tc;      -- when clk counter ends, increment frame
  s_tss_c_e       <= s_tsf_c_tc;      -- when frame counter ends, increment sec

  o_ts_fraction   <= std_logic_vector(s_tsf_c_out);
  o_ts_seconds    <= std_logic_vector(s_tss_c_out);

  -- process
  p_ts_clock: process(s_clk_c_out)
  begin
    if s_clk_c_out < to_unsigned(c_clk_half_cnt, up_int_log2(g_clock_div)) then
      s_ts_clock <= '0';
    else
      s_ts_clock <= '1';
    end if;
  end process;

end architecture;
