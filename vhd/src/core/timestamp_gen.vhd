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

begin

  -- components
  CLOCK_C : UP_COUNTER_MOD
  generic map (up_int_log2(g_clock_div))
  port map(
    i_clk       => i_clk,
    i_reset_n   => s_clk_c_rst,
    i_en        => s_clk_c_e,
    i_tc_value  => s_clk_c_tc_val,
    o_cnt_out   => s_clk_c_out,
    o_chain_en  => s_clk_c_tc
  );

  TSF_COUNTER: UP_COUNTER_MOD
  generic map(ST_TSF_SIZE)
  port map(
    i_clk       => i_clk,
    i_reset_n   => s_tsf_c_rst,
    i_en        => s_tsf_c_e,
    i_tc_value  => s_tsf_c_tc_val,
    o_cnt_out   => s_tsf_c_out,
    o_chain_en  => s_tsf_c_tc
  );

  TSS_COUNTER: UP_COUNTER_MOD
  generic map(ST_TSS_SIZE)
  port map(
    i_clk       => i_clk,
    i_reset_n   => s_tss_c_rst,
    i_en        => s_tss_c_e,
    i_tc_value  => s_tss_c_tc_val,
    o_cnt_out   => s_tss_c_out,
    o_chain_en  => s_tss_c_tc
  );

  s_clk_c_e       <= i_play_stop_n;

  s_clk_c_rst     <= i_reset_n and not(i_restart) and i_play_stop_n;
  s_tsf_c_rst     <= i_reset_n and not(i_restart);
  s_tss_c_rst     <= i_reset_n and not(i_restart);

  s_clk_c_tc_val  <= to_unsigned(g_clock_div, up_int_log2(g_clock_div));
  -- TODO remove comments, just for test purposes
  s_tsf_c_tc_val  <= (others => '1');
  s_tss_c_tc_val  <= (others => '1');
  -- s_tsf_c_tc_val  <= to_unsigned(10, ST_TSF_SIZE);
  -- s_tss_c_tc_val  <= to_unsigned(100, ST_TSS_SIZE);

  -- when i_play_stop_n is active, run
  s_clk_c_e       <= i_play_stop_n;
  -- when clk counter ends, increment frame
  s_tsf_c_e       <= s_clk_c_tc;
  -- when frame counter ends, increment sec
  s_tss_c_e       <= s_tsf_c_tc;

  o_ts_fraction   <= std_logic_vector(s_tsf_c_out);
  o_ts_seconds    <= std_logic_vector(s_tss_c_out);

end architecture;
