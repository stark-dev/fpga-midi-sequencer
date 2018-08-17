library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity UART_RX is
  generic (
    g_ext_clock : natural := 500000000;
    g_baud_rate : natural := 9600;
    g_databits  : natural := 8;
    g_parity    : boolean := false;
    g_stop_bits : integer := 1
  );
  port (
    i_uart_in   : in    std_logic;  -- UART IN port
    i_clk       : in    std_logic;  -- external clock
    o_uart_data : out   std_logic_vector(g_databits-1 downto 0);
    o_uart_end  : out   std_logic
  );
end UART_RX;

architecture RTL of UART_RX is

  -- components
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

  -- constants
  constant c_clock_divider : natural := g_ext_clock / g_baud_rate;

  --signals
  signal s_cnt_en       : std_logic;
  signal s_cnt_rst      : std_logic;
  signal s_cnt_tc       : std_logic;
  signal s_cnt_out      : unsigned(up_int_log2(c_clock_divider)-1 downto 0);

  signal s_uart_clk     : std_logic;

begin

  s_cnt_en  <= '1';
  s_cnt_rst <= '1';

  uart_clock_cnt : UP_COUNTER
  generic map (
    up_int_log2(c_clock_divider)
  )
  port map (
    i_clk       => i_clk,
    i_reset_n   => s_cnt_rst,
    i_en        => s_cnt_en,
    i_tc_value  => to_unsigned(c_clock_divider, up_int_log2(c_clock_divider)),
    o_q         => s_cnt_out,
    o_tc        => s_cnt_tc
  );

  p_uart_clock: process(s_cnt_tc, s_cnt_rst)
  begin
    if s_cnt_rst = '0' then
      s_uart_clk <= '0';
    elsif(s_cnt_tc'event and s_cnt_tc = '1') then
      s_uart_clk <= not(s_uart_clk);
    end if;
  end process;

end architecture RTL;
