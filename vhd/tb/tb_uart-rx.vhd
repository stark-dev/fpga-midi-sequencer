library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_UART_RX is
end TB_UART_RX;

architecture TEST of TB_UART_RX is

  component UART_RX is
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
  end component;

  constant c_clock_period : time := 1 ns;

  signal s_uart_in    : std_logic;
  signal s_clk        : std_logic;
  signal s_uart_data  : std_logic_vector(7 downto 0);
  signal s_uart_end   : std_logic;

begin

  DUT: UART_RX
    generic map (
      g_ext_clock  => 50000000,
      g_baud_rate  => 9600,
      g_databits   => 8,
      g_parity     => false,
      g_stop_bits  => 1
    )
    port map (
      i_uart_in   => s_uart_in,
      i_clk       => s_clk,
      o_uart_data => s_uart_data,
      o_uart_end  => s_uart_end
    );

  clock_gen : process
  begin
    s_clk <= '0';
    wait for c_clock_period;
    s_clk <= '1';
    wait for c_clock_period;
  end process;

  input_gen: process
  begin
    s_uart_in <= '0';

    wait for 1 ns;
    -- assert t_data_out = "11001100101111101011110000100000" report "Error -99999999";

    wait;
  end process;

end TEST;
