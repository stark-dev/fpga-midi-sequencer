library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_UART_TX is
end TB_UART_TX;

architecture TEST of TB_UART_TX is

  component UART_TX is
    generic (
      g_ext_clock   : natural := 500000000;
      g_baud_rate   : natural := 9600;
      g_databits    : natural := 8;
      g_parity      : boolean := false;
      g_parity_odd  : boolean := false;
      g_stop_bits   : integer := 1
    );
    port (
      i_uart_en     : in    std_logic;  -- UART enable
      i_uart_rst_n  : in    std_logic;  -- UART reset
      i_data_ld_en  : in    std_logic;  -- parallel data load enable
      i_data_in     : in    std_logic_vector(g_databits-1 downto 0);
      i_clk         : in    std_logic;  -- external clock
      o_uart_out    : out   std_logic;
      o_uart_end    : out   std_logic
    );
  end component;

  constant c_clock_period : time := 10 ns;
  constant c_uart_period  : time := 8681 ns;
  constant c_databits     : integer := 8;

  signal s_uart_en    : std_logic;
  signal s_uart_rst_n : std_logic;
  signal s_data_ld_en : std_logic;
  signal s_data_in    : std_logic_vector(c_databits-1 downto 0);
  signal s_clk        : std_logic;
  signal s_uart_out   : std_logic;
  signal s_uart_end   : std_logic;

begin

  DUT: UART_TX
    generic map (
      g_ext_clock  => 50000000,
      g_baud_rate  => 115200,
      g_databits   => c_databits,
      g_parity     => true,
      g_parity_odd => false,
      g_stop_bits  => 2
    )
    port map (
      i_uart_en     => s_uart_en,
      i_uart_rst_n  => s_uart_rst_n,
      i_data_ld_en  => s_data_ld_en,
      i_data_in     => s_data_in,
      i_clk         => s_clk,
      o_uart_out    => s_uart_out,
      o_uart_end    => s_uart_end
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
    s_uart_en    <= '0';
    s_uart_rst_n <= '1';
    s_data_ld_en <= '0';
    s_data_in    <= "00000000";
    wait for 1 ns;

    s_uart_en    <= '0';
    s_uart_rst_n <= '0';
    s_data_ld_en <= '0';
    s_data_in    <= "00000000";
    wait for 10 ns;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_data_ld_en <= '0';
    s_data_in    <= "00000000";
    wait for 9 ns;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_data_ld_en <= '1';
    s_data_in    <= "00000000";
    wait for 2*c_clock_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_data_ld_en <= '0';
    s_data_in    <= "00000000";
    wait for 15*c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_data_ld_en <= '1';
    s_data_in    <= "11010010";
    wait for 2*c_clock_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_data_ld_en <= '0';
    s_data_in    <= "11010010";
    wait for 15*c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_data_ld_en <= '1';
    s_data_in    <= "00101111";
    wait for 2*c_clock_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_data_ld_en <= '0';
    s_data_in    <= "00101111";

    wait;
  end process;

end TEST;
