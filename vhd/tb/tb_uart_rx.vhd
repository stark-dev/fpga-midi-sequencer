library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_UART_RX is
end TB_UART_RX;

architecture TEST of TB_UART_RX is

  component UART_RX is
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
      i_uart_in     : in    std_logic;  -- UART IN port
      i_clk         : in    std_logic;  -- external clock
      o_uart_data   : out   std_logic_vector(g_databits-1 downto 0);
      o_uart_end    : out   std_logic;
      o_uart_err    : out   std_logic
    );
  end component;

  constant c_clock_period : time := 10 ns;
  constant c_uart_period  : time := 8681 ns;
  constant c_databits     : integer := 8;

  signal s_uart_en    : std_logic;
  signal s_uart_rst_n : std_logic;
  signal s_uart_in    : std_logic;
  signal s_clk        : std_logic;
  signal s_uart_data  : std_logic_vector(c_databits-1 downto 0);
  signal s_uart_end   : std_logic;
  signal s_uart_err   : std_logic;

begin

  DUT: UART_RX
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
      i_uart_in     => s_uart_in,
      i_clk         => s_clk,
      o_uart_data   => s_uart_data,
      o_uart_end    => s_uart_end,
      o_uart_err    => s_uart_err
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
    s_uart_in    <= '1';
    wait for 1 ns;

    s_uart_en    <= '0';
    s_uart_rst_n <= '0';
    s_uart_in    <= '1';
    wait for 10 ns;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';
    wait for 9 ns;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  --start bit
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  -- data 0
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- data 1
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  -- data 2
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  -- data 3
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- data 4
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  -- data 5
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- data 6
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- data 7
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  -- parity (even parity)
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- stop 1
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- stop 2
    wait for c_uart_period;

    -- assert s_uart_data = "11010010" report "Error UART data 11010010";
    -- assert s_uart_err = '0' report "Error UART parity check 0";

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- idle
    wait for 3*c_uart_period;


    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  --start bit
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- data 0
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- data 1
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- data 2
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- data 3
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  -- data 4
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- data 5
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  -- data 6
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '0';  -- data 7
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- parity (even parity)
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- stop 1
    wait for c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- stop 2
    wait for c_uart_period;

    -- assert s_uart_data = "00101111" report "Error UART data 00101111";
    -- assert s_uart_err = '1' report "Error UART parity check 1";

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_uart_in    <= '1';  -- idle

    wait;
  end process;

end TEST;
