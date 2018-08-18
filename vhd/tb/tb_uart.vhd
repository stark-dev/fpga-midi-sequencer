library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_UART is
end TB_UART;

architecture TEST of TB_UART is

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

  constant c_ext_clock    : integer := 50000000;
  constant c_baud_rate    : integer := 115200;
  constant c_databits     : integer := 8;
  constant c_parity       : boolean := true;
  constant c_parity_odd   : boolean := false;
  constant c_stop_bits    : integer := 1;

  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_uart         : integer := 1000000000/c_baud_rate;

  constant c_clock_half_p : time := c_clock * 1 ns;
  constant c_uart_period  : time := c_uart * 1 ns;

  -- uart common
  signal s_clk          : std_logic;
  signal s_uart_en      : std_logic;
  signal s_uart_rst_n   : std_logic;

  -- uart tx
  signal s_tx_load_en   : std_logic;
  signal s_tx_data      : std_logic_vector(c_databits-1 downto 0);
  signal s_tx_data_out  : std_logic;
  signal s_uart_tx_end  : std_logic;

  -- uart rx
  signal s_rx_data      : std_logic_vector(c_databits-1 downto 0);
  signal s_uart_rx_end  : std_logic;
  signal s_uart_rx_err  : std_logic;

  -- out buffer
  signal s_reg_data_out : std_logic_vector(c_databits-1 downto 0);

begin

  TX: UART_TX
  generic map (
    g_ext_clock  => c_ext_clock,
    g_baud_rate  => c_baud_rate,
    g_databits   => c_databits,
    g_parity     => c_parity,
    g_parity_odd => c_parity_odd,
    g_stop_bits  => c_stop_bits
  )
  port map (
    i_uart_en     => s_uart_en,
    i_uart_rst_n  => s_uart_rst_n,
    i_data_ld_en  => s_tx_load_en,
    i_data_in     => s_tx_data,
    i_clk         => s_clk,
    o_uart_out    => s_tx_data_out,
    o_uart_end    => s_uart_tx_end
  );

  RX: UART_RX
  generic map (
    g_ext_clock  => c_ext_clock,
    g_baud_rate  => c_baud_rate,
    g_databits   => c_databits,
    g_parity     => c_parity,
    g_parity_odd => c_parity_odd,
    g_stop_bits  => c_stop_bits
  )
  port map (
    i_uart_en     => s_uart_en,
    i_uart_rst_n  => s_uart_rst_n,
    i_uart_in     => s_tx_data_out,
    i_clk         => s_clk,
    o_uart_data   => s_rx_data,
    o_uart_end    => s_uart_rx_end,
    o_uart_err    => s_uart_rx_err
  );

  UART_BUF: REGISTER_N
  generic map (
    c_databits
  )
  port map (
    i_clk         => s_clk,
    i_reset_n     => s_uart_rst_n,
    i_load_en     => s_uart_rx_end,
    i_par_in      => s_rx_data,
    o_par_out     => s_reg_data_out
  );

  clock_gen : process
  begin
    s_clk <= '0';
    wait for c_clock_half_p;
    s_clk <= '1';
    wait for c_clock_half_p;
  end process;

  input_gen: process
  begin
    s_uart_en    <= '0';
    s_uart_rst_n <= '1';
    s_tx_load_en <= '0';
    s_tx_data    <= "00000000";
    wait for 1 ns;

    s_uart_en    <= '0';
    s_uart_rst_n <= '0';
    s_tx_load_en <= '0';
    s_tx_data    <= "00000000";
    wait for 10 ns;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_tx_load_en <= '0';
    s_tx_data    <= "00000000";
    wait for 9 ns;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_tx_load_en <= '1';
    s_tx_data    <= "00000000";
    wait for 2*c_clock_half_p;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_tx_load_en <= '0';
    s_tx_data    <= "00000000";
    wait for 15*c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_tx_load_en <= '1';
    s_tx_data    <= "11010010";
    wait for 2*c_clock_half_p;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_tx_load_en <= '0';
    s_tx_data    <= "11010010";
    wait for 15*c_uart_period;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_tx_load_en <= '1';
    s_tx_data    <= "00101111";
    wait for 2*c_clock_half_p;

    s_uart_en    <= '1';
    s_uart_rst_n <= '1';
    s_tx_load_en <= '0';
    s_tx_data    <= "00101111";

    wait;
  end process;

end TEST;
