library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_TLE is
end TB_TLE;

architecture TEST of TB_TLE is

  component TLE is
    port (
      -- common
      i_clk           : in  std_logic;
      i_reset_n       : in  std_logic;

      -- buttons
      i_btn_left      : in  std_logic;
      i_btn_up        : in  std_logic;
      i_btn_down      : in  std_logic;
      i_btn_right     : in  std_logic;

      -- switches
      i_tr_mute       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);

      -- serial in
      i_midi_in       : in  std_logic;

      -- display out
      o_display_a     : out t_display_if;

      -- DAC
      o_dac_out_l     : out std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
      o_dac_out_r     : out std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
      o_clip          : out std_logic;

      -- PWM out
      o_pwm           : out std_logic;
      o_pwm_n         : out std_logic
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

  -- common
  constant c_ext_clock    : integer := 50000000;
  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_clock_half_p : time := c_clock * 1 ns;
  -- uart constants
  constant c_baud_rate    : integer := MIDI_BAUD_RATE;
  constant c_uart         : integer := 1000000000/c_baud_rate;
  constant c_databits     : integer := 8;
  constant c_parity       : boolean := false;
  constant c_parity_odd   : boolean := false;
  constant c_stop_bits    : integer := 0;
  constant c_uart_period  : time := c_uart * 1 ns;

  -- common
  signal s_clk      : std_logic;
  signal s_rst      : std_logic;

  -- interface
  signal s_btn1     : std_logic;
  signal s_btn2     : std_logic;
  signal s_btn3     : std_logic;
  signal s_btn4     : std_logic;

  signal s_tr_mute  : std_logic_vector(SEQ_TRACKS - 1 downto 0);

  signal s_display  : t_display_if;

  signal s_dac_out_l    : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
  signal s_dac_out_r    : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
  signal s_clip         : std_logic;

  -- uart tx
  signal s_uart_en      : std_logic;
  signal s_tx_load_en   : std_logic;
  signal s_tx_data      : std_logic_vector(c_databits-1 downto 0);
  signal s_tx_data_out  : std_logic;
  signal s_uart_tx_end  : std_logic;

  -- pwm signals
  signal s_pwm          : std_logic;
  signal s_pwm_n        : std_logic;

begin

  DUT : TLE
  port map(
    i_clk           => s_clk,
    i_reset_n       => s_rst,
    i_btn_left      => s_btn1,
    i_btn_up        => s_btn2,
    i_btn_down      => s_btn3,
    i_btn_right     => s_btn4,
    i_tr_mute       => s_tr_mute,
    i_midi_in       => s_tx_data_out,
    o_display_a     => s_display,
    o_dac_out_l     => s_dac_out_l,
    o_dac_out_r     => s_dac_out_r,
    o_clip          => s_clip,
    o_pwm           => s_pwm,
    o_pwm_n         => s_pwm_n
  );

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
    i_uart_rst_n  => s_rst,
    i_data_ld_en  => s_tx_load_en,
    i_data_in     => s_tx_data,
    i_clk         => s_clk,
    o_uart_out    => s_tx_data_out,
    o_uart_end    => s_uart_tx_end
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
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');

    s_uart_en       <= '0';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00000000";

    wait for 100 ns;

    --reset
    s_rst     <= '0';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');

    s_uart_en       <= '0';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00000000";

    wait for 100 ns;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00000000";

    wait for 2000 ns; -- allow load of samples

    -- very short press (not detected)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- restart
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 320 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- menu
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 320 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- toggle menu (patch)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc patch
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- toggle menu (track vol)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- toggle menu (pan)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- toggle menu (poly)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- toggle poly
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- exit menu
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- toggle rec
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- start rec
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- start midi tx

    -- unknown command
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "00000000";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00000000";
    wait for 12*c_uart_period;

    -- note on : status
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "10010001";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "10010001";
    wait for 12*c_uart_period;

    -- note on : key
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "00100000";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00100000";
    wait for 12*c_uart_period;

    -- note on : vel
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "01111111";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "01111111";
    wait for 12*c_uart_period;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 100 ns;

    -- note off : status
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "10000001";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "10000001";
    wait for 12*c_uart_period;

    -- note off : key
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "00100000";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00100000";
    wait for 12*c_uart_period;

    -- note off : vel
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "01111111";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "01111111";
    wait for 12*c_uart_period;

    -- stop rec
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- menu
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 320 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc track
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc track
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- toggle menu (patch)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- toggle menu (track vol)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- inc vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec vol
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- exit menu
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- toggle rec
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- start rec
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- note on : status
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "10010001";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "10010001";
    wait for 12*c_uart_period;

    -- note on : key
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "00001100";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00001100";
    wait for 12*c_uart_period;

    -- note on : vel
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "00100101";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00100101";
    wait for 12*c_uart_period;

    -- note off : status
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "10000001";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "10000001";
    wait for 12*c_uart_period;

    -- note off : key
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "00001100";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00001100";
    wait for 12*c_uart_period;

    -- note off : vel
    s_uart_en       <= '1';
    s_tx_load_en    <= '1';
    s_tx_data       <= "01111111";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "01111111";
    wait for 12*c_uart_period;

    -- stop rec
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- menu
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '1');
    wait for 320 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec track
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- dec track
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- exit menu
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    -- play
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '1');
    wait for 1000 ns;

    wait;
  end process;

end TEST;
