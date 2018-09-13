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
      o_dac_out       : out std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
      o_clip          : out std_logic;

      -- rec memory
      i_mem_data      : in  std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);
      i_mem_ready     : in  std_logic;
      i_mem_error     : in  std_logic;
      o_mem_read_en   : out std_logic;
      o_mem_write_en  : out std_logic;
      o_address       : out std_logic_vector(MEMORY_SIZE - 1 downto 0);
      o_load_data     : out std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0)
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

  component REC_MEMORY is
  generic (
    g_mem_size      : integer := 8
  );
  port (
    i_clk           : in  std_logic;
    i_reset_n       : in  std_logic;
    i_enable        : in  std_logic;
    i_read_en       : in  std_logic;
    i_write_en      : in  std_logic;
    i_address       : in  std_logic_vector(g_mem_size - 1 downto 0);
    i_load_data     : in  std_logic_vector(31 downto 0);
    o_data          : out std_logic_vector(31 downto 0);
    o_mem_ready     : out std_logic;
    o_mem_error     : out std_logic
  );
  end component;

  -- common
  constant c_ext_clock    : integer := 50000000;
  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_clock_half_p : time := c_clock * 1 ns;
  -- uart constants
  constant c_baud_rate    : integer := 115200;
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

  signal s_dac_out  : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
  signal s_clip     : std_logic;

  -- uart tx
  signal s_uart_en      : std_logic;
  signal s_tx_load_en   : std_logic;
  signal s_tx_data      : std_logic_vector(c_databits-1 downto 0);
  signal s_tx_data_out  : std_logic;
  signal s_uart_tx_end  : std_logic;

  -- evt memory
  signal s_mem_enable     : std_logic;

  signal s_mem_ready      : std_logic;
  signal s_mem_data_out   : std_logic_vector(SEQ_EVENT_SIZE-1 downto 0);
  signal s_mem_data_in    : std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);

  signal s_mem_read       : std_logic;
  signal s_mem_write      : std_logic;
  signal s_mem_address    : std_logic_vector(MEMORY_SIZE - 1 downto 0);
  signal s_mem_error      : std_logic;



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
    o_dac_out       => s_dac_out,
    o_clip          => s_clip,
    i_mem_data      => s_mem_data_out,
    i_mem_ready     => s_mem_ready,
    i_mem_error     => s_mem_error,
    o_mem_read_en   => s_mem_read,
    o_mem_write_en  => s_mem_write,
    o_address       => s_mem_address,
    o_load_data     => s_mem_data_in
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

  EVT_MEM : REC_MEMORY
  generic map ( 10 )
  port map (
    i_clk           => s_clk,
    i_reset_n       => s_rst,
    i_enable        => s_mem_enable,
    i_read_en       => s_mem_read,
    i_write_en      => s_mem_write,
    i_address       => s_mem_address(11 downto 2), -- 4 byte align
    i_load_data     => s_mem_data_in,
    o_data          => s_mem_data_out,
    o_mem_ready     => s_mem_ready,
    o_mem_error     => s_mem_error
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
    s_tr_mute <= (others => '0');

    s_uart_en       <= '0';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00000000";

    s_mem_enable    <= '0';
    wait for 100 ns;

    --reset
    s_rst     <= '0';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');

    s_uart_en       <= '0';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00000000";

    s_mem_enable    <= '0';
    wait for 100 ns;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00000000";

    s_mem_enable    <= '1';
    wait for 2000 ns; -- allow load of samples

    -- polyphony test

    for i in 0 to MAX_POLYPHONY loop
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
      s_tx_data       <= std_logic_vector(to_unsigned(i, 8));
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= std_logic_vector(to_unsigned(i, 8));
      wait for 12*c_uart_period;

      -- note on : vel
      s_uart_en       <= '1';
      s_tx_load_en    <= '1';
      s_tx_data       <= "01000000";
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= "01000000";
      wait for 12*c_uart_period;

    end loop;

    for i in MAX_POLYPHONY downto 0 loop
      -- note on : status
      s_uart_en       <= '1';
      s_tx_load_en    <= '1';
      s_tx_data       <= "10000001";
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= "10000001";
      wait for 12*c_uart_period;

      -- note on : key
      s_uart_en       <= '1';
      s_tx_load_en    <= '1';
      s_tx_data       <= std_logic_vector(to_unsigned(i, 8));
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= std_logic_vector(to_unsigned(i, 8));
      wait for 12*c_uart_period;

      -- note on : vel
      s_uart_en       <= '1';
      s_tx_load_en    <= '1';
      s_tx_data       <= "01000000";
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= "01000000";
      wait for 12*c_uart_period;
    end loop;

    -- menu
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    wait for 320 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (patch)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (track vol)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (pan)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (poly)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    wait for 1000 ns;

    -- toggle poly
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    wait for 1000 ns;

    -- exit menu
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    wait for 1000 ns;

    for i in 0 to 3 loop
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
      s_tx_data       <= std_logic_vector(to_unsigned(i, 8));
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= std_logic_vector(to_unsigned(i, 8));
      wait for 12*c_uart_period;

      -- note on : vel
      s_uart_en       <= '1';
      s_tx_load_en    <= '1';
      s_tx_data       <= "01000000";
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= "01000000";
      wait for 12*c_uart_period;

    end loop;

    for i in 3 downto 0 loop
      -- note on : status
      s_uart_en       <= '1';
      s_tx_load_en    <= '1';
      s_tx_data       <= "10000001";
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= "10000001";
      wait for 12*c_uart_period;

      -- note on : key
      s_uart_en       <= '1';
      s_tx_load_en    <= '1';
      s_tx_data       <= std_logic_vector(to_unsigned(i, 8));
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= std_logic_vector(to_unsigned(i, 8));
      wait for 12*c_uart_period;

      -- note on : vel
      s_uart_en       <= '1';
      s_tx_load_en    <= '1';
      s_tx_data       <= "01000000";
      wait for 2*c_clock_half_p;

      s_uart_en       <= '1';
      s_tx_load_en    <= '0';
      s_tx_data       <= "01000000";
      wait for 12*c_uart_period;
    end loop;

    wait;

  end process;

end TEST;
