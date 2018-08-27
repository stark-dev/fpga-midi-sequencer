library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_CORE is
end TB_CORE;

architecture TEST of TB_CORE is

  component SEQUENCER_CORE is
  port (
    -- system inputs
    i_clk           : in  std_logic;
    i_reset_n       : in  std_logic;

    i_btn_left      : in  std_logic;
    i_btn_up        : in  std_logic;
    i_btn_down      : in  std_logic;
    i_btn_right     : in  std_logic;

    i_tr_mute       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);
    i_tr_solo       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);

    -- direct midi events
    i_midi_ready    : in  std_logic;
    i_midi_data     : in  std_logic_vector(SEQ_EVENT_SIZE - 1  downto 0);

    -- playback events
    i_pb_ready      : in  t_midi_ready;
    i_pb_data       : in  t_midi_data;

    -- outputs
    o_ts_seconds    : out std_logic_vector(ST_TSS_SIZE-1 downto 0);
    o_ts_fraction   : out std_logic_vector(ST_TSF_SIZE-1 downto 0);

    o_sound_on      : out std_logic;
    o_sg_note       : out t_sg_note;
    o_sg_vel        : out t_sg_vel;
    o_sg_start      : out t_sg_start;
    o_sg_stop       : out t_sg_stop;

    o_display_a     : out t_display_array
  );
  end component;

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

  component MIDI_EVT_FILTER is
  port (
    i_clk         : in  std_logic;
    i_reset_n     : in  std_logic;
    i_new_data    : in  std_logic;
    i_data_in     : in  std_logic_vector(7 downto 0);

    o_midi_msg    : out std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);
    o_midi_ready  : out std_logic
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
  signal s_tr_solo  : std_logic_vector(SEQ_TRACKS - 1 downto 0);

  signal s_display  : t_display_array;

  signal s_ts_seconds   : std_logic_vector(ST_TSS_SIZE-1 downto 0);
  signal s_ts_fraction  : std_logic_vector(ST_TSF_SIZE-1 downto 0);

  -- uart tx
  signal s_uart_en      : std_logic;
  signal s_tx_load_en   : std_logic;
  signal s_tx_data      : std_logic_vector(c_databits-1 downto 0);
  signal s_tx_data_out  : std_logic;
  signal s_uart_tx_end  : std_logic;

  -- uart rx
  signal s_rx_data      : std_logic_vector(c_databits-1 downto 0);
  signal s_uart_rx_end  : std_logic;
  signal s_uart_rx_err  : std_logic;

  -- midi evt filter
  signal s_evt_out      : std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);
  signal s_evt_ready    : std_logic;

  -- sound gen
  signal s_midi_ready   : std_logic;
  signal s_midi_data    : std_logic_vector(SEQ_EVENT_SIZE - 1  downto 0);
  signal s_pb_ready     : t_midi_ready;
  signal s_pb_data      : t_midi_data;
  signal s_sg_note      : t_sg_note;
  signal s_sg_vel       : t_sg_vel;
  signal s_sg_start     : t_sg_start;
  signal s_sg_stop      : t_sg_stop;

  signal s_sound_on     : std_logic;

begin
  s_midi_ready    <= s_evt_ready;
  s_midi_data     <= s_evt_out;


  DUT : SEQUENCER_CORE
  port map(
    i_clk         => s_clk,
    i_reset_n     => s_rst,
    i_btn_left    => s_btn1,
    i_btn_up      => s_btn2,
    i_btn_down    => s_btn3,
    i_btn_right   => s_btn4,
    i_tr_mute     => s_tr_mute,
    i_tr_solo     => s_tr_solo,
    i_midi_ready  => s_midi_ready,
    i_midi_data   => s_midi_data,
    i_pb_ready    => s_pb_ready,
    i_pb_data     => s_pb_data,
    o_ts_seconds  => s_ts_seconds,
    o_ts_fraction => s_ts_fraction,
    o_sound_on    => s_sound_on,
    o_sg_note     => s_sg_note,
    o_sg_vel      => s_sg_vel,
    o_sg_start    => s_sg_start,
    o_sg_stop     => s_sg_stop,
    o_display_a   => s_display
  );

  EVT: MIDI_EVT_FILTER
  port map(
    i_clk         => s_clk,
    i_reset_n     => s_rst,
    i_new_data    => s_uart_rx_end,
    i_data_in     => s_rx_data,
    o_midi_msg    => s_evt_out,
    o_midi_ready  => s_evt_ready
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
    i_uart_rst_n  => s_rst,
    i_uart_in     => s_tx_data_out,
    i_clk         => s_clk,
    o_uart_data   => s_rx_data,
    o_uart_end    => s_uart_rx_end,
    o_uart_err    => s_uart_rx_err
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
    s_pb_ready(0)   <= '0';
    s_pb_data(0)    <= (others => '0');
    s_pb_ready(1)   <= '0';
    s_pb_data(1)    <= (others => '0');
    s_pb_ready(2)   <= '0';
    s_pb_data(2)    <= (others => '0');
    s_pb_ready(3)   <= '0';
    s_pb_data(3)    <= (others => '0');
    s_pb_ready(4)   <= '0';
    s_pb_data(4)    <= (others => '0');
    s_pb_ready(5)   <= '0';
    s_pb_data(5)    <= (others => '0');
    s_pb_ready(6)   <= '0';
    s_pb_data(6)    <= (others => '0');
    s_pb_ready(7)   <= '0';
    s_pb_data(7)    <= (others => '0');

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');

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
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');

    s_uart_en       <= '0';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00000000";
    wait for 100 ns;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "00000000";
    wait for 100 ns;

    -- very short press (not detected)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- play
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- pause
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 500 ns;

    -- play
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- pause
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- restart
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 320 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- menu
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 320 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (patch)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- inc patch
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (track vol)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (pan)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (poly)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle poly
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- exit menu
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- play
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
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
    s_tx_data       <= "01000000";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "01000000";
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
    s_tx_data       <= "01000000";
    wait for 2*c_clock_half_p;

    s_uart_en       <= '1';
    s_tx_load_en    <= '0';
    s_tx_data       <= "01000000";
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

    wait;
  end process;

end TEST;
