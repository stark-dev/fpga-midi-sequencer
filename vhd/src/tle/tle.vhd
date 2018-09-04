library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TLE is
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
    i_tr_solo       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);

    -- serial in
    i_midi_in       : in  std_logic;

    -- display out
    o_display_a     : out t_display_if;

    -- rec memory
    i_mem_data      : in  std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);
    i_mem_ready     : in  std_logic;
    i_mem_error     : in  std_logic;
    o_mem_read_en   : out std_logic;
    o_mem_write_en  : out std_logic;
    o_address       : out std_logic_vector(MEMORY_SIZE - 1 downto 0);
    o_load_data     : out std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0)
  );
end TLE;

architecture STRUCT of TLE is

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
    i_pb_ready      : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);
    i_pb_end        : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);
    i_pb_data       : in  t_midi_data;

    -- external module ready
    i_pb_q_ready    : in  std_logic;

    -- outputs
    o_ts_seconds    : out std_logic_vector(ST_TSS_SIZE-1 downto 0);
    o_ts_fraction   : out std_logic_vector(ST_TSF_SIZE-1 downto 0);
    o_active_track  : out std_logic_vector(ST_TRACK_SIZE - 1 downto 0);
    o_rec_mode      : out std_logic;
    o_restart       : out std_logic;

    o_sound_on      : out std_logic;
    o_sg_patch      : out t_sg_patch;
    o_sg_note       : out t_sg_note;
    o_sg_vel        : out t_sg_vel;
    o_sg_start      : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
    o_sg_stop       : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
    o_sg_poly       : out std_logic_vector(SEQ_TRACKS - 1 downto 0);

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

  component SAMPLE_MANAGER is
  port (
    i_clk           : in  std_logic;
    i_reset_n       : in  std_logic;

    i_rec_mode      : in  std_logic;
    i_ts_seconds    : in  std_logic_vector(ST_TSS_SIZE-1 downto 0);
    i_ts_fraction   : in  std_logic_vector(ST_TSF_SIZE-1 downto 0);

    i_active_track  : in  std_logic_vector(ST_TRACK_SIZE - 1 downto 0);

    i_midi_ready    : in  std_logic;

    i_data_ready    : in  std_logic;
    i_mem_data      : in  std_logic_vector(SEQ_EVENT_SIZE-1 downto 0);
    i_mem_error     : in  std_logic;

    o_mem_read      : out std_logic;
    o_mem_write     : out std_logic;
    o_mem_address   : out std_logic_vector(MEMORY_SIZE - 1 downto 0);
    o_mem_wr_mux    : out t_mem_wr_mux;

    o_pb_ready      : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
    o_pb_end        : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
    o_pb_data       : out t_midi_data;

    o_init_ready    : out std_logic
  );
end component;

component SIMPLE_SOUND_GEN is
generic (
  g_sample_width  : integer := 8;
  g_smp_mem_size  : integer := 16
);
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;

  i_sound_on      : in  std_logic;
  i_patch         : in  std_logic_vector(TR_PATCH_SIZE - 1 downto 0);
  i_poly          : in  std_logic;

  i_sample_clk    : in  std_logic;

  i_start         : in  std_logic;
  i_stop          : in  std_logic;
  i_note          : in  std_logic_vector(SEQ_NOTE_SIZE - 1 downto 0);
  i_vel           : in  std_logic_vector(SEQ_VEL_SIZE - 1 downto 0);

  o_patch         : out std_logic_vector(TR_PATCH_SIZE - 1 downto 0);
  o_poly_cnt      : out std_logic_vector(MAX_POLY_BIT - 1 downto 0);
  o_sample_en     : out std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);
  o_sample_index  : out t_table_idx
);
end component;

component SAMPLE_CLOCK is
generic (
  g_ext_clock     : integer := 50000000;
  g_sample_freq   : integer := 44100
);
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;

  i_clk_enable    : in  std_logic;

  o_sample_clk    : out std_logic
);
end component;

component DISPLAY_IF is
port (
  i_display_value   : in  t_display_out;
  o_display_array   : out std_logic_vector(6 downto 0)
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

  -- display
  signal s_display      : t_display_array;

  -- timestamp gen
  signal s_ts_seconds   : std_logic_vector(ST_TSS_SIZE-1 downto 0);
  signal s_ts_fraction  : std_logic_vector(ST_TSF_SIZE-1 downto 0);
  signal s_active_track : std_logic_vector(ST_TRACK_SIZE - 1 downto 0);
  signal s_rec_mode     : std_logic;
  signal s_restart      : std_logic;

  -- uart rx
  signal s_rx_data      : std_logic_vector(c_databits-1 downto 0);
  signal s_uart_rx_end  : std_logic;
  signal s_uart_rx_err  : std_logic;

  -- sound gen
  signal s_midi_ready   : std_logic;
  signal s_midi_data    : std_logic_vector(SEQ_EVENT_SIZE - 1  downto 0);
  signal s_pb_ready     : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_pb_end       : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_pb_data      : t_midi_data;
  signal s_sg_note      : t_sg_note;
  signal s_sg_vel       : t_sg_vel;
  signal s_sg_start     : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_sg_stop      : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_sg_poly      : std_logic_vector(SEQ_TRACKS - 1 downto 0);

  signal s_sound_on     : std_logic;
  signal s_sample_clk   : std_logic;
  signal s_sg_patch     : t_sg_patch;
  signal s_sg_patch_2   : t_sg_patch; -- TODO change
  signal s_sg_poly_cnt  : t_poly_cnt;
  signal s_sample_index : t_table_idx;

  -- ext module ready
  signal s_pb_q_ready   : std_logic;

  -- playback queue and memory
  signal s_data_reload    : std_logic;
  signal s_mem_error      : std_logic;
  signal s_mem_data       : std_logic_vector(SEQ_EVENT_SIZE-1 downto 0);

  signal s_mem_wr_mux     : t_mem_wr_mux;
  signal s_mem_wr_mux_in  : std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);

begin
  o_load_data     <= s_mem_wr_mux_in;

  s_data_reload   <= i_reset_n and not(s_restart);

  CORE : SEQUENCER_CORE
  port map(
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,
    i_btn_left      => i_btn_left,
    i_btn_up        => i_btn_up,
    i_btn_down      => i_btn_down,
    i_btn_right     => i_btn_right,
    i_tr_mute       => i_tr_mute,
    i_tr_solo       => i_tr_solo,
    i_midi_ready    => s_midi_ready,
    i_midi_data     => s_midi_data,
    i_pb_ready      => s_pb_ready,
    i_pb_end        => s_pb_end,
    i_pb_data       => s_pb_data,
    i_pb_q_ready    => s_pb_q_ready,
    o_ts_seconds    => s_ts_seconds,
    o_ts_fraction   => s_ts_fraction,
    o_active_track  => s_active_track,
    o_rec_mode      => s_rec_mode,
    o_restart       => s_restart,
    o_sound_on      => s_sound_on,
    o_sg_patch      => s_sg_patch,
    o_sg_note       => s_sg_note,
    o_sg_vel        => s_sg_vel,
    o_sg_start      => s_sg_start,
    o_sg_stop       => s_sg_stop,
    o_sg_poly       => s_sg_poly,
    o_display_a     => s_display
  );

  EVT: MIDI_EVT_FILTER
  port map(
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_new_data    => s_uart_rx_end,
    i_data_in     => s_rx_data,
    o_midi_msg    => s_midi_data,
    o_midi_ready  => s_midi_ready
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
    i_uart_en     => '1', -- TODO fix
    i_uart_rst_n  => i_reset_n,
    i_uart_in     => i_midi_in,
    i_clk         => i_clk,
    o_uart_data   => s_rx_data,
    o_uart_end    => s_uart_rx_end,
    o_uart_err    => s_uart_rx_err
  );

  SAMPLE_MNG : SAMPLE_MANAGER
  port map (
    i_clk           => i_clk,
    i_reset_n       => s_data_reload,

    i_rec_mode      => s_rec_mode,
    i_ts_seconds    => s_ts_seconds,
    i_ts_fraction   => s_ts_fraction,

    i_active_track  => s_active_track,

    i_midi_ready    => s_midi_ready,

    i_data_ready    => i_mem_ready,
    i_mem_data      => i_mem_data,

    o_mem_read      => o_mem_read_en,
    o_mem_write     => o_mem_write_en,
    o_mem_address   => o_address,
    o_mem_wr_mux    => s_mem_wr_mux,
    i_mem_error     => i_mem_error,

    o_pb_ready      => s_pb_ready,
    o_pb_end        => s_pb_end,
    o_pb_data       => s_pb_data,

    o_init_ready    => s_pb_q_ready
  );

  SOUND_GEN_GENERATE:
  for i in 0 to SEQ_TRACKS - 1 generate
    SOUND_GEN_X : SIMPLE_SOUND_GEN
    generic map (
      g_sample_width  => SAMPLE_WIDTH,
      g_smp_mem_size  => SMP_MEM_SIZE
    )
    port map (
      i_clk           => i_clk,
      i_reset_n       => i_reset_n,

      i_sound_on      => s_sound_on,
      i_patch         => s_sg_patch(i),
      i_poly          => s_sg_poly(i),

      i_sample_clk    => s_sample_clk,

      i_start         => s_sg_start(i),
      i_stop          => s_sg_stop(i),
      i_note          => s_sg_note(i),
      i_vel           => s_sg_vel(i),

      o_patch         => s_sg_patch_2(i),
      o_poly_cnt      => s_sg_poly_cnt(i),
      o_sample_index  => s_sample_index
    );
  end generate;

  SAMPLE_CLK : SAMPLE_CLOCK
  generic map (
    g_ext_clock     => 50000000,
    g_sample_freq   => 44100
  )
  port map (
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,

    i_clk_enable    => s_sound_on,

    o_sample_clk    => s_sample_clk
  );

  DISPLAY_IF_GEN:
  for i in 0 to DISPLAY_SIZE - 1 generate
    DISPLAY_IF_X : DISPLAY_IF
      port map(s_display(i), o_display_a(i));
  end generate;


  p_mem_wr_mux: process(s_mem_wr_mux, s_midi_data, s_ts_fraction, s_ts_seconds)
  begin
    case s_mem_wr_mux is
      when mux_off  =>
        s_mem_wr_mux_in <= (others => '0');
      when mux_midi =>
        s_mem_wr_mux_in <= s_midi_data;
      when mux_ts   =>
        s_mem_wr_mux_in(ST_TS_RESERV) <= (others => '0');
        s_mem_wr_mux_in(ST_TSS_RANGE) <= s_ts_seconds;
        s_mem_wr_mux_in(ST_TSF_RANGE) <= s_ts_fraction;
      when others   =>
        s_mem_wr_mux_in <= (others => '0');
    end case;
  end process;

end STRUCT;
