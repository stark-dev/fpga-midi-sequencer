-- MIDI sequencer - Top Level Entity
-- Created by Mauro Guerrera <stark.devel@gmail.com>
-- Target FPGA is Altera DE1-SoC 5CSEMA5F31C6N

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

    -- serial in
    i_midi_in       : in  std_logic;

    -- display out
    o_display_a     : out t_display_if;

    -- DAC
    o_dac_out       : out std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
    o_clip          : out std_logic
  );
end TLE;

architecture STRUCT of TLE is

  component SEQUENCER_CORE is
  port (
    -- system inputs
    i_clk           : in  std_logic;
    i_reset_n       : in  std_logic;

    i_btn_l_l       : in  std_logic;
    i_btn_l_s       : in  std_logic;
    i_btn_u_l       : in  std_logic;
    i_btn_u_s       : in  std_logic;
    i_btn_d_l       : in  std_logic;
    i_btn_d_s       : in  std_logic;
    i_btn_r_l       : in  std_logic;
    i_btn_r_s       : in  std_logic;

    i_tr_mute       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);

    -- timestamp
    i_ts_seconds    : in  std_logic_vector(ST_TSS_SIZE-1 downto 0);
    i_ts_fraction   : in  std_logic_vector(ST_TSF_SIZE-1 downto 0);

    -- direct midi events
    i_midi_ready    : in  std_logic;
    i_midi_data     : in  std_logic_vector(SEQ_EVENT_SIZE - 1  downto 0);

    -- rec memory
    i_rec_data      : in  std_logic_vector(SEQ_EVENT_SIZE-1 downto 0);
    o_rec_mem_add   : out std_logic_vector(MEMORY_SIZE - 1 downto 0);
    o_rec_mem_wr    : out std_logic;
    o_rec_mem_mux   : out t_mem_wr_mux;

    -- outputs
    o_sound_on      : out std_logic;
    o_main_vol      : out natural range 0 to MAIN_VOL_MAX;
    o_restart       : out std_logic;
    o_play_pause_n  : out std_logic;
    o_sg_patch      : out t_sg_patch;
    o_sg_vol        : out t_sg_vol;
    o_sg_note       : out t_sg_note;
    o_sg_vel        : out t_sg_vel;
    o_sg_start      : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
    o_sg_stop       : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
    o_sg_poly       : out std_logic_vector(SEQ_TRACKS - 1 downto 0);

    o_display_a     : out t_display_array
  );
  end component;

  component TIMESTAMP_GEN is
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
  end component;

  component SL_BUTTON is
  generic (
    g_size      : integer := 12;
    g_short     : integer := 500;
    g_long      : integer := 1000
  );
  port (
    i_clk         : in  std_logic;
    i_reset_n     : in  std_logic;
    i_btn         : in  std_logic;
    o_long        : out std_logic;
    o_short       : out std_logic
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

component SOUND_SYNTH is
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;

  i_sound_on      : in  std_logic;
  i_patch         : in  t_sg_patch;
  i_vol           : in  t_sg_vol;

  i_main_vol      : in  natural range 0 to MAIN_VOL_MAX;

  i_sample_clk    : in  std_logic;        -- clock @ sample freq speed
  i_sample_en     : in  t_sample_enable;  -- array of enable signals
  i_sample_idx    : in  t_sample_idx;     -- array of sample indexes

  o_mem_patch     : out std_logic_vector(TR_PATCH_SIZE - 1 downto 0);
  o_mem_address   : out std_logic_vector(SMP_MEM_SIZE - 1 downto 0);
  i_mem_sample    : in  std_logic_vector(SAMPLE_WIDTH - 1 downto 0);  -- sample from memory

  o_clip          : out std_logic;  -- clip indicator
  o_sample_out    : out std_logic_vector(SAMPLE_WIDTH - 1 downto 0)
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
  i_poly          : in  std_logic;

  i_sample_clk    : in  std_logic;

  i_start         : in  std_logic;
  i_stop          : in  std_logic;
  i_note          : in  std_logic_vector(SEQ_NOTE_SIZE - 1 downto 0);
  i_vel           : in  std_logic_vector(SEQ_VEL_SIZE - 1 downto 0);

  o_sample_en     : out std_logic_vector(MAX_POLYPHONY - 1 downto 0);
  o_sample_idx    : out t_sound_table
);
end component;

component SAMPLE_MANAGER is
	generic (
		g_sample_width 	: integer := 8;
		g_patch_width		: integer := 7;
		g_mem_size			: integer := 12
	);
	port (
		i_clk					: in  std_logic;
		i_enable  		: in  std_logic;
		i_patch				: in	std_logic_vector(g_patch_width - 1 downto 0);
		i_address			: in  std_logic_vector(g_mem_size - 1 downto 0);
		o_mem_out			: out std_logic_vector(g_sample_width - 1 downto 0)
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

component rec_memory IS
	port
	(
		address		: IN STD_LOGIC_VECTOR (12 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		data		: IN STD_LOGIC_VECTOR (31 DOWNTO 0);
		wren		: IN STD_LOGIC ;
		q		: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	);
end component;

  -- real
  constant c_btn_size     : integer := 32;
  constant c_btn_short    : integer := 2500000;
  constant c_btn_long     : integer := 25000000;
  constant c_ts_clock_div : integer := 12207;

  -- testing with modelsim
  -- constant c_btn_size     : integer := 8;
  -- constant c_btn_short    : integer := 50;
  -- constant c_btn_long     : integer := 150;
  -- constant c_ts_clock_div : integer := 100;

  -- common
  constant c_ext_clock    : integer := 50000000;
  -- uart constants
  constant c_baud_rate    : integer := MIDI_BAUD_RATE;
  -- constant c_baud_rate    : integer := 115200;
  constant c_databits     : integer := 8;
  constant c_parity       : boolean := false;
  constant c_parity_odd   : boolean := false;
  constant c_stop_bits    : integer := 0;

  -- display
  signal s_display      : t_display_array;

  -- interface buttons (s -> short, l -> long)
  signal s_btn_l_s        : std_logic;
  signal s_btn_l_l        : std_logic;
  signal s_btn_u_s        : std_logic;
  signal s_btn_u_l        : std_logic;
  signal s_btn_d_s        : std_logic;
  signal s_btn_d_l        : std_logic;
  signal s_btn_r_s        : std_logic;
  signal s_btn_r_l        : std_logic;

  -- timestamp
  signal s_restart      : std_logic;
  signal s_play_pause_n : std_logic;
  signal s_ts_seconds   : std_logic_vector(ST_TSS_SIZE-1 downto 0);
  signal s_ts_fraction  : std_logic_vector(ST_TSF_SIZE-1 downto 0);

  -- uart rx
  signal s_rx_data      : std_logic_vector(c_databits-1 downto 0);
  signal s_uart_rx_end  : std_logic;
  signal s_uart_rx_err  : std_logic;

  -- midi filter
  signal s_midi_ready   : std_logic;
  signal s_midi_data    : std_logic_vector(SEQ_EVENT_SIZE - 1  downto 0);

  -- sample clock
  signal s_sample_clk   : std_logic;

  -- sound gen
  signal s_main_vol     : natural range 0 to MAIN_VOL_MAX;
  signal s_sound_on     : std_logic;
  signal s_sg_note      : t_sg_note;
  signal s_sg_vel       : t_sg_vel;
  signal s_sg_start     : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_sg_stop      : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_sg_poly      : std_logic_vector(SEQ_TRACKS - 1 downto 0);

  -- sound synth
  signal s_sg_patch       : t_sg_patch;
  signal s_sg_vol         : t_sg_vol;
  signal s_sample_enable  : t_sample_enable;
  signal s_sample_index   : t_sample_idx;

  -- playback queue and memory
  signal s_rec_mem_wr_en  : std_logic;
  signal s_rec_mem_add    : std_logic_vector(MEMORY_SIZE - 1 downto 0);
  signal s_rec_mem_out    : std_logic_vector(SEQ_EVENT_SIZE-1 downto 0);

  signal s_mem_wr_mux     : t_mem_wr_mux;
  signal s_mem_wr_mux_in  : std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);

  -- sample memory
  signal s_sample_mem_en    : std_logic;
  signal s_sample_mem_patch : std_logic_vector(TR_PATCH_SIZE - 1 downto 0);
  signal s_sample_mem_add   : std_logic_vector(SMP_MEM_SIZE - 1 downto 0);
  signal s_sample_mem_out   : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);

  -- sample out
  signal s_dac_out        : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
  signal s_clip           : std_logic;

begin

  o_dac_out       <= s_dac_out;
  o_clip          <= s_clip;

  s_sample_mem_en <= i_reset_n;

  TS_GEN : TIMESTAMP_GEN
  generic map (c_ts_clock_div)
  port map (
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,
    i_play_stop_n   => s_play_pause_n,
    i_restart       => s_restart,
    o_ts_fraction   => s_ts_fraction,
    o_ts_seconds    => s_ts_seconds
  );

  CORE : SEQUENCER_CORE
  port map(
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,
    i_btn_l_l       => s_btn_l_l,
    i_btn_l_s       => s_btn_l_s,
    i_btn_u_l       => s_btn_u_l,
    i_btn_u_s       => s_btn_u_s,
    i_btn_d_l       => s_btn_d_l,
    i_btn_d_s       => s_btn_d_s,
    i_btn_r_l       => s_btn_r_l,
    i_btn_r_s       => s_btn_r_s,
    i_tr_mute       => i_tr_mute,
    i_ts_seconds    => s_ts_seconds,
    i_ts_fraction   => s_ts_fraction,
    i_midi_ready    => s_midi_ready,
    i_midi_data     => s_midi_data,
    i_rec_data      => s_rec_mem_out,
    o_rec_mem_add   => s_rec_mem_add,
    o_rec_mem_wr    => s_rec_mem_wr_en,
    o_rec_mem_mux   => s_mem_wr_mux,
    o_sound_on      => s_sound_on,
    o_main_vol      => s_main_vol,
    o_restart       => s_restart,
    o_play_pause_n  => s_play_pause_n,
    o_sg_patch      => s_sg_patch,
    o_sg_vol        => s_sg_vol,
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

  REC_MEM : rec_memory
	port map(
		address		 => s_rec_mem_add(14 downto 2), -- address is byte aligned, memory is work aligned
		clock		   => i_clk,
		data		   => s_mem_wr_mux_in,
		wren		   => s_rec_mem_wr_en,
		q		       => s_rec_mem_out
	);

  SYNTH : SOUND_SYNTH
  port map (
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,

    i_sound_on      => s_sound_on,
    i_patch         => s_sg_patch,
    i_vol           => s_sg_vol,

    i_main_vol      => s_main_vol,

    i_sample_clk    => s_sample_clk,
    i_sample_en     => s_sample_enable,
    i_sample_idx    => s_sample_index,

    o_mem_patch     => s_sample_mem_patch,
    o_mem_address   => s_sample_mem_add,
    i_mem_sample    => s_sample_mem_out,

    o_clip          => s_clip,
    o_sample_out    => s_dac_out
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
      i_poly          => s_sg_poly(i),

      i_sample_clk    => s_sample_clk,

      i_start         => s_sg_start(i),
      i_stop          => s_sg_stop(i),
      i_note          => s_sg_note(i),
      i_vel           => s_sg_vel(i),

      o_sample_en     => s_sample_enable(i),
      o_sample_idx    => s_sample_index(i)
    );
  end generate;

  SAMPLE_MNG : SAMPLE_MANAGER
	generic map (
		g_sample_width 	=> SAMPLE_WIDTH,
		g_patch_width		=> TR_PATCH_SIZE,
		g_mem_size			=> SMP_MEM_SIZE
	)
	port map (
		i_clk					=> i_clk,
		i_enable  		=> s_sample_mem_en,
		i_patch			  => s_sample_mem_patch,
		i_address			=> s_sample_mem_add,
		o_mem_out			=> s_sample_mem_out
	);

  SAMPLE_CLK : SAMPLE_CLOCK
  generic map (
    g_ext_clock     => c_ext_clock,
    g_sample_freq   => SAMPLE_FREQ
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

  BTN_UP : SL_BUTTON
  generic map (
    g_size        => c_btn_size,
    g_short       => c_btn_short,
    g_long        => c_btn_long
  )
  port map (
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_btn         => i_btn_up,
    o_long        => s_btn_u_l,
    o_short       => s_btn_u_s
  );

  BTN_DOWN : SL_BUTTON
  generic map (
    g_size        => c_btn_size,
    g_short       => c_btn_short,
    g_long        => c_btn_long
  )
  port map (
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_btn         => i_btn_down,
    o_long        => s_btn_d_l,
    o_short       => s_btn_d_s
  );

  BTN_LEFT : SL_BUTTON
  generic map (
    g_size        => c_btn_size,
    g_short       => c_btn_short,
    g_long        => c_btn_long
  )
  port map (
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_btn         => i_btn_left,
    o_long        => s_btn_l_l,
    o_short       => s_btn_l_s
  );

  BTN_RIGTH : SL_BUTTON
  generic map (
    g_size        => c_btn_size,
    g_short       => c_btn_short,
    g_long        => c_btn_long
  )
  port map (
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_btn         => i_btn_right,
    o_long        => s_btn_r_l,
    o_short       => s_btn_r_s
  );

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
