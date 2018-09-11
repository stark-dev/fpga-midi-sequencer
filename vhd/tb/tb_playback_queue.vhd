library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_PB_QUEUE is
end TB_PB_QUEUE;

architecture TEST of TB_PB_QUEUE is

  component EVENT_MANAGER is
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

    o_mem_read      : out std_logic;
    o_mem_write     : out std_logic;
    o_mem_address   : out std_logic_vector(MEMORY_SIZE - 1 downto 0);
    o_mem_wr_mux    : out t_mem_wr_mux;
    o_mem_error     : out std_logic;

    o_pb_ready      : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
    o_pb_end        : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
    o_pb_data       : out t_midi_data;

    o_init_ready    : out std_logic
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

  constant c_ext_clock    : integer := 50000000;
  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_clock_half_p : time := c_clock * 1 ns;

  signal s_clk            : std_logic;
  signal s_rst            : std_logic;

  signal s_play_pause_n   : std_logic;
  signal s_rec_mode       : std_logic;
  signal s_restart        : std_logic;
  signal s_ts_frac        : std_logic_vector(ST_TSF_SIZE - 1 downto 0);
  signal s_ts_secs        : std_logic_vector(ST_TSS_SIZE - 1 downto 0);

  signal s_active_track   : std_logic_vector(ST_TRACK_SIZE - 1 downto 0);

  signal s_midi_ready     : std_logic;

  signal s_data_ready     : std_logic;
  signal s_mem_data       : std_logic_vector(SEQ_EVENT_SIZE-1 downto 0);

  signal s_mem_read       : std_logic;
  signal s_mem_write      : std_logic;
  signal s_mem_address    : std_logic_vector(MEMORY_SIZE - 1 downto 0);
  signal s_pb_ready       : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_pb_end         : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_pb_data        : t_midi_data;
  signal s_init_ready     : std_logic;
  signal s_mem_error      : std_logic;

  signal s_mem_enable     : std_logic;
  signal s_mem_wr_mux     : t_mem_wr_mux;

begin

  DUT : EVENT_MANAGER
  port map(
    i_clk           => s_clk,
    i_reset_n       => s_rst,

    i_rec_mode      => s_rec_mode,
    i_ts_seconds    => s_ts_secs,
    i_ts_fraction   => s_ts_frac,

    i_active_track  => s_active_track,
    i_midi_ready    => s_midi_ready,

    i_data_ready    => s_data_ready,
    i_mem_data      => s_mem_data,

    o_mem_read      => s_mem_read,
    o_mem_write     => s_mem_write,
    o_mem_address   => s_mem_address,
    o_mem_wr_mux    => s_mem_wr_mux,

    o_pb_ready      => s_pb_ready,
    o_pb_data       => s_pb_data,
    o_pb_end        => s_pb_end,
    o_init_ready    => s_init_ready,
    o_mem_error     => s_mem_error
  );

  TS_GEN : TIMESTAMP_GEN
  generic map (100)
  port map (
    i_clk           => s_clk,
    i_reset_n       => s_rst,
    i_play_stop_n   => s_play_pause_n,
    i_restart       => s_restart,
    o_ts_fraction   => s_ts_frac,
    o_ts_seconds    => s_ts_secs
  );

  SAMPLE_MEM : REC_MEMORY
  generic map ( 10 )
  port map (
    i_clk           => s_clk,
    i_reset_n       => s_rst,
    i_enable        => s_mem_enable,
    i_read_en       => s_mem_read,
    i_write_en      => s_mem_write,
    i_address       => s_mem_address(11 downto 2), -- 4 byte align
    i_load_data     => s_pb_data(0),
    o_data          => s_mem_data,
    o_mem_ready     => s_data_ready,
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
    s_rst           <= '1';
    s_mem_enable    <= '0';
    s_rec_mode      <= '0';
    s_midi_ready    <= '0';
    s_play_pause_n  <= '1';
    s_active_track  <= (others => '0');
    s_restart       <= '0';
    wait for 100 ns;

    s_rst        <= '0';
    s_mem_enable <= '0';
    wait for 100 ns;

    s_rst        <= '1';
    s_mem_enable <= '1';
    wait for 100 ns;


    wait;
  end process;

end TEST;
