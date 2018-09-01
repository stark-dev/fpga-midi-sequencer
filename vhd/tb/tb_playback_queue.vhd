library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_PB_QUEUE is
end TB_PB_QUEUE;

architecture TEST of TB_PB_QUEUE is

  component PLAYBACK_QUEUE is
  port (
    i_clk           : in  std_logic;
    i_reset_n       : in  std_logic;

    i_ts_seconds    : in  std_logic_vector(ST_TSS_SIZE-1 downto 0);
    i_ts_fraction   : in  std_logic_vector(ST_TSF_SIZE-1 downto 0);

    i_data_ready    : in  std_logic;
    i_load_data     : in  std_logic_vector(SEQ_EVENT_SIZE-1 downto 0);

    o_mem_load      : out std_logic;
    o_mem_address   : out std_logic_vector(MEMORY_SIZE - 1 downto 0);

    o_pb_ready      : out t_midi_ready;
    o_pb_data       : out t_midi_data;
    o_init_ready    : out std_logic;
    o_mem_error     : out std_logic
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

component SAMPLE_MEMORY is
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
  signal s_restart        : std_logic;
  signal s_ts_frac        : std_logic_vector(ST_TSF_SIZE - 1 downto 0);
  signal s_ts_secs        : std_logic_vector(ST_TSS_SIZE - 1 downto 0);

  signal s_data_ready     : std_logic;
  signal s_mem_data       : std_logic_vector(SEQ_EVENT_SIZE-1 downto 0);

  signal s_mem_load       : std_logic;
  signal s_mem_address    : std_logic_vector(MEMORY_SIZE - 1 downto 0);
  signal s_pb_ready       : t_midi_ready;
  signal s_pb_data        : t_midi_data;
  signal s_init_ready     : std_logic;
  signal s_mem_error      : std_logic;

  signal s_mem_enable     : std_logic;
  signal s_mem_write      : std_logic;

begin

  DUT : PLAYBACK_QUEUE
  port map(
    i_clk           => s_clk,
    i_reset_n       => s_rst,

    i_ts_seconds    => s_ts_secs,
    i_ts_fraction   => s_ts_frac,

    i_data_ready    => s_data_ready,
    i_load_data     => s_mem_data,

    o_mem_load      => s_mem_load,
    o_mem_address   => s_mem_address,

    o_pb_ready      => s_pb_ready,
    o_pb_data       => s_pb_data,
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

  SAMPLE_MEM : SAMPLE_MEMORY
  generic map ( 10 )
  port map (
    i_clk           => s_clk,
    i_reset_n       => s_rst,
    i_enable        => s_mem_enable,
    i_read_en       => s_mem_load,
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
    s_play_pause_n  <= '1';
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
