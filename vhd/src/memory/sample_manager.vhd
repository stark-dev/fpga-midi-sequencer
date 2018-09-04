library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SAMPLE_MANAGER is
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
end entity;

architecture BHV of SAMPLE_MANAGER is

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

  component UP_COUNTER_MOD is
    generic (
      N           : integer := 16
    );
    port (
      i_clk       : in   std_logic;
      i_reset_n   : in   std_logic;
      i_en        : in   std_logic;
      i_tc_value  : in   unsigned(N-1 downto 0);
      o_cnt_out   : out  unsigned(N-1 downto 0);
      o_chain_en  : out  std_logic
    );
  end component;

  -- constant c_mem_track_size : integer := MEMORY_TR_SIZE;
  constant c_mem_track_size : integer := 512; -- remove, just for test

  constant c_memory_tr_cnt  : integer := up_int_log2(c_mem_track_size / 8); -- each sample is 4 bytes (data) + 4 bytes (ts)

  type t_queue_fsm is (st_reset, st_init, st_ready, st_idle, st_scan, st_event,
                       st_load_data, st_wait_data, st_load_ts, st_wait_ts,
                       st_write_data, st_wait_write_data, st_write_ts, st_wait_write_ts, st_update,
                       st_error);

  type t_sample_cnt is array (SEQ_TRACKS - 1 downto 0) of unsigned(c_memory_tr_cnt - 1 downto 0);

  signal s_evt_data_r       : t_midi_data;
  signal s_evt_ts_r         : t_midi_ts;

  signal s_evt_end          : std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);

  signal s_ts_match         : std_logic;

  signal s_mem_address      : unsigned(MEMORY_SIZE - 1 downto 0);

  signal s_load_data_en     : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_load_ts_en       : std_logic_vector(SEQ_TRACKS - 1 downto 0);

  signal s_q_state          : t_queue_fsm;

  signal s_active_track     : unsigned(ST_TRACK_SIZE - 1 downto 0);

  signal s_track_scan       : unsigned(ST_TRACK_SIZE - 1 downto 0);
  signal s_track_scan_en    : std_logic;
  signal s_track_scan_rs    : std_logic;
  signal s_track_scan_tc    : std_logic;
  signal s_track_scan_end   : unsigned(ST_TRACK_SIZE - 1 downto 0);

  signal s_sample_count     : t_sample_cnt;
  signal s_sample_count_en  : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_sample_count_rs  : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_sample_count_tc  : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_sample_count_end : t_sample_cnt;

  signal s_init_ready       : std_logic;
  signal s_init_ready_rst   : std_logic;
  signal s_init_ready_set   : std_logic;

  signal s_rec_request      : std_logic;
  signal s_rec_request_set  : std_logic;
  signal s_rec_request_rst  : std_logic;

begin

  DATA_REG_GEN:
  for i in 0 to SEQ_TRACKS - 1 generate
    DATA_REG_X : REGISTER_N
    generic map(SEQ_EVENT_SIZE)
    port map(
      i_clk         => i_clk,
      i_reset_n     => i_reset_n,
      i_load_en     => s_load_data_en(i),
      i_par_in      => i_mem_data,
      o_par_out     => s_evt_data_r(i)
    );
  end generate;

  TS_REG_GEN:
  for i in 0 to SEQ_TRACKS - 1 generate
    TS_REG_X : REGISTER_N
    generic map(SEQ_TIME_SIZE)
    port map(
      i_clk         => i_clk,
      i_reset_n     => i_reset_n,
      i_load_en     => s_load_ts_en(i),
      i_par_in      => i_mem_data,
      o_par_out     => s_evt_ts_r(i)
    );
  end generate;

  SAMPLE_CNT_GEN:
  for i in 0 to SEQ_TRACKS - 1 generate
    SAMPLE_CNT_X : UP_COUNTER_MOD
    generic map(c_memory_tr_cnt)
    port map(
      i_clk       => i_clk,
      i_reset_n   => s_sample_count_rs(i),
      i_en        => s_sample_count_en(i),
      i_tc_value  => s_sample_count_end(i),
      o_cnt_out   => s_sample_count(i),
      o_chain_en  => s_sample_count_tc(i)
    );
  end generate;

  o_mem_address <= std_logic_vector(s_mem_address);
  o_init_ready  <= s_init_ready;

  s_active_track <= unsigned(i_active_track);

  s_sample_count_end <= (others => (others => '1'));

  s_track_scan_end  <= to_unsigned(SEQ_TRACKS - 1 , ST_TRACK_SIZE);

  s_ts_match <= '1' when (s_evt_ts_r(to_integer(s_track_scan))(ST_TSS_RANGE) = i_ts_seconds) and (s_evt_ts_r(to_integer(s_track_scan))(ST_TSF_RANGE) = i_ts_fraction) else '0';

  s_evt_end <= (others => '1');

  p_track_scan: process(s_track_scan_rs, i_clk)
  begin
    if s_track_scan_rs = '0' then
      s_track_scan <= (others => '0');
    elsif i_clk'event and i_clk = '1' then
      if s_track_scan_en = '1' then
        if s_track_scan_tc = '1' then
          s_track_scan <= (others => '0');
        else
          s_track_scan <= s_track_scan + 1;
        end if;
      end if;
    end if;
  end process;

  s_track_scan_tc <= '1' when s_track_scan = s_track_scan_end else '0';

  p_fsm_state: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_q_state <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_q_state is
        when st_reset     =>
          s_q_state <= st_init;
        when st_init      =>
          s_q_state <= st_wait_data;
        when st_ready     =>
          s_q_state <= st_idle;
        when st_idle      =>
          if s_rec_request = '1' then
            s_q_state <= st_write_data;
          elsif s_ts_match = '1' then
            s_q_state <= st_event;
          elsif i_mem_error = '1' then
            s_q_state <= st_error;
          else
            s_q_state <= st_scan;
          end if;
        when st_event     =>
          s_q_state <= st_wait_data;
        when st_scan      =>
          if s_init_ready = '1' then
            s_q_state <= st_idle;
          elsif s_track_scan_tc = '1' then
            s_q_state <= st_ready;
          else
            s_q_state <= st_init;
          end if;
        when st_wait_data =>
          if i_data_ready = '1' then
            s_q_state <= st_load_data;
          else
            s_q_state <= st_wait_data;
          end if;
        when st_load_data =>
          s_q_state <= st_wait_ts;
        when st_wait_ts   =>
          if i_data_ready = '1' then
            s_q_state <= st_load_ts;
          else
            s_q_state <= st_wait_ts;
          end if;
        when st_load_ts   =>
          s_q_state <= st_scan;
        when st_write_data =>
          s_q_state <= st_wait_write_data;
        when st_wait_write_data =>
          if i_data_ready = '1' then
            s_q_state <= st_write_ts;
          else
            s_q_state <= st_wait_write_data;
          end if;
        when st_write_ts =>
          s_q_state <= st_wait_write_ts;
        when st_wait_write_ts =>
          if i_data_ready = '1' then
            s_q_state <= st_update;
          else
            s_q_state <= st_wait_write_ts;
          end if;
        when st_update =>
          s_q_state <= st_idle;
        when st_error =>
          s_q_state <= st_error;
        when others       =>
          s_q_state <= st_reset;
      end case;
    end if;
  end process;

  p_fsm_ctrl: process(s_q_state, s_track_scan, s_sample_count, s_active_track, s_evt_end)
  begin
    o_mem_read        <= '0';  -- read request
    o_mem_write       <= '0';  -- write request
    o_mem_wr_mux      <= mux_off;
    s_mem_address     <= (others => '0');

    s_track_scan_rs   <= '1';
    s_track_scan_en   <= '0';

    for i in 0 to SEQ_TRACKS - 1 loop
      o_pb_ready(i)        <= '0';
      s_load_data_en(i)    <= '0';
      s_load_ts_en(i)      <= '0';

      s_sample_count_rs(i) <= '1';
      s_sample_count_en(i) <= '0';
    end loop;

    s_init_ready_rst  <= '0';
    s_init_ready_set  <= '0';

    case s_q_state is
      when st_reset     =>
        o_mem_read        <= '0';
        o_mem_write       <= '0';
        o_mem_wr_mux      <= mux_off;
        s_mem_address   <= (others => '0');

        s_track_scan_rs   <= '0';
        s_track_scan_en   <= '0';

        for i in 0 to SEQ_TRACKS - 1 loop
          o_pb_ready(i)        <= '0';
          s_load_data_en(i)    <= '0';
          s_load_ts_en(i)      <= '0';

          s_sample_count_rs(i) <= '0';
          s_sample_count_en(i) <= '0';
        end loop;

        s_init_ready_rst  <= '1';
        s_init_ready_set  <= '0';

      when st_init      =>

      when st_ready     =>
        s_init_ready_set  <= '1';
        s_track_scan_rs   <= '0';

      when st_idle      =>

      when st_event     =>
        if (s_track_scan /= s_active_track) then
          o_pb_ready(to_integer(s_track_scan)) <= '1';
          s_sample_count_en(to_integer(s_track_scan)) <= '1'; -- increment current sample counter
        end if;

      when st_scan      =>
        s_track_scan_en <= '1';

      when st_wait_data =>
        o_mem_read        <= '1';
        s_mem_address <= to_unsigned((c_mem_track_size * (to_integer(s_track_scan))) + to_integer(8 * s_sample_count(to_integer(s_track_scan))), MEMORY_SIZE);

      when st_load_data =>
        s_load_data_en(to_integer(s_track_scan)) <= '1';
        s_mem_address <= to_unsigned((c_mem_track_size * (to_integer(s_track_scan))) + to_integer(8 * s_sample_count(to_integer(s_track_scan))), MEMORY_SIZE);

      when st_wait_ts   =>
        o_mem_read        <= '1';
        s_mem_address <= to_unsigned((c_mem_track_size * (to_integer(s_track_scan))) + to_integer(8 * s_sample_count(to_integer(s_track_scan))) + 4, MEMORY_SIZE);

      when st_load_ts   =>
        s_load_ts_en(to_integer(s_track_scan)) <= '1';
        s_mem_address <= to_unsigned((c_mem_track_size * (to_integer(s_track_scan))) + to_integer(8 * s_sample_count(to_integer(s_track_scan))) + 4, MEMORY_SIZE);

      when st_write_data =>
        o_mem_write     <= '1';
        o_mem_wr_mux    <= mux_midi;
        s_mem_address <= to_unsigned((c_mem_track_size * (to_integer(s_active_track))) + to_integer(8 * s_sample_count(to_integer(s_active_track))), MEMORY_SIZE);

      when st_wait_write_data =>
        o_mem_wr_mux    <= mux_midi;
        s_mem_address <= to_unsigned((c_mem_track_size * (to_integer(s_active_track))) + to_integer(8 * s_sample_count(to_integer(s_active_track))), MEMORY_SIZE);

      when st_write_ts  =>
        o_mem_write     <= '1';
        o_mem_wr_mux    <= mux_ts;
        s_mem_address <= to_unsigned((c_mem_track_size * (to_integer(s_active_track))) + to_integer(8 * s_sample_count(to_integer(s_active_track))) + 4, MEMORY_SIZE);

      when st_wait_write_ts  =>
        o_mem_wr_mux    <= mux_ts;
        s_mem_address <= to_unsigned((c_mem_track_size * (to_integer(s_active_track))) + to_integer(8 * s_sample_count(to_integer(s_active_track))) + 4, MEMORY_SIZE);

      when st_update         =>
        s_sample_count_en(to_integer(s_active_track)) <= '1'; -- increment active track sample counter

      when others       =>
        o_mem_read        <= '0';  -- load request
        o_mem_write       <= '0';
        o_mem_wr_mux      <= mux_off;
        s_mem_address   <= (others => '0');

        s_track_scan_rs   <= '0';
        s_track_scan_en   <= '0';

        for i in 0 to SEQ_TRACKS - 1 loop
          o_pb_ready(i)        <= '0';
          s_load_data_en(i)    <= '0';
          s_load_ts_en(i)      <= '0';

          s_sample_count_rs(i) <= '0';
          s_sample_count_en(i) <= '0';
        end loop;

        s_init_ready_rst  <= '1';
        s_init_ready_set  <= '0';
    end case;

  end process;

  p_evt_data: process(s_evt_data_r)
  begin
    for i in 0 to SEQ_TRACKS - 1 loop
      o_pb_data(i)  <= s_evt_data_r(i);
    end loop;
  end process;

  p_init_ready: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_init_ready <= '0';
    elsif i_clk'event and i_clk = '1' then
      if s_init_ready_rst = '1' then
        s_init_ready <= '0';
      elsif s_init_ready_set = '1' then
        s_init_ready <= '1';
      end if;
    end if;
  end process;

  p_playback_end: process(i_reset_n, s_init_ready, s_evt_data_r, s_active_track)
  begin
    if i_reset_n = '0' then
      o_pb_end <= (others => '0');
    else
      for i in 0 to SEQ_TRACKS - 1 loop
        if s_init_ready = '1' then
          if s_evt_data_r(i) = s_evt_end then
            o_pb_end(i) <= '1';
          elsif i = to_integer(s_active_track) then
            o_pb_end(i) <= '1';
          else
            o_pb_end(i) <= '0';
          end if;
        else
          o_pb_end(i) <= '0';
        end if;
      end loop;
    end if;
  end process;

  p_rec_request: process(s_rec_request_set, s_rec_request_rst)
  begin
    if s_rec_request_rst = '1' then
      s_rec_request <= '0';
    elsif s_rec_request_set = '1' then
      s_rec_request <= '1';
    end if;
  end process;

  p_rec_request_ctrl: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_rec_request_rst <= '1';
      s_rec_request_set <= '0';
    elsif i_clk'event and i_clk = '1' then
      if i_rec_mode = '1' and i_midi_ready = '1' then
        s_rec_request_set <= '1';
        s_rec_request_rst <= '0';
      elsif s_q_state = st_write_data then
        s_rec_request_set <= '0';
        s_rec_request_rst <= '1';
      end if;
    end if;
  end process;

end architecture;
