library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SOUND_SYNTH is
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
  o_dac_out_l     : out std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
  o_dac_out_r     : out std_logic_vector(SAMPLE_WIDTH - 1 downto 0)
);
end entity;

architecture BHV of SOUND_SYNTH is

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  type t_synth_fsm is (st_reset, st_idle, st_update, st_current, st_read, st_wait, st_scan);

--------------------------------------------------------------------------------
-- components
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- additive synthesis
  signal s_fsm_state            : t_synth_fsm;

  signal s_next_sample          : std_logic;
  signal s_next_sample_set      : std_logic;
  signal s_next_sample_rst      : std_logic;

  -- mem sample
  signal s_mem_sample_ext       : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);

  -- current sample register
  signal s_current_sample_in    : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);
  signal s_current_sample_en    : std_logic;
  signal s_current_sample_rst   : std_logic;
  signal s_current_sample       : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);

  signal s_sample_offset        : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);

  -- out sample register
  signal s_out_sample_in        : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
  signal s_out_sample_en        : std_logic;
  signal s_out_sample_rst       : std_logic;
  signal s_out_sample           : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);

  signal s_out_sample_off       : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);
  signal s_out_sample_lim       : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);
  signal s_out_sample_ext       : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);

  signal s_clip                 : std_logic;

  -- sample scan counter
  signal s_sample_scan          : unsigned(MAX_POLY_BIT - 1 downto 0);
  signal s_sample_scan_en       : std_logic;
  signal s_sample_scan_rst      : std_logic;
  signal s_sample_scan_tc       : std_logic;
  signal s_sample_scan_end      : unsigned(MAX_POLY_BIT - 1 downto 0);

  -- track scan counter
  signal s_track_scan           : unsigned(up_int_log2(SEQ_TRACKS - 1) - 1 downto 0);
  signal s_track_scan_en        : std_logic;
  signal s_track_scan_rst       : std_logic;
  signal s_track_scan_tc        : std_logic;
  signal s_track_scan_end       : unsigned(up_int_log2(SEQ_TRACKS - 1) - 1 downto 0);

begin

  o_dac_out_l       <= s_out_sample;
  o_dac_out_r       <= s_out_sample;

  o_clip            <= s_clip;

  o_mem_patch       <= i_patch(to_integer(s_track_scan));
  o_mem_address     <= i_sample_idx(to_integer(s_track_scan))(to_integer(s_sample_scan));

  s_sample_scan_end <= to_unsigned(MAX_POLYPHONY - 1, MAX_POLY_BIT);
  s_sample_offset   <= std_logic_vector(to_unsigned(2**(SAMPLE_WIDTH - 1), 2*SAMPLE_WIDTH));

  s_track_scan_en   <= s_sample_scan_tc;
  s_track_scan_end  <= (others => '1');

  s_out_sample_in <= s_out_sample_off(SAMPLE_WIDTH - 1 downto 0);

  s_out_sample_off <= std_logic_vector(signed(s_out_sample_lim) + signed(s_sample_offset));
  s_current_sample_in <= std_logic_vector(signed(s_current_sample) + signed(s_mem_sample_ext));

  p_limiter: process(s_out_sample_ext)
  begin
    if s_out_sample_ext(2*SAMPLE_WIDTH - 1) = '1' then  -- negative
      if and_reduce(s_out_sample_ext(2*SAMPLE_WIDTH - 1 downto SAMPLE_WIDTH - 1)) = '0' then -- overflow
        s_out_sample_lim(2*SAMPLE_WIDTH - 1 downto SAMPLE_WIDTH - 1)  <= (others => '1');
        s_out_sample_lim(SAMPLE_WIDTH - 2 downto 0)                   <= (others => '0');
        s_clip                                                        <= '1';
      else
        s_out_sample_lim <= s_out_sample_ext;
        s_clip                                                        <= '0';
      end if;
    else
      -- positive
      if or_reduce(s_out_sample_ext(2*SAMPLE_WIDTH - 1 downto SAMPLE_WIDTH - 1)) = '1' then -- overflow
        s_out_sample_lim(2*SAMPLE_WIDTH - 1 downto SAMPLE_WIDTH - 1)  <= (others => '0');
        s_out_sample_lim(SAMPLE_WIDTH - 2 downto 0)                   <= (others => '1');
        s_clip           <= '1';
      else
        s_out_sample_lim <= s_out_sample_ext;
        s_clip           <= '0';
      end if;
    end if;
  end process;

  p_main_vol: process(i_main_vol, s_current_sample)
  begin
    if i_main_vol = 0 then
      s_out_sample_ext <= (others => '0');
    else
      s_out_sample_ext <= std_logic_vector(shift_right(signed(s_current_sample), MAIN_VOL_MAX - i_main_vol));
    end if;
  end process;

  p_mem_sample_ext: process(i_mem_sample, i_vol, s_track_scan)
    variable v_track  : integer := 0;
    variable v_vol    : integer := 0;
  begin
    v_track := to_integer(s_track_scan);
    v_vol   := to_integer(unsigned(i_vol(v_track)));
    if v_vol = 0 then
      s_mem_sample_ext <= (others => '0');
    else
      s_mem_sample_ext(2*SAMPLE_WIDTH -1 downto SAMPLE_WIDTH - (TR_VOL_MAX - v_vol))  <= (others => i_mem_sample(SAMPLE_WIDTH - 1));
      s_mem_sample_ext(SAMPLE_WIDTH - (TR_VOL_MAX - v_vol) - 1 downto 0)              <= i_mem_sample(SAMPLE_WIDTH - 1 downto (SAMPLE_WIDTH - 1 - v_vol));
    end if;
  end process;

  p_next_sample_set: process(s_fsm_state, i_sample_clk)
  begin
    if s_fsm_state /= st_idle then
      s_next_sample_set <= '0';
    elsif i_sample_clk'event and i_sample_clk = '1' then
      s_next_sample_set <= '1';
    end if;
  end process;

  SAMPLE_SCAN_C : UP_COUNTER_MOD
  generic map ( MAX_POLY_BIT )
  port map (
    i_clk       => i_clk,
    i_reset_n   => s_sample_scan_rst,
    i_en        => s_sample_scan_en,
    i_tc_value  => s_sample_scan_end,
    o_cnt_out   => s_sample_scan,
    o_chain_en  => s_sample_scan_tc
  );

  TRACK_SCAN_C : UP_COUNTER_MOD
  generic map ( up_int_log2(SEQ_TRACKS - 1) )
  port map (
    i_clk       => i_clk,
    i_reset_n   => s_track_scan_rst,
    i_en        => s_track_scan_en,
    i_tc_value  => s_track_scan_end,
    o_cnt_out   => s_track_scan,
    o_chain_en  => s_track_scan_tc
  );

  OUT_SAMPLE_R : REGISTER_N
  generic map ( SAMPLE_WIDTH )
  port map (
    i_clk         => i_clk,
    i_reset_n     => s_out_sample_rst,
    i_load_en     => s_out_sample_en,
    i_par_in      => s_out_sample_in,
    o_par_out     => s_out_sample
  );

  CURRENT_SAMPLE_R : REGISTER_N
  generic map ( 2*SAMPLE_WIDTH )
  port map (
    i_clk         => i_clk,
    i_reset_n     => s_current_sample_rst,
    i_load_en     => s_current_sample_en,
    i_par_in      => s_current_sample_in,
    o_par_out     => s_current_sample
  );

  p_fsm_state: process(i_clk, i_reset_n, i_sound_on)
  begin
    if i_reset_n = '0' or i_sound_on = '0' then
      s_fsm_state <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_fsm_state is
        when st_reset     =>
          s_fsm_state <= st_idle;
        when st_idle      =>
          if s_next_sample = '1' then
            s_fsm_state <= st_update;
          else
            s_fsm_state <= st_idle;
          end if;
        when st_update    =>
          s_fsm_state <= st_current;
        when st_current   =>
          s_fsm_state <= st_read;
        when st_read      =>
          s_fsm_state <= st_wait;
        when st_wait      =>
          s_fsm_state <=  st_scan;
        when st_scan      =>
          if s_sample_scan_tc = '1' and s_track_scan_tc = '1' then
            s_fsm_state <= st_idle;
          else
            s_fsm_state <= st_read;
          end if;
        when others       =>
          s_fsm_state <= st_reset;
      end case;
    end if;
  end process;

  p_fsm_ctrl: process(s_fsm_state, i_sample_en, s_track_scan, s_sample_scan)
  begin
    case s_fsm_state is
      when st_reset     =>
        s_sample_scan_rst     <= '0';
        s_sample_scan_en      <= '0';
        s_next_sample_rst     <= '1';

        s_track_scan_rst      <= '0';

        s_current_sample_rst  <= '0';
        s_current_sample_en   <= '0';

        s_out_sample_en       <= '0';
        s_out_sample_rst      <= '0';

      when st_idle      =>
        s_sample_scan_rst     <= '1';
        s_sample_scan_en      <= '0';
        s_next_sample_rst     <= '0';

        s_track_scan_rst      <= '1';

        s_current_sample_rst  <= '1';
        s_current_sample_en   <= '0';

        s_out_sample_en       <= '0';
        s_out_sample_rst      <= '1';

      when st_update    =>  -- out sample <= current sample
        s_sample_scan_rst     <= '1';
        s_sample_scan_en      <= '0';
        s_next_sample_rst     <= '1';

        s_track_scan_rst      <= '1';

        s_current_sample_rst  <= '1';
        s_current_sample_en   <= '0';

        s_out_sample_en       <= '1';
        s_out_sample_rst      <= '1';

      when st_current   =>  -- reset current sample reg
        s_sample_scan_rst     <= '1';
        s_sample_scan_en      <= '0';
        s_next_sample_rst     <= '1';

        s_track_scan_rst      <= '1';

        s_current_sample_rst  <= '0';
        s_current_sample_en   <= '0';

        s_out_sample_en       <= '0';
        s_out_sample_rst      <= '1';

      when st_read      =>
        s_sample_scan_rst     <= '1';
        s_sample_scan_en      <= '0';
        s_next_sample_rst     <= '1';

        s_track_scan_rst      <= '1';

        s_current_sample_rst  <= '1';
        s_current_sample_en   <= '0';

        s_out_sample_en       <= '0';
        s_out_sample_rst      <= '1';

      when st_wait      =>
        s_sample_scan_rst     <= '1';
        s_sample_scan_en      <= '0';
        s_next_sample_rst     <= '1';

        s_track_scan_rst      <= '1';

        s_current_sample_rst  <= '1';
        if i_sample_en(to_integer(s_track_scan))(to_integer(s_sample_scan)) = '1' then
          s_current_sample_en   <= '1';
        else
          s_current_sample_en   <= '0';
        end if;

        s_out_sample_en       <= '0';
        s_out_sample_rst      <= '1';

      when st_scan      =>
        s_sample_scan_rst     <= '1';
        s_sample_scan_en      <= '1';
        s_next_sample_rst     <= '1';

        s_track_scan_rst      <= '1';

        s_current_sample_rst  <= '1';
        s_current_sample_en   <= '0';

        s_out_sample_en       <= '0';
        s_out_sample_rst      <= '1';

      when others       =>
        s_sample_scan_rst     <= '0';
        s_sample_scan_en      <= '0';
        s_next_sample_rst     <= '1';

        s_track_scan_rst      <= '0';

        s_current_sample_rst  <= '0';
        s_current_sample_en   <= '0';

        s_out_sample_en       <= '0';
        s_out_sample_rst      <= '0';
    end case;
  end process;

  p_next_sample: process(s_next_sample_set, s_next_sample_rst)
  begin
    if s_next_sample_rst = '1' then
      s_next_sample <= '0';
    elsif s_next_sample_set = '1' then
      s_next_sample <= '1';
    end if;
  end process;

end architecture;
