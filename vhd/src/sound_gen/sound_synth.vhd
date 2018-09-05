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

  i_sample_clk    : in  std_logic;
  i_sample_en     : in  t_sample_enable;
  i_sample_idx    : in  t_sample_idx;

  o_sample_out    : out std_logic_vector(SAMPLE_WIDTH - 1 downto 0)
);
end entity;

architecture BHV of SOUND_SYNTH is

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  type t_synth_fsm is (st_reset, st_idle, st_scan, st_write);

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
  signal s_mem_sample           : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
  signal s_mem_sample_ext       : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);

  -- current sample register
  signal s_current_sample_in    : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);
  signal s_current_sample_en    : std_logic;
  signal s_current_sample_rst   : std_logic;
  signal s_current_sample       : std_logic_vector(2*SAMPLE_WIDTH - 1 downto 0);

  -- out sample register
  signal s_out_sample_in        : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);
  signal s_out_sample_en        : std_logic;
  signal s_out_sample_rst       : std_logic;
  signal s_out_sample           : std_logic_vector(SAMPLE_WIDTH - 1 downto 0);

  -- sample scan counter
  signal s_sample_scan          : unsigned(MAX_POLY_BIT - 1 downto 0);
  signal s_sample_scan_en       : std_logic;
  signal s_sample_scan_rst      : std_logic;
  signal s_sample_scan_tc       : std_logic;
  signal s_sample_scan_end      : unsigned(MAX_POLY_BIT - 1 downto 0);

  -- track scan counter
  signal s_track_scan           : unsigned(SEQ_TRACKS - 1 downto 0);
  signal s_track_scan_en        : std_logic;
  signal s_track_scan_rst       : std_logic;
  signal s_track_scan_tc        : std_logic;
  signal s_track_scan_end       : unsigned(SEQ_TRACKS - 1 downto 0);

begin

  s_sample_scan_end <= (others => '1');
  s_track_scan_end  <= (others => '1');

  s_track_scan_en <= s_sample_scan_tc;

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

  p_fsm_state: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_fsm_state <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_fsm_state is
        when st_reset     =>
          s_fsm_state <= st_idle;
        when st_idle      =>
          if s_next_sample = '1' then
            s_fsm_state <= st_scan;
          else
            s_fsm_state <= st_idle;
          end if;
        when st_scan      =>
          if s_sample_scan_tc = '1' and s_track_scan_tc = '1' then
            s_fsm_state <= st_write;
          else
            s_fsm_state <= st_scan;
          end if;
        when st_write     =>
          s_fsm_state <= st_idle;
        when others       =>
          s_fsm_state <= st_reset;
      end case;
    end if;
  end process;

  p_fsm_ctrl: process(s_fsm_state)
  begin
    case s_fsm_state is
      when st_reset     =>
        s_sample_scan_rst     <= '0';
        s_sample_scan_en      <= '0';

        s_track_scan_rst      <= '0';

        s_current_sample_rst  <= '0';

        s_out_sample_rst      <= '0';
        s_out_sample_en       <= '0';

      when st_idle      =>
        s_sample_scan_rst     <= '1';
        s_sample_scan_en      <= '0';

        s_track_scan_rst      <= '1';

        s_current_sample_rst  <= '0';

        s_out_sample_rst      <= '1';
        s_out_sample_en       <= '0';

      when st_scan      =>
        s_sample_scan_rst     <= '1';
        s_sample_scan_en      <= '1';

        s_track_scan_rst      <= '1';

        s_current_sample_rst  <= '1';

        s_out_sample_rst      <= '1';
        s_out_sample_en       <= '0';

      when st_write     =>
        s_sample_scan_rst     <= '1';
        s_sample_scan_en      <= '0';

        s_track_scan_rst      <= '1';

        s_current_sample_rst  <= '1';

        s_out_sample_rst      <= '1';
        s_out_sample_en       <= '1';

      when others       =>
        s_sample_scan_rst     <= '0';
        s_sample_scan_en      <= '0';

        s_track_scan_rst      <= '0';

        s_current_sample_rst  <= '0';

        s_out_sample_rst      <= '0';
        s_out_sample_en       <= '0';
    end case;
  end process;

  s_next_sample_rst <= '1' when i_reset_n = '0' else
                       '1' when s_fsm_state = st_scan else
                       '0';

  s_next_sample_set <= i_sample_clk;

  p_next_sample: process(s_next_sample_set, s_next_sample_rst)
  begin
    if s_next_sample_rst = '1' then
      s_next_sample <= '0';
    elsif s_next_sample_set = '1' then
      s_next_sample <= '1';
    end if;
  end process;

  s_out_sample_in <= s_current_sample(SAMPLE_WIDTH - 1 downto 0);

  p_mem_sample: process(i_patch, s_track_scan, s_sample_scan)
  begin
    case i_patch(to_integer(s_track_scan)) is
      -- when 0      =>
        -- sine mem
        -- s_mem_sample <= sine_mem(to_integer(unsigned(s_sample_scan)));
      -- when 1      =>
        -- square mem
        -- s_mem_sample <= (others => '0');
      when others =>
        s_mem_sample <= (others => '0');
    end case;
  end process;

  p_sample_mem_ext: process(s_mem_sample)
  begin
    for i in SAMPLE_WIDTH to 2*SAMPLE_WIDTH - 1 loop
      s_mem_sample_ext(i) <= s_mem_sample(SAMPLE_WIDTH - 1);
    end loop;
  end process;

  s_mem_sample_ext(SAMPLE_WIDTH - 1 downto 0) <= s_mem_sample;

  p_current_sample_in: process(i_sample_en, s_track_scan, s_sample_scan, s_current_sample, s_mem_sample_ext)
  begin
    if i_sample_en(to_integer(s_track_scan))(to_integer(s_sample_scan)) = '1' then
      s_current_sample_in <= std_logic_vector(signed(s_current_sample) + signed(s_mem_sample_ext));
    else
      s_current_sample_in <= s_current_sample;
    end if;
  end process;

end architecture;
