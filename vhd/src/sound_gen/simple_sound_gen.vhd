library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SIMPLE_SOUND_GEN is
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
  o_poly_cnt      : out std_logic_vector(up_int_log2(MAX_POLYPHONY) - 1 downto 0);
  o_sample_index  : out t_sample_idx
);
end entity;

architecture BHV of SIMPLE_SOUND_GEN is

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  constant SOUND_TABLE_ROW  : integer := SEQ_VEL_SIZE + 1 + g_smp_mem_size;

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  subtype  SAMPLE_ID_RANGE   is natural range SOUND_TABLE_ROW - 1 downto SOUND_TABLE_ROW - g_smp_mem_size;
  subtype  VEL_RANGE         is natural range SEQ_VEL_SIZE - 1 downto 0;
  constant START_STOP_N      : natural := SEQ_VEL_SIZE;

  type t_sound_table is array (2**SEQ_NOTE_SIZE - 1 downto 0) of std_logic_vector(SOUND_TABLE_ROW - 1 downto 0);
  type t_sample_inc  is array (2**SEQ_NOTE_SIZE - 1 downto 0) of std_logic_vector(g_smp_mem_size - 1 downto 0);

--------------------------------------------------------------------------------
-- components
--------------------------------------------------------------------------------
component SAMPLE_ID_COUNTER is
generic (
  g_size  : integer := 16
);
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;
  i_enable        : in  std_logic;
  i_tc_value      : in  std_logic_vector(g_size - 1 downto 0);
  i_sample_inc    : in  std_logic_vector(g_size - 1 downto 0);

  o_tc            : out std_logic;
  o_sample_id     : out std_logic_vector(g_size - 1 downto 0)
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
  -- sound table
  signal s_sound_table    : t_sound_table;
  signal s_vel_enable     : std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);

  -- sample index counters
  signal s_idx_cnt_en     : std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);
  signal s_idx_cnt_tc     : std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);
  signal s_idx_cnt_end    : std_logic_vector(g_smp_mem_size - 1 downto 0);
  signal s_sample_inc     : t_sample_inc;

  -- max polyphony counter
  signal s_max_poly       : unsigned(up_int_log2(MAX_POLYPHONY) - 1 downto 0);
  signal s_max_poly_start : unsigned(up_int_log2(MAX_POLYPHONY) - 1 downto 0);
  signal s_max_poly_end   : unsigned(up_int_log2(MAX_POLYPHONY) - 1 downto 0);
  signal s_max_poly_tc    : std_logic;


begin
  -- output  signal assignments
  o_patch           <= i_patch;
  o_poly_cnt        <= std_logic_vector(s_max_poly);

  p_sample_index_enc: process(s_sound_table)
    variable v_out_idx : integer range 0 to MAX_POLYPHONY - 1;
  begin
    o_sample_index <= (others => (others => '0'));
    v_out_idx := 0;

    for i in 0 to 2**SEQ_NOTE_SIZE - 1 loop
      if v_out_idx <= (MAX_POLYPHONY - 1) and s_sound_table(i)(START_STOP_N) = '1' then
        o_sample_index(v_out_idx) <= unsigned(s_sound_table(i)(SAMPLE_ID_RANGE));
        v_out_idx := v_out_idx + 1;
      end if;
    end loop;
  end process;

  -- internal signal assignments
  s_idx_cnt_end     <= (others => '1');
  s_sample_inc      <= (others => (others => '0'));-- to_unsigned(1, g_smp_mem_size))); -- TODO set sample increment for each note

  s_max_poly_start  <= (others => '0');
  s_max_poly_end    <= (others => '1');

  p_start_stop_assign: process(s_idx_cnt_en)
  begin
    for i in 0 to 2**SEQ_NOTE_SIZE - 1 loop
      s_sound_table(i)(START_STOP_N) <= s_idx_cnt_en(i);
    end loop;
  end process;

  -- components
  SOUND_TABLE_VEL_GEN:
  for i in 0 to 2**SEQ_NOTE_SIZE - 1 generate
    SOUND_TABLE_VEL_X : REGISTER_N
    generic map (SEQ_VEL_SIZE)
    port map (
      i_clk         => i_clk,
      i_reset_n     => i_reset_n,
      i_load_en     => s_vel_enable(i),
      i_par_in      => i_vel,
      o_par_out     => s_sound_table(i)(VEL_RANGE)
    );
  end generate;

  SOUND_TABLE_IDX_GEN:
  for i in 0 to 2**SEQ_NOTE_SIZE - 1 generate
    SOUND_TABLE_IDX_X : SAMPLE_ID_COUNTER
    generic map (g_smp_mem_size)
    port map (
      i_clk         => i_clk,
      i_reset_n     => s_idx_cnt_en(i), -- same as reset_n, when enabled reset_n is at 1
      i_enable      => s_idx_cnt_en(i),
      i_tc_value    => s_idx_cnt_end,
      i_sample_inc  => s_sample_inc(i),
      o_tc          => s_idx_cnt_tc(i),
      o_sample_id   => s_sound_table(i)(SAMPLE_ID_RANGE)
    );
  end generate;

  -- process
  p_sound_table_rst :process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' or i_sound_on = '0' then
      s_idx_cnt_en <= (others => '0');
    elsif i_clk'event and i_clk = '1' then
      if i_start = '1' then
        if s_max_poly_tc = '0' then -- prevent from increasing active notes over max polyphony
          s_idx_cnt_en(to_integer(unsigned(i_note))) <= '1';
        end if;
      elsif i_stop = '1' then
        s_idx_cnt_en(to_integer(unsigned(i_note))) <= '0';
      end if;
    end if;
  end process;

  p_sound_table_vel: process(i_reset_n, i_start, i_stop, i_note, i_vel)
  begin
    if i_reset_n = '0' then
      s_vel_enable <= (others => '0');
    else
      for i in 0 to 2**SEQ_NOTE_SIZE - 1 loop
        if i = to_integer(unsigned(i_note)) then
          s_vel_enable(i) <= i_start or i_stop;
        else
          s_vel_enable(i) <= '0';
        end if;
      end loop;
    end if;
  end process;

  p_polyphony_cnt: process(i_clk, i_reset_n, i_sound_on, i_note)
  begin
    if i_reset_n = '0' or i_sound_on = '0' then
      s_max_poly <= (others => '0');
    elsif i_clk'event and i_clk = '1' then
      if i_start = '1' then
        if s_max_poly /= s_max_poly_end and s_sound_table(to_integer(unsigned(i_note)))(START_STOP_N) = '0' then -- avoid two consecutive note on messages to increment counter
          s_max_poly <= s_max_poly + 1;
        end if;
      elsif i_stop = '1' then
        if s_max_poly /= s_max_poly_start and s_sound_table(to_integer(unsigned(i_note)))(START_STOP_N) = '1' then -- avoid two consecutive note off messages to decrement counter
          s_max_poly <= s_max_poly - 1;
        end if;
      end if;
    end if;
  end process;

end architecture;
