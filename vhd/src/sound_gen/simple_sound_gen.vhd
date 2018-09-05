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
  o_sample_en     : out std_logic_vector(MAX_POLYPHONY - 1 downto 0);
  o_sample_idx    : out t_sound_gen_out
);
end entity;

architecture BHV of SIMPLE_SOUND_GEN is

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  type t_table_vel   is array (2**SEQ_NOTE_SIZE - 1 downto 0) of std_logic_vector(SEQ_VEL_SIZE - 1 downto 0);

--------------------------------------------------------------------------------
-- components
--------------------------------------------------------------------------------

component SAMPLE_ENCODER is
port (
  i_clk       : in  std_logic;
  i_reset_n   : in  std_logic;
  i_enable    : in  std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);
  i_data      : in  t_sound_table;
  o_enable    : out std_logic_vector(MAX_POLYPHONY - 1 downto 0);
  o_output    : out t_sound_gen_out
);
end component;

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- sound table
  signal s_vel_enable     : std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);
  signal s_table_vel      : t_table_vel;

  -- sample index counters
  signal s_idx_cnt_en     : std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);
  signal s_idx_cnt_tc     : std_logic_vector(2**SEQ_NOTE_SIZE - 1 downto 0);
  signal s_idx_cnt_end    : std_logic_vector(g_smp_mem_size - 1 downto 0);
  signal s_idx_cnt        : t_sound_table;
  signal s_sample_inc     : t_sound_table;

  -- max polyphony counter
  signal s_max_poly       : unsigned(MAX_POLY_BIT - 1 downto 0);
  signal s_max_poly_start : unsigned(MAX_POLY_BIT - 1 downto 0);
  signal s_max_poly_end   : unsigned(MAX_POLY_BIT - 1 downto 0);
  signal s_max_poly_tc    : std_logic;

  -- encoder
  signal s_sample_enc_rst : std_logic;

begin
  -- output  signal assignments
  o_patch           <= i_patch;

  -- internal signal assignments
  s_idx_cnt_end     <= (others => '1');
  s_sample_inc      <= (others => (others => '0'));-- to_unsigned(1, g_smp_mem_size))); -- TODO set sample increment for each note

  s_max_poly_start  <= (others => '0');
  s_max_poly_end    <= (others => '1');

  s_sample_enc_rst  <= i_reset_n and i_sound_on;

  -- components
  SAMPLE_ENC : SAMPLE_ENCODER
  port map (
    i_clk           => i_sample_clk,
    i_reset_n       => s_sample_enc_rst,
    i_enable        => s_idx_cnt_en,
    i_data          => s_idx_cnt,
    o_enable        => o_sample_en,
    o_output        => o_sample_idx
  );

  p_table_vel: process(i_reset_n, i_clk)
  begin
    for i in 0 to 2**SEQ_NOTE_SIZE - 1 loop
      if i_reset_n = '0' then
        s_table_vel(i) <= (others => '0');
      elsif i_clk'event and i_clk ='1' then
        if s_vel_enable(i) = '1' then
          s_table_vel(i) <= i_vel;
        end if;
      end if;
    end loop;
  end process;

  p_sample_idx: process(i_reset_n, i_sample_clk)
  begin
    for i in 0 to 2**SEQ_NOTE_SIZE - 1 loop
      if i_reset_n = '0' then
        s_idx_cnt(i) <= (others => '0');
      elsif i_sample_clk'event and i_sample_clk = '1' then
        if s_idx_cnt_en(i) = '1' then
          if s_idx_cnt(i) = s_idx_cnt_end then
            s_idx_cnt(i) <= (others => '0');
          else
            s_idx_cnt(i) <= std_logic_vector(unsigned(s_idx_cnt(i)) + 1);
          end if;
        end if;
      end if;
    end loop;
  end process;

  p_sample_idx_tc: process(s_idx_cnt, s_idx_cnt_end)
  begin
    for i in 0 to 2**SEQ_NOTE_SIZE - 1 loop
      if s_idx_cnt(i) = s_idx_cnt_end then
        s_idx_cnt_tc(i) <= '1';
      else
        s_idx_cnt_tc(i) <= '0';
      end if;
    end loop;
  end process;

  -- process
  p_sound_table_rst :process(i_reset_n, i_clk, i_sound_on)
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
        if s_max_poly /= s_max_poly_end and s_idx_cnt_en(to_integer(unsigned(i_note))) = '0' then -- avoid two consecutive note on messages to increment counter
          s_max_poly <= s_max_poly + 1;
        end if;
      elsif i_stop = '1' then
        if s_max_poly /= s_max_poly_start and s_idx_cnt_en(to_integer(unsigned(i_note))) = '1' then -- avoid two consecutive note off messages to decrement counter
          s_max_poly <= s_max_poly - 1;
        end if;
      end if;
    end if;
  end process;

  s_max_poly_tc <= '1' when s_max_poly = s_max_poly_end else '0';

end architecture;
