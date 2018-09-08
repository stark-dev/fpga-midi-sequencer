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
  i_poly          : in  std_logic;

  i_sample_clk    : in  std_logic;

  i_start         : in  std_logic;
  i_stop          : in  std_logic;
  i_note          : in  std_logic_vector(SEQ_NOTE_SIZE - 1 downto 0);
  i_vel           : in  std_logic_vector(SEQ_VEL_SIZE - 1 downto 0);

  o_sample_en     : out std_logic_vector(MAX_POLYPHONY - 1 downto 0);
  o_sample_idx    : out t_sound_table
);
end entity;

architecture BHV of SIMPLE_SOUND_GEN is

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

  type t_sample_inc is array (2**SEQ_NOTE_SIZE - 1 downto 0) of unsigned(g_smp_mem_size - 1 downto 0);

  type t_note_array is array (MAX_POLYPHONY - 1 downto 0) of std_logic_vector(SEQ_NOTE_SIZE - 1 downto 0);
  type t_vel_array is array (MAX_POLYPHONY - 1 downto 0) of std_logic_vector(SEQ_VEL_SIZE - 1 downto 0);

--------------------------------------------------------------------------------
-- functions
--------------------------------------------------------------------------------
  function init_sample_inc return t_sample_inc is
    variable v_tmp : t_sample_inc := (others => (others => '0'));
    begin
      for idx in 0 to 2**SEQ_NOTE_SIZE - 1 loop
        -- TODO intialize correct sample increment
        v_tmp(idx) := to_unsigned(idx, g_smp_mem_size);
      end loop;
    return v_tmp;
  end init_sample_inc;

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- sound table
  signal s_note_array     : t_note_array;
  signal s_vel_array      : t_vel_array;
  signal s_note_active    : std_logic_vector(MAX_POLYPHONY - 1 downto 0);

  signal s_is_active      : std_logic;
  signal s_note_index     : natural range 0 to MAX_POLYPHONY - 1;

  signal s_first_empty    : natural range 0 to MAX_POLYPHONY - 1;

  signal s_poly_counter   : natural range 0 to MAX_POLYPHONY;
  signal s_max_polyphony  : natural range 0 to MAX_POLYPHONY;

  -- sample index counters
  signal s_idx_cnt_en     : std_logic_vector(MAX_POLYPHONY - 1 downto 0);
  signal s_idx_cnt_tc     : std_logic_vector(MAX_POLYPHONY - 1 downto 0);
  signal s_idx_cnt_end    : std_logic_vector(g_smp_mem_size - 1 downto 0);
  signal s_idx_cnt        : t_sound_table;

  signal s_sample_inc     : t_sample_inc := init_sample_inc;

begin
  -- output signal assignments
  o_sample_en       <= s_note_active;
  o_sample_idx      <= s_idx_cnt;

  -- internal signal assignments
  s_max_polyphony   <= MAX_POLYPHONY when i_poly = '1' else 1;

  s_idx_cnt_end     <= (others => '1');

  -- processes
  p_first_row_empty: process(s_note_active)
  begin
    s_first_empty <= 0;
    for i in MAX_POLYPHONY - 1 downto 0 loop
      if s_note_active(i) = '0' then
        s_first_empty <= i;
      end if;
    end loop;
  end process;

  p_note_check: process(s_note_active, i_note, s_note_array)
  begin
    s_is_active <= '0';
    for i in 0 to MAX_POLYPHONY - 1 loop
      if (s_note_array(i) = i_note) and (s_note_active(i) = '1') then
        s_is_active <= '1';
      end if;
    end loop;
  end process;

  p_note_index: process(s_note_active, i_note, s_note_array)
  begin
    s_note_index <= 0;
    for i in 0 to MAX_POLYPHONY - 1 loop
      if s_note_array(i) = i_note then
        s_note_index <= i;
      end if;
    end loop;
  end process;

  p_sound_table: process(i_reset_n, i_clk, i_sound_on)
  begin
    if i_reset_n = '0' or i_sound_on = '0' then
      s_note_array  <= (others => (others => '0'));
      s_vel_array   <= (others => (others => '0'));
      s_note_active <= (others => '0');

      s_poly_counter <= 0;

    elsif i_clk'event and i_clk = '1' then
      if i_start = '1' then
        if s_poly_counter < s_max_polyphony then -- ignore if max polyphony has been reached
          if s_is_active = '0' then -- avoid doubling active notes
            s_note_array(s_first_empty)   <= i_note;
            s_vel_array(s_first_empty)    <= i_vel;
            s_note_active(s_first_empty)  <= '1';
            s_poly_counter <= s_poly_counter + 1;
          end if;
        end if;
      elsif i_stop = '1' then
        if s_is_active = '1' then -- avoid stopping inactive note
          s_note_array(s_note_index)    <= (others => '0');
          s_vel_array(s_note_index)     <= (others => '0');
          s_note_active(s_note_index)   <= '0';
          s_poly_counter <= s_poly_counter - 1;
        end if;
      end if;
    end if;
  end process;

  p_cnt_enable: process(s_note_active)
  begin
    for i in 0 to MAX_POLYPHONY - 1 loop
      s_idx_cnt_en(i) <= s_note_active(i);
    end loop;
  end process;

  p_sample_idx: process(i_reset_n, i_sample_clk, s_note_array, s_sample_inc)
  begin
    for i in 0 to MAX_POLYPHONY - 1 loop
      if i_reset_n = '0' then
        s_idx_cnt(i) <= (others => '0');
      elsif i_sample_clk'event and i_sample_clk = '1' then
        if s_idx_cnt_en(i) = '1' then
          if s_idx_cnt(i) = s_idx_cnt_end then
            s_idx_cnt(i) <= (others => '0');
          else
            s_idx_cnt(i) <= std_logic_vector(unsigned(s_idx_cnt(i)) + s_sample_inc(to_integer(unsigned(s_note_array(i)))));
          end if;
        end if;
      end if;
    end loop;
  end process;

  p_sample_idx_tc: process(s_idx_cnt, s_idx_cnt_end)
  begin
    for i in 0 to MAX_POLYPHONY - 1 loop
      if s_idx_cnt(i) = s_idx_cnt_end then
        s_idx_cnt_tc(i) <= '1';
      else
        s_idx_cnt_tc(i) <= '0';
      end if;
    end loop;
  end process;

end architecture;
