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
      v_tmp(0)   := to_unsigned(12   , g_smp_mem_size);
      v_tmp(1)   := to_unsigned(13   , g_smp_mem_size);
      v_tmp(2)   := to_unsigned(14   , g_smp_mem_size);
      v_tmp(3)   := to_unsigned(14   , g_smp_mem_size);
      v_tmp(4)   := to_unsigned(15   , g_smp_mem_size);
      v_tmp(5)   := to_unsigned(16   , g_smp_mem_size);
      v_tmp(6)   := to_unsigned(17   , g_smp_mem_size);
      v_tmp(7)   := to_unsigned(18   , g_smp_mem_size);
      v_tmp(8)   := to_unsigned(19   , g_smp_mem_size);
      v_tmp(9)   := to_unsigned(20   , g_smp_mem_size);
      v_tmp(10)  := to_unsigned(22   , g_smp_mem_size);
      v_tmp(11)  := to_unsigned(23   , g_smp_mem_size);
      v_tmp(12)  := to_unsigned(24   , g_smp_mem_size);
      v_tmp(13)  := to_unsigned(26   , g_smp_mem_size);
      v_tmp(14)  := to_unsigned(27   , g_smp_mem_size);
      v_tmp(15)  := to_unsigned(29   , g_smp_mem_size);
      v_tmp(16)  := to_unsigned(31   , g_smp_mem_size);
      v_tmp(17)  := to_unsigned(32   , g_smp_mem_size);
      v_tmp(18)  := to_unsigned(34   , g_smp_mem_size);
      v_tmp(19)  := to_unsigned(36   , g_smp_mem_size);
      v_tmp(20)  := to_unsigned(39   , g_smp_mem_size);
      v_tmp(21)  := to_unsigned(41   , g_smp_mem_size);
      v_tmp(22)  := to_unsigned(43   , g_smp_mem_size);
      v_tmp(23)  := to_unsigned(46   , g_smp_mem_size);
      v_tmp(24)  := to_unsigned(49   , g_smp_mem_size);
      v_tmp(25)  := to_unsigned(51   , g_smp_mem_size);
      v_tmp(26)  := to_unsigned(55   , g_smp_mem_size);
      v_tmp(27)  := to_unsigned(58   , g_smp_mem_size);
      v_tmp(28)  := to_unsigned(61   , g_smp_mem_size);
      v_tmp(29)  := to_unsigned(65   , g_smp_mem_size);
      v_tmp(30)  := to_unsigned(69   , g_smp_mem_size);
      v_tmp(31)  := to_unsigned(73   , g_smp_mem_size);
      v_tmp(32)  := to_unsigned(77   , g_smp_mem_size);
      v_tmp(33)  := to_unsigned(82   , g_smp_mem_size);
      v_tmp(34)  := to_unsigned(87   , g_smp_mem_size);
      v_tmp(35)  := to_unsigned(92   , g_smp_mem_size);
      v_tmp(36)  := to_unsigned(97   , g_smp_mem_size);
      v_tmp(37)  := to_unsigned(103  , g_smp_mem_size);
      v_tmp(38)  := to_unsigned(109  , g_smp_mem_size);
      v_tmp(39)  := to_unsigned(116  , g_smp_mem_size);
      v_tmp(40)  := to_unsigned(122  , g_smp_mem_size);
      v_tmp(41)  := to_unsigned(130  , g_smp_mem_size);
      v_tmp(42)  := to_unsigned(137  , g_smp_mem_size);
      v_tmp(43)  := to_unsigned(146  , g_smp_mem_size);
      v_tmp(44)  := to_unsigned(154  , g_smp_mem_size);
      v_tmp(45)  := to_unsigned(163  , g_smp_mem_size);
      v_tmp(46)  := to_unsigned(173  , g_smp_mem_size);
      v_tmp(47)  := to_unsigned(183  , g_smp_mem_size);
      v_tmp(48)  := to_unsigned(194  , g_smp_mem_size);
      v_tmp(49)  := to_unsigned(206  , g_smp_mem_size);
      v_tmp(50)  := to_unsigned(218  , g_smp_mem_size);
      v_tmp(51)  := to_unsigned(231  , g_smp_mem_size);
      v_tmp(52)  := to_unsigned(245  , g_smp_mem_size);
      v_tmp(53)  := to_unsigned(259  , g_smp_mem_size);
      v_tmp(54)  := to_unsigned(275  , g_smp_mem_size);
      v_tmp(55)  := to_unsigned(291  , g_smp_mem_size);
      v_tmp(56)  := to_unsigned(309  , g_smp_mem_size);
      v_tmp(57)  := to_unsigned(327  , g_smp_mem_size);
      v_tmp(58)  := to_unsigned(346  , g_smp_mem_size);
      v_tmp(59)  := to_unsigned(367  , g_smp_mem_size);
      v_tmp(60)  := to_unsigned(389  , g_smp_mem_size);
      v_tmp(61)  := to_unsigned(412  , g_smp_mem_size);
      v_tmp(62)  := to_unsigned(436  , g_smp_mem_size);
      v_tmp(63)  := to_unsigned(462  , g_smp_mem_size);
      v_tmp(64)  := to_unsigned(490  , g_smp_mem_size);
      v_tmp(65)  := to_unsigned(519  , g_smp_mem_size);
      v_tmp(66)  := to_unsigned(550  , g_smp_mem_size);
      v_tmp(67)  := to_unsigned(583  , g_smp_mem_size);
      v_tmp(68)  := to_unsigned(617  , g_smp_mem_size);
      v_tmp(69)  := to_unsigned(654  , g_smp_mem_size);
      v_tmp(70)  := to_unsigned(693  , g_smp_mem_size);
      v_tmp(71)  := to_unsigned(734  , g_smp_mem_size);
      v_tmp(72)  := to_unsigned(778  , g_smp_mem_size);
      v_tmp(73)  := to_unsigned(824  , g_smp_mem_size);
      v_tmp(74)  := to_unsigned(873  , g_smp_mem_size);
      v_tmp(75)  := to_unsigned(925  , g_smp_mem_size);
      v_tmp(76)  := to_unsigned(980  , g_smp_mem_size);
      v_tmp(77)  := to_unsigned(1038 , g_smp_mem_size);
      v_tmp(78)  := to_unsigned(1100 , g_smp_mem_size);
      v_tmp(79)  := to_unsigned(1165 , g_smp_mem_size);
      v_tmp(80)  := to_unsigned(1234 , g_smp_mem_size);
      v_tmp(81)  := to_unsigned(1308 , g_smp_mem_size);
      v_tmp(82)  := to_unsigned(1386 , g_smp_mem_size);
      v_tmp(83)  := to_unsigned(1468 , g_smp_mem_size);
      v_tmp(84)  := to_unsigned(1555 , g_smp_mem_size);
      v_tmp(85)  := to_unsigned(1648 , g_smp_mem_size);
      v_tmp(86)  := to_unsigned(1746 , g_smp_mem_size);
      v_tmp(87)  := to_unsigned(1849 , g_smp_mem_size);
      v_tmp(88)  := to_unsigned(1959 , g_smp_mem_size);
      v_tmp(89)  := to_unsigned(2076 , g_smp_mem_size);
      v_tmp(90)  := to_unsigned(2199 , g_smp_mem_size);
      v_tmp(91)  := to_unsigned(2330 , g_smp_mem_size);
      v_tmp(92)  := to_unsigned(2469 , g_smp_mem_size);
      v_tmp(93)  := to_unsigned(2615 , g_smp_mem_size);
      v_tmp(94)  := to_unsigned(2771 , g_smp_mem_size);
      v_tmp(95)  := to_unsigned(2936 , g_smp_mem_size);
      v_tmp(96)  := to_unsigned(3110 , g_smp_mem_size);
      v_tmp(97)  := to_unsigned(3295 , g_smp_mem_size);
      v_tmp(98)  := to_unsigned(3491 , g_smp_mem_size);
      v_tmp(99)  := to_unsigned(3699 , g_smp_mem_size);
      v_tmp(100) := to_unsigned(3919 , g_smp_mem_size);
      v_tmp(101) := to_unsigned(4152 , g_smp_mem_size);
      v_tmp(102) := to_unsigned(4399 , g_smp_mem_size);
      v_tmp(103) := to_unsigned(4660 , g_smp_mem_size);
      v_tmp(104) := to_unsigned(4937 , g_smp_mem_size);
      v_tmp(105) := to_unsigned(5231 , g_smp_mem_size);
      v_tmp(106) := to_unsigned(5542 , g_smp_mem_size);
      v_tmp(107) := to_unsigned(5872 , g_smp_mem_size);
      v_tmp(108) := to_unsigned(6221 , g_smp_mem_size);
      v_tmp(109) := to_unsigned(6591 , g_smp_mem_size);
      v_tmp(110) := to_unsigned(6983 , g_smp_mem_size);
      v_tmp(111) := to_unsigned(7398 , g_smp_mem_size);
      v_tmp(112) := to_unsigned(7838 , g_smp_mem_size);
      v_tmp(113) := to_unsigned(8304 , g_smp_mem_size);
      v_tmp(114) := to_unsigned(8797 , g_smp_mem_size);
      v_tmp(115) := to_unsigned(9321 , g_smp_mem_size);
      v_tmp(116) := to_unsigned(9875 , g_smp_mem_size);
      v_tmp(117) := to_unsigned(10462, g_smp_mem_size);
      v_tmp(118) := to_unsigned(11084, g_smp_mem_size);
      v_tmp(119) := to_unsigned(11743, g_smp_mem_size);
      v_tmp(120) := to_unsigned(12441, g_smp_mem_size);
      v_tmp(121) := to_unsigned(13181, g_smp_mem_size);
      v_tmp(122) := to_unsigned(13965, g_smp_mem_size);
      v_tmp(123) := to_unsigned(14795, g_smp_mem_size);
      v_tmp(124) := to_unsigned(15675, g_smp_mem_size);
      v_tmp(125) := to_unsigned(16607, g_smp_mem_size);
      v_tmp(126) := to_unsigned(17595, g_smp_mem_size);
      v_tmp(127) := to_unsigned(18641, g_smp_mem_size);
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
