library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_CORE is
end TB_CORE;

architecture TEST of TB_CORE is

  component SEQUENCER_CORE is
  port (
    -- system inputs
    i_clk           : in  std_logic;
    i_reset_n       : in  std_logic;

    i_btn_left      : in  std_logic;
    i_btn_up        : in  std_logic;
    i_btn_down      : in  std_logic;
    i_btn_right     : in  std_logic;

    i_tr_mute       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);
    i_tr_solo       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);

    o_display_a     : out t_display_array
  );
  end component;

  constant c_ext_clock    : integer := 50000000;
  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_clock_half_p : time := c_clock * 1 ns;

  signal s_clk      : std_logic;
  signal s_rst      : std_logic;
  signal s_btn1     : std_logic;
  signal s_btn2     : std_logic;
  signal s_btn3     : std_logic;
  signal s_btn4     : std_logic;

  signal s_tr_mute  : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_tr_solo  : std_logic_vector(SEQ_TRACKS - 1 downto 0);

  signal s_display  : t_display_array;

begin
  DUT : SEQUENCER_CORE
  port map(s_clk, s_rst, s_btn1, s_btn2, s_btn3, s_btn4, s_tr_mute, s_tr_solo, s_display);

  clock_gen : process
  begin
    s_clk <= '0';
    wait for c_clock_half_p;
    s_clk <= '1';
    wait for c_clock_half_p;
  end process;

  input_gen: process
  begin
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    --reset
    s_rst     <= '0';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- very short press (not detected)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- play
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- pause
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 500 ns;

    -- play
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- vol up
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- vol down
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 100 ns;

    -- pause
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- restart
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 320 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- menu
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 320 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (patch)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- inc patch
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '0';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (track vol)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (pan)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle menu (poly)
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '0';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- toggle poly
    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '0';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- exit menu
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    -- play
    s_rst     <= '1';
    s_btn1    <= '0';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 120 * c_clock_half_p;

    s_rst     <= '1';
    s_btn1    <= '1';
    s_btn2    <= '1';
    s_btn3    <= '1';
    s_btn4    <= '1';
    s_tr_mute <= (others => '0');
    s_tr_solo <= (others => '0');
    wait for 1000 ns;

    wait;
  end process;

end TEST;
