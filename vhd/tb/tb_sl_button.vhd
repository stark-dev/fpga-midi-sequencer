library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_SL_BUTTON is
end TB_SL_BUTTON;

architecture TEST of TB_SL_BUTTON is

  component SL_BUTTON is
  generic (
    g_size      : integer := 12;
    g_short     : integer := 500;
    g_long      : integer := 1000
  );
  port (
    i_clk         : in  std_logic;
    i_reset_n     : in  std_logic;
    i_btn         : in  std_logic;
    o_long        : out std_logic;
    o_short       : out std_logic
  );
  end component;

  constant c_ext_clock    : integer := 50000000;
  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_clock_half_p : time := c_clock * 1 ns;

  signal s_clk    : std_logic;
  signal s_rst    : std_logic;
  signal s_btn    : std_logic;
  signal s_long   : std_logic;
  signal s_short  : std_logic;

begin

  DUT : SL_BUTTON
  generic map(8, 60, 150)
  port map(s_clk, s_rst, s_btn, s_long, s_short);

  clock_gen : process
  begin
    s_clk <= '0';
    wait for c_clock_half_p;
    s_clk <= '1';
    wait for c_clock_half_p;
  end process;

  input_gen: process
  begin
    s_rst  <= '1';
    s_btn  <= '1';
    wait for 100 ns;

    s_rst  <= '0';
    s_btn  <= '1';
    wait for 100 ns;

    s_rst  <= '1';
    s_btn  <= '1';
    wait for 100 ns;

    -- very short press
    s_rst  <= '1';
    s_btn  <= '0';
    wait for 100 ns;

    s_rst  <= '1';
    s_btn  <= '1';
    wait for 100 ns;

    -- short press
    s_rst  <= '1';
    s_btn  <= '0';
    wait for 140 * c_clock_half_p;

    s_rst  <= '1';
    s_btn  <= '1';
    wait for 100 ns;

    -- long press
    s_rst  <= '1';
    s_btn  <= '0';
    wait for 320 * c_clock_half_p;

    s_rst  <= '1';
    s_btn  <= '1';
    wait for 100 ns;

    -- very long press
    s_rst  <= '1';
    s_btn  <= '0';
    wait for 600 * c_clock_half_p;

    s_rst  <= '1';
    s_btn  <= '1';
    wait for 100 ns;

    wait;
  end process;

end TEST;
