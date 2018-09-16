library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_IO_PERIPHERAL is
end TB_IO_PERIPHERAL;

architecture TEST of TB_IO_PERIPHERAL is

  component module is
  port (
    i_clk           : in  std_logic;
    i_reset_n       : in  std_logic;
    i_load_div      : in  std_logic;  -- load divisor
    i_load_dc       : in  std_logic;  -- load duty cycle
    i_start_stop_n  : in  std_logic;  -- start pwm (0 disables outputs)
    i_div           : in  std_logic_vector(12 downto 0);
    i_dc            : in  std_logic_vector(12 downto 0);
    o_pwm           : out std_logic;
    o_pwm_n         : out std_logic
  );
end component;

  constant c_ext_clock    : integer := 50000000;
  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_clock_half_p : time := c_clock * 1 ns;

  signal s_clk              : std_logic;
  signal s_rst              : std_logic;
  signal s_start_stop_n     : std_logic;
  signal s_load_div         : std_logic;
  signal s_load_dc          : std_logic;
  signal s_div_in           : std_logic_vector(12 downto 0);
  signal s_dc_in            : std_logic_vector(12 downto 0);
  signal s_pwm              : std_logic;
  signal s_pwm_n            : std_logic;

begin

  DUT : module
  port map(
    s_clk,
    s_rst,
    s_load_div,
    s_load_dc,
    s_start_stop_n,
    s_div_in,
    s_dc_in,
    s_pwm,
    s_pwm_n
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
    s_load_div      <= '0';
    s_load_dc       <= '0';
    s_start_stop_n  <= '0';
    s_div_in        <= (others => '0');
    s_dc_in         <= (others => '0');
    wait for 10 ns;

    s_rst           <= '0';
    s_load_div      <= '0';
    s_load_dc       <= '0';
    s_start_stop_n  <= '0';
    s_div_in        <= (others => '0');
    s_dc_in         <= (others => '0');
    wait for 10 ns;

    s_rst           <= '1';
    s_load_div      <= '0';
    s_load_dc       <= '0';
    s_start_stop_n  <= '0';
    s_div_in        <= (others => '0');
    s_dc_in         <= (others => '0');
    wait for 100 ns;

    -- test div load
    s_rst           <= '1';
    s_load_div      <= '1';
    s_load_dc       <= '0';
    s_start_stop_n  <= '0';
    s_div_in        <= (others => '1');
    s_dc_in         <= (others => '0');
    wait for 20 ns;

    -- test dc load
    s_rst           <= '1';
    s_load_div      <= '0';
    s_load_dc       <= '1';
    s_start_stop_n  <= '0';
    s_div_in        <= (others => '0');
    s_dc_in         <= std_logic_vector(to_unsigned(2500, 13));
    wait for 20 ns;

    -- start pwm
    s_rst           <= '1';
    s_load_div      <= '0';
    s_load_dc       <= '0';
    s_start_stop_n  <= '1';
    s_div_in        <= (others => '0');
    s_dc_in         <= std_logic_vector(to_unsigned(2500, 13));
    wait for 200 ns;

    -- change div while on (should be avoided)
    s_rst           <= '1';
    s_load_div      <= '1';
    s_load_dc       <= '0';
    s_start_stop_n  <= '1';
    s_div_in        <= (others => '0');
    s_dc_in         <= std_logic_vector(to_unsigned(2500, 13));
    wait for 20 ns;

    -- change duty cycle while on (ok)
    s_rst           <= '1';
    s_load_div      <= '0';
    s_load_dc       <= '1';
    s_start_stop_n  <= '1';
    s_div_in        <= (others => '0');
    s_dc_in         <= std_logic_vector(to_unsigned(3500, 13));
    wait for 20 ns;

    -- run
    s_rst           <= '1';
    s_load_div      <= '0';
    s_load_dc       <= '0';
    s_start_stop_n  <= '1';
    s_div_in        <= (others => '0');
    s_dc_in         <= std_logic_vector(to_unsigned(3500, 13));
    wait for 200 ns;

    -- stop
    s_rst           <= '1';
    s_load_div      <= '0';
    s_load_dc       <= '0';
    s_start_stop_n  <= '0';
    s_div_in        <= (others => '0');
    s_dc_in         <= std_logic_vector(to_unsigned(3500, 13));
    wait for 20 ns;

    -- change div
    s_rst           <= '1';
    s_load_div      <= '1';
    s_load_dc       <= '0';
    s_start_stop_n  <= '0';
    s_div_in        <= std_logic_vector(to_unsigned(4000, 13));
    s_dc_in         <= std_logic_vector(to_unsigned(3500, 13));
    wait for 20 ns;

    -- start
    s_rst           <= '1';
    s_load_div      <= '0';
    s_load_dc       <= '0';
    s_start_stop_n  <= '1';
    s_div_in        <= std_logic_vector(to_unsigned(4000, 13));
    s_dc_in         <= std_logic_vector(to_unsigned(3500, 13));
    wait for 200 ns;

    wait;
  end process;

end TEST;
