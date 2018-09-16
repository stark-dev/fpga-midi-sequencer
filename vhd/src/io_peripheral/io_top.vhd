library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity io_top is
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;
  i_address       : in  std_logic_vector(1 downto 0);
  i_chip_select   : in  std_logic;
  i_write         : in  std_logic;
  i_writedata     : in  std_logic_vector(31 downto 0);
  -- o_readdata      : out std_logic_vector(31 downto 0);
  o_pwm           : out std_logic_vector(1 downto 0)
);
end entity;

architecture bhv of io_top is

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

-- write enable from avalon
signal s_wr_en        : std_logic;

-- if regs load
signal s_load_div     : std_logic;
signal s_load_dc      : std_logic;
signal s_start_stop_n : std_logic;

begin
  -- write signal
  s_wr_en           <= '1' when (i_chip_select = '1' and i_write = '1') else '0';
  -- load signals
  s_load_div        <= '1' when (s_wr_en = '1' and i_address = "01") else '0';
  s_load_dc         <= '1' when (s_wr_en = '1' and i_address = "10") else '0';

  p_start_stop_reg: process(i_reset_n, i_clk, i_writedata)
  begin
    if i_reset_n = '0' then
      s_start_stop_n <= '0';
    elsif i_clk'event and i_clk = '1' then
      if s_wr_en = '1' and i_address = "00" then
        s_start_stop_n <= i_writedata(0);
      end if;
    end if;
  end process;

  pwm_controller: module
  port map (
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,
    i_load_div      => s_load_div,
    i_load_dc       => s_load_dc,
    i_start_stop_n  => s_start_stop_n,
    i_div           => i_writedata(12 downto 0),
    i_dc            => i_writedata(12 downto 0),
    o_pwm           => o_pwm(0),
    o_pwm_n         => o_pwm(1)
  );

  end architecture;
