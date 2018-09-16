library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity module is
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
end entity;

architecture bhv of module is

  component UP_COUNTER_MOD is
    generic (
      N           : integer
       := 16
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

  signal s_duty_cycle   : unsigned(12 downto 0);

  signal s_pwm          : std_logic;

  signal s_cnt_out      : unsigned(12 downto 0);
  signal s_tc_value     : unsigned(12 downto 0);
  signal s_cnt_tc       : std_logic;

begin

  o_pwm     <= s_pwm and i_start_stop_n;
  o_pwm_n   <= (not s_pwm) and i_start_stop_n;

  p_divisor: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_tc_value <= to_unsigned(5000, 13); -- at reset frequency is 10 KHz
    elsif i_clk'event and i_clk = '1' then
      if i_start_stop_n = '0' and i_load_div = '1' then   -- frequency may be changed only if pwm is stopped
        s_tc_value <= unsigned(i_div);
      end if;
    end if;
  end process;

  p_duty_cycle: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_duty_cycle <= to_unsigned(2500, 13); -- at reset duty cycle is 50%
    elsif i_clk'event and i_clk = '1' then
      if i_load_dc = '1' then   -- duty cycle may be changed at any time
        s_duty_cycle <= unsigned(i_dc);
      end if;
    end if;
  end process;

  p_pwm: process(s_cnt_out, s_duty_cycle)
  begin
    if s_cnt_out < s_duty_cycle then
      s_pwm <= '1';
    else
      s_pwm <= '0';
    end if;
  end process;

  PWM_CNT :  UP_COUNTER_MOD
  generic map ( 13 )
  port map (
    i_clk       => i_clk,
    i_reset_n   => i_start_stop_n,
    i_en        => i_start_stop_n,
    i_tc_value  => s_tc_value,
    o_cnt_out   => s_cnt_out,
    o_chain_en  => s_cnt_tc
  );

end architecture;
