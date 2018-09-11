library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity module is
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;
  i_load_mask     : in  std_logic;
  i_load_leds     : in  std_logic;
  i_mask          : in  std_logic_vector(7 downto 0);
  i_leds          : in  std_logic_vector(7 downto 0);
  o_mask          : out std_logic_vector(7 downto 0);
  o_leds          : out std_logic_vector(7 downto 0)
);
end entity;

architecture bhv of module is
  signal s_mask   : std_logic_vector(7 downto 0);
  signal s_leds   : std_logic_vector(7 downto 0);

begin

  o_leds <= s_leds and s_mask;
  o_mask <= s_mask;

  p_control: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_mask <= "00000000";
      s_leds <= "00000000";
    elsif i_clk'event and i_clk = '1' then
      if i_load_mask = '1' then
        s_mask <= i_mask;
      elsif i_load_leds = '1' then
        s_leds <= i_leds;
      end if;
    end if;
  end process;

end architecture;
