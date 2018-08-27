library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity DISPLAY_MUX_3 is
port (
  i_enable  : in  std_logic;
  i_input   : in  std_logic_vector(2 downto 0);
  o_display : out t_display_out
);
end entity;

architecture BHV of DISPLAY_MUX_3 is

begin

  p_display_mux_3: process(i_input, i_enable)
  begin
    if i_enable = '0' then
      o_display  <= ds_OFF;
    else
      case i_input is
        when "000" =>
          o_display  <= ds_0;
        when "001" =>
          o_display  <= ds_1;
        when "010" =>
          o_display  <= ds_2;
        when "011" =>
          o_display  <= ds_3;
        when "100" =>
          o_display  <= ds_4;
        when "101" =>
          o_display  <= ds_5;
        when "110" =>
          o_display  <= ds_6;
        when "111" =>
          o_display  <= ds_7;
        when others =>
          o_display  <= ds_OFF;
      end case;
    end if;
  end process;

end architecture;
