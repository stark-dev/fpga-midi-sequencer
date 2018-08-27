library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity DISPLAY_MUX_4 is
port (
  i_enable  : in  std_logic;
  i_input   : in  std_logic_vector(3 downto 0);
  o_display : out t_display_out
);
end entity;

architecture BHV of DISPLAY_MUX_4 is

begin

  p_display_mux_4: process(i_input, i_enable)
  begin
    if i_enable = '0' then
      o_display  <= ds_OFF;
    else
      case i_input is
        when "0000" =>
          o_display  <= ds_0;
        when "0001" =>
          o_display  <= ds_1;
        when "0010" =>
          o_display  <= ds_2;
        when "0011" =>
          o_display  <= ds_3;
        when "0100" =>
          o_display  <= ds_4;
        when "0101" =>
          o_display  <= ds_5;
        when "0110" =>
          o_display  <= ds_6;
        when "0111" =>
          o_display  <= ds_7;
        when "1000" =>
          o_display  <= ds_8;
        when "1001" =>
          o_display  <= ds_9;
        when "1010" =>
          o_display  <= ds_A;
        when "1011" =>
          o_display  <= ds_B;
        when "1100" =>
          o_display  <= ds_C;
        when "1101" =>
          o_display  <= ds_D;
        when "1110" =>
          o_display  <= ds_E;
        when "1111" =>
          o_display  <= ds_F;
        when others =>
          o_display  <= ds_OFF;
      end case;
    end if;
  end process;

end architecture;
