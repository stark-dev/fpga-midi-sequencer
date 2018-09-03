library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity DISPLAY_IF is
port (
  i_display_value   : in  t_display_out;
  o_display_array   : out std_logic_vector(6 downto 0)
);
end entity;

architecture BHV of DISPLAY_IF is

begin

  process(i_display_value)
  begin
    case i_display_value is
      when ds_A     =>
        o_display_array <= "0001000";
      when ds_B     =>
        o_display_array <= "0000011";
      when ds_C     =>
        o_display_array <= "1000110";
      when ds_D     =>
        o_display_array <= "0100001";
      when ds_E     =>
        o_display_array <= "0000110";
      when ds_F     =>
        o_display_array <= "0001110";
      when ds_G     =>
        o_display_array <= "0010000";
      when ds_H     =>
        o_display_array <= "0001001";
      when ds_I     =>
        o_display_array <= "1001111";
      when ds_J     =>
        o_display_array <= "1100001";
      when ds_K     =>
        o_display_array <= "0001001";
      when ds_L     =>
        o_display_array <= "1000111";
      when ds_M     =>
        o_display_array <= "1001000";
      when ds_N     =>
        o_display_array <= "0101011";
      when ds_O     =>
        o_display_array <= "1000000";
      when ds_P     =>
        o_display_array <= "0001100";
      when ds_Q     =>
        o_display_array <= "0011000";
      when ds_R     =>
        o_display_array <= "0101111";
      when ds_S     =>
        o_display_array <= "0010010";
      when ds_T     =>
        o_display_array <= "0000111";
      when ds_U     =>
        o_display_array <= "1000001";
      when ds_V     =>
        o_display_array <= "1100011";
      when ds_W     =>
        o_display_array <= "1010101";
      when ds_X     =>
        o_display_array <= "0001001";
      when ds_Y     =>
        o_display_array <= "0010001";
      when ds_Z     =>
        o_display_array <= "0100100";
      when ds_0     =>
        o_display_array <= "1000000";
      when ds_1     =>
        o_display_array <= "1111001";
      when ds_2     =>
        o_display_array <= "0100100";
      when ds_3     =>
        o_display_array <= "0110000";
      when ds_4     =>
        o_display_array <= "0011001";
      when ds_5     =>
        o_display_array <= "0010010";
      when ds_6     =>
        o_display_array <= "0000010";
      when ds_7     =>
        o_display_array <= "1111000";
      when ds_8     =>
        o_display_array <= "0000000";
      when ds_9     =>
        o_display_array <= "0010000";
      when ds_OFF   =>
        o_display_array <= "1111111";
      when others   =>
        o_display_array <= "1111111";
    end case;
  end process;

end architecture;
