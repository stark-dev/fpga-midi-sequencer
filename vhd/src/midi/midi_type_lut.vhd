library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity MIDI_TYPE_LUT is
port (
  i_midi_cmd    : in  t_midi_cmd;
  o_type        : out std_logic_vector(MIDI_TYPE_SIZE - 1 downto 0)
  );
end entity;

architecture BHV of MIDI_TYPE_LUT is

begin

  p_midi_cmd: process(i_midi_cmd)
  begin
    case i_midi_cmd is
      when midi_note_off =>
        o_type <= "000";
      when midi_note_on =>
        o_type <= "001";
      when midi_ctrl_ch =>
        o_type <= "010";
      when midi_prg_ch =>
        o_type <= "011";
      when others =>
        o_type <= "111";
      end case;
  end process;

end architecture;
