library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity MIDI_CMD_LUT is
port (
  i_midi_cmd    : in  std_logic_vector(3 downto 0);
  o_midi_cmd    : out t_midi_cmd
  );
end entity;

architecture BHV of MIDI_CMD_LUT is

begin

  p_midi_cmd: process(i_midi_cmd)
  begin
    case i_midi_cmd is
      when "1000" =>
        o_midi_cmd <= midi_note_off;
      when "1001" =>
        o_midi_cmd <= midi_note_on;
      when "1011" =>
        o_midi_cmd <= midi_ctrl_ch;
      when "1100" =>
        o_midi_cmd <= midi_prg_ch;
      when others =>
        o_midi_cmd <= midi_unknown;
      end case;
  end process;

end architecture;
