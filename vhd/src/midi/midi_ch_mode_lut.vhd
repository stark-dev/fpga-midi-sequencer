library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity MIDI_CH_MODE_LUT is
port (
  i_ch_mode     : in  std_logic_vector(6 downto 0);
  o_ch_mode     : out t_ch_mode
  );
end entity;

architecture BHV of MIDI_CH_MODE_LUT is

begin

  p_ch_mode: process(i_ch_mode)
  begin
    case i_ch_mode is
      when "1111000" =>  -- 120
        o_ch_mode <= ctrl_all_sn_off;
      when "1111001" =>  -- 121
        o_ch_mode <= ctrl_rst;
      when "1111011" =>  -- 123
        o_ch_mode <= ctrl_all_nt_off;
      when "1111100" =>  -- 124
        o_ch_mode <= ctrl_omni_on;
      when "1111101" =>  -- 125
        o_ch_mode <= ctrl_omni_off;
      when "1111110" =>  -- 126
        o_ch_mode <= ctrl_mono_on;
      when "1111111" =>  -- 127
        o_ch_mode <= ctrl_poly_on;
      when others =>
        o_ch_mode <= ctrl_unknown;
      end case;
  end process;

end architecture;
