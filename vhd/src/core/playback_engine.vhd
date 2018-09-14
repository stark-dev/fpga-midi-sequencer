library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity PLAYBACK_ENGINE is
port (
  i_clk         : in  std_logic;
  i_reset_n     : in  std_logic;
  i_state       : in  t_core_fsm;
  i_evt_ready   : in  std_logic;
  i_evt_data    : in  std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);
  o_sg_note     : out std_logic_vector(MIDI_DATA_SIZE - 1 downto 0);
  o_sg_vel      : out std_logic_vector(MIDI_DATA_SIZE - 1 downto 0);
  o_sg_start    : out std_logic;
  o_sg_stop     : out std_logic
);
end entity;

architecture BHV of PLAYBACK_ENGINE is

  type t_sound_gen_fsm  is (st_reset, st_ready, st_data);

  signal s_sg_fsm       : t_sound_gen_fsm;

begin

  o_sg_note      <= i_evt_data(SEQ_DATA1_RANGE);
  o_sg_vel       <= i_evt_data(SEQ_DATA2_RANGE);

  p_sound_gen_fsm: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_sg_fsm <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_sg_fsm is
        when st_reset   =>
          s_sg_fsm <= st_ready;
        when st_ready   =>
          if i_evt_ready = '1' then
            s_sg_fsm <= st_data;
          else
            s_sg_fsm <= st_ready;
          end if;
        when st_data  =>
          s_sg_fsm <= st_ready;
      end case;
    end if;
  end process;

  p_sound_ctrl: process(s_sg_fsm, i_evt_data)
  begin
    case s_sg_fsm is
      when st_reset   =>
        o_sg_start <= '0';
        o_sg_stop  <= '1';

      when st_ready   =>
        o_sg_start <= '0';
        o_sg_stop  <= '0';

      when st_data    =>
        case i_evt_data(SEQ_TYPE_RANGE) is
          when "000"  => -- note off
            o_sg_start <= '0';
            o_sg_stop  <= '1';
          when "001"  => -- note on
            o_sg_start  <= '1';
            o_sg_stop   <= '0';
          when others => -- nothing
            o_sg_start <= '0';
            o_sg_stop  <= '0';
        end case;

      when others     =>
        o_sg_start <= '0';
        o_sg_stop  <= '1';
    end case;
  end process;

end architecture;
