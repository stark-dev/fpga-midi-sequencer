library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity SEQUENCER_FSM is
port (
  i_clk         : in   std_logic;
  i_reset_n     : in   std_logic);
end entity;

architecture BHV of SEQUENCER_FSM is

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  type t_seq_st is (st_start, st_play, st_rec, st_err);

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------

signal s_state : t_seq_st;

begin

  p_fsm_state: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_state <= st_start;
    elsif i_clk'event and i_clk = '1' then
      case s_state is
        when st_start =>
          s_state <= st_play;
        when others   =>
          s_state <= st_play;
        end case;
    end if;
  end process;

end architecture;
