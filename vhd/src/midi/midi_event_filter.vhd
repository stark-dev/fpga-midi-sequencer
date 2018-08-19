library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity MIDI_EVT_FILTER is
port (
  i_clk         : in  std_logic;
  i_reset_n     : in  std_logic;
  i_new_data    : in  std_logic;
  i_data_in     : in  std_logic_vector(7 downto 0);

  o_midi_msg    : out std_logic_vector(31 downto 0);
  o_midi_ready  : out std_logic
  );
end entity;

architecture BHV of MIDI_EVT_FILTER is

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  type t_data_fsm is (st_idle, st_sample, st_data);
  type t_midi_fsm is (st_reset, st_status, st_data1, st_data2);

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- fsm
  signal s_fsm_state : t_data_fsm;  -- main fsm
  signal s_msg_state : t_midi_fsm;  -- message fsm

  -- message data register
  signal s_data_en      : std_logic;
  signal s_data_rst     : std_logic;
  signal s_data_out     : std_logic_vector(7 downto 0);

  -- midi command register
  signal s_midi_cmd_r   : t_midi_cmd;
  signal s_midi_cmd_in  : t_midi_cmd;
  signal s_midi_cmd_en  : std_logic;
  signal s_midi_cmd_rst : std_logic;

  -- ch mode lut out
  signal s_ch_mode_lut  : t_ch_mode;

  -- message fields
  signal s_midi_cmd     : std_logic_vector(3 downto 0);
  signal s_midi_ch      : std_logic_vector(3 downto 0);
  signal s_midi_note    : std_logic_vector(7 downto 0);
  signal s_midi_vel     : std_logic_vector(7 downto 0);
  signal s_ctrl_id      : std_logic_vector(7 downto 0);
  signal s_ctrl_val     : std_logic_vector(7 downto 0);

--------------------------------------------------------------------------------
-- components
--------------------------------------------------------------------------------
  component REGISTER_N is
  generic (
    N           : integer := 16);
  port (
    i_clk         : in   std_logic;
    i_reset_n     : in   std_logic;
    i_load_en     : in   std_logic;
    i_par_in      : in   std_logic_vector(N-1 downto 0);
    o_par_out     : out  std_logic_vector(N-1 downto 0));
  end component;

  component MIDI_CMD_LUT is
  port (
    i_midi_cmd    : in  std_logic_vector(3 downto 0);
    o_midi_cmd    : out t_midi_cmd
    );
  end component;

  component MIDI_CH_MODE_LUT is
  port (
    i_ch_mode     : in  std_logic_vector(7 downto 0);
    o_ch_mode     : out t_ch_mode
    );
  end component;

begin

  DATA_REG : REGISTER_N
  generic map (8)
  port map(
    i_clk         => i_clk,
    i_reset_n     => s_data_rst,
    i_load_en     => s_data_en,
    i_par_in      => i_data_in,
    o_par_out     => s_data_out
  );

  CMD_LUT : MIDI_CMD_LUT
  port map(
    i_midi_cmd    => s_midi_cmd,
    o_midi_cmd    => s_midi_cmd_in
  );

  CH_MODE_LUT : MIDI_CH_MODE_LUT
  port map(
    i_ch_mode     => s_ctrl_id,
    o_ch_mode     => s_ch_mode_lut
  );

  s_midi_cmd  <= s_data_out(MIDI_CMD);
  s_midi_ch   <= s_data_out(MIDI_CH);
  s_midi_note <= s_data_out(MIDI_DATA);
  s_midi_vel  <= s_data_out(MIDI_DATA);
  s_ctrl_id   <= s_data_out(MIDI_DATA);
  s_ctrl_val  <= s_data_out(MIDI_DATA);

  p_data_fsm_state: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_fsm_state <= st_idle;
    elsif i_clk'event and i_clk = '1' then
      case s_fsm_state is
        when st_idle    =>
          if i_new_data = '1' then
            s_fsm_state <= st_sample;
          else
            s_fsm_state <= st_idle;
          end if;
        when st_sample  =>
          s_fsm_state <= st_data;
        when st_data    =>
          s_fsm_state <= st_idle;
        when others     =>
          s_fsm_state <= st_idle;
      end case;
    end if;
  end process;

  p_data_fsm_control: process(s_fsm_state)
  begin
    case s_fsm_state is
      when st_idle    =>
        s_data_rst    <= '0';
        s_data_en     <= '0';
      when st_sample  =>
        s_data_rst    <= '1';
        s_data_en     <= '1';
      when st_data    =>
        s_data_rst    <= '1';
        s_data_en     <= '0';
      when others     =>
        s_data_rst    <= '0';
        s_data_en     <= '0';
    end case;
  end process;

  p_midi_fsm_state: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_msg_state <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      if s_msg_state = st_reset then
        s_msg_state <= st_status;
      elsif s_fsm_state = st_sample then
        case s_msg_state is
          when st_status  =>
            case s_midi_cmd_in is
              when midi_note_off  =>
              when midi_note_on   =>
              when midi_ctrl_ch   =>
              when midi_prg_ch    =>
                s_msg_state <= st_data1;
              when others         =>
                s_msg_state <= st_status;
            end case;
          when st_data1   =>
            case s_midi_cmd_r is
              when midi_note_off  =>
              when midi_note_on   =>
              when midi_ctrl_ch   =>
                s_msg_state <= st_data2;
              when midi_prg_ch    =>
                s_msg_state <= st_status;
              when others         =>
                s_msg_state <= st_status;
            end case;
          when st_data2   =>
            s_msg_state <= st_status;
          when others     =>
            s_msg_state <= st_status;
        end case;
      end if;
    end if;
  end process;

  p_midi_fsm_control: process(s_msg_state)
  begin
    case s_msg_state is
      when st_reset   =>
        s_midi_cmd_rst  <= '0';
        s_midi_cmd_en   <= '0';
      when st_status  =>
        s_midi_cmd_rst  <= '1';
        s_midi_cmd_en   <= '1';
      when st_data1   =>
        s_midi_cmd_rst  <= '1';
        s_midi_cmd_en   <= '0';
      when st_data2   =>
        s_midi_cmd_rst  <= '1';
        s_midi_cmd_en   <= '0';
      when others     =>
        s_midi_cmd_rst  <= '0';
        s_midi_cmd_en   <= '0';
    end case;
  end process;

  p_midi_cmd_reg: process(i_clk, s_midi_cmd_en, s_midi_cmd_rst)
  begin
    if s_midi_cmd_rst = '0' then
      s_midi_cmd_r <= midi_unknown;
    elsif i_clk'event and i_clk = '1' then
      if s_midi_cmd_en = '1' then
        s_midi_cmd_r <= s_midi_cmd_in;
      end if;
    end if;
  end process;

end architecture;
