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

  o_midi_msg    : out std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);
  o_midi_ready  : out std_logic
  );
end entity;

architecture BHV of MIDI_EVT_FILTER is

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  type t_midi_fsm is (st_reset, st_idle, st_status, st_wait1, st_data1, st_wait2, st_data2, st_end, st_ready);

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- fsm
  signal s_msg_state  : t_midi_fsm;  -- message fsm

  -- sample data
  signal s_sample     : std_logic;

  -- new data flag
  signal s_data_flag  : std_logic;
  signal s_rst_flag   : std_logic;

  -- message data register
  signal s_data_out     : std_logic_vector(7 downto 0);

  -- command register
  signal s_cmd_out      : std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);
  signal s_cmd_ready    : std_logic;

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
  signal s_midi_data    : std_logic_vector(6 downto 0);

  -- sequencer data fields
  signal s_cmd_type     : std_logic_vector(MIDI_TYPE_SIZE - 1 downto 0);

  signal s_cmd_type_e   : std_logic;
  signal s_cmd_ch_e     : std_logic;
  signal s_cmd_data1_e  : std_logic;
  signal s_cmd_data2_e  : std_logic;

  -- output command latch
  signal s_out_latch    : std_logic_vector(SEQ_EVENT_SIZE - 1 downto 0);
  signal s_out_latch_en : std_logic;

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
    i_ch_mode     : in  std_logic_vector(6 downto 0);
    o_ch_mode     : out t_ch_mode
    );
  end component;

  component MIDI_TYPE_LUT is
  port (
    i_midi_cmd    : in  t_midi_cmd;
    o_type        : out std_logic_vector(MIDI_TYPE_SIZE - 1 downto 0)
    );
  end component;

begin

  CMD_LUT : MIDI_CMD_LUT
  port map(
    i_midi_cmd    => s_midi_cmd,
    o_midi_cmd    => s_midi_cmd_in
  );

  CH_MODE_LUT : MIDI_CH_MODE_LUT
  port map(
    i_ch_mode     => s_midi_data,
    o_ch_mode     => s_ch_mode_lut
  );

  TYPE_LUT : MIDI_TYPE_LUT
  port map(
    i_midi_cmd    => s_midi_cmd_in,
    o_type        => s_cmd_type
  );

  DATA_REG : REGISTER_N
  generic map (8)
  port map(
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_load_en     => s_data_flag,
    i_par_in      => i_data_in,
    o_par_out     => s_data_out
  );

  TYPE_REG : REGISTER_N
  generic map (MIDI_TYPE_SIZE)
  port map(
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_load_en     => s_cmd_type_e,
    i_par_in      => s_cmd_type,
    o_par_out     => s_cmd_out(SEQ_TYPE_RANGE)
  );

  CH_REG : REGISTER_N
  generic map (4)
  port map(
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_load_en     => s_cmd_ch_e,
    i_par_in      => s_midi_ch,
    o_par_out     => s_cmd_out(SEQ_CH_RANGE)
  );

  DATA1_REG : REGISTER_N
  generic map (7)
  port map(
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_load_en     => s_cmd_data1_e,
    i_par_in      => s_midi_data,
    o_par_out     => s_cmd_out(SEQ_DATA1_RANGE)
  );

  DATA2_REG : REGISTER_N
  generic map (7)
  port map(
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_load_en     => s_cmd_data2_e,
    i_par_in      => s_midi_data,
    o_par_out     => s_cmd_out(SEQ_DATA2_RANGE)
  );

  CMD_REG : REGISTER_N
  generic map (SEQ_EVENT_SIZE)
  port map(
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_load_en     => s_out_latch_en,
    i_par_in      => s_cmd_out,
    o_par_out     => s_out_latch
  );

  s_cmd_out(SEQ_RSVD_RANGE) <= (others => '0');

  o_midi_msg      <= s_out_latch;
  o_midi_ready    <= s_cmd_ready;


  s_midi_cmd      <= s_data_out(MIDI_CMD_RANGE);
  s_midi_ch       <= s_data_out(MIDI_CH_RANGE);
  s_midi_data     <= s_data_out(MIDI_DATA_RANGE);

  s_rst_flag      <= i_reset_n and not(s_sample);

  s_midi_cmd_en   <= '1' when s_msg_state = st_idle and s_sample = '1' else '0';

  p_sample_data: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_sample <= '0';
    elsif i_clk'event and i_clk = '1' then
      if s_data_flag = '1' then
        s_sample <= '1';
      else
        s_sample <= '0';
      end if;
    end if;
  end process;

  p_midi_fsm_state: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_msg_state <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_msg_state is
        when st_reset   =>
          s_msg_state <= st_idle;
        when st_idle    =>
          if (s_sample = '1') and (s_midi_cmd_in /= midi_unknown) then
            s_msg_state <= st_status;
          else
            s_msg_state <= st_idle;
          end if;
        when st_status  =>
          case s_midi_cmd_r is
            when midi_note_off  =>
              s_msg_state <= st_wait1;
            when midi_note_on   =>
              s_msg_state <= st_wait1;
            when midi_ctrl_ch   =>
              s_msg_state <= st_wait1;
            when midi_prg_ch    =>
              s_msg_state <= st_wait1;
            when others         =>
              s_msg_state <= st_idle;
          end case;
        when st_wait1   =>
          if s_sample = '1' then
            s_msg_state <= st_data1;
          else
            s_msg_state <= st_wait1;
          end if;
        when st_data1   =>
          case s_midi_cmd_r is
            when midi_note_off  =>
              s_msg_state <= st_wait2;
            when midi_note_on   =>
              s_msg_state <= st_wait2;
            when midi_ctrl_ch   =>
              s_msg_state <= st_wait2;
            when midi_prg_ch    =>
              s_msg_state <= st_end;
            when others         =>
              s_msg_state <= st_idle;
          end case;
        when st_wait2   =>
          if s_sample = '1' then
            s_msg_state <= st_data2;
          else
            s_msg_state <= st_wait2;
          end if;
        when st_data2   =>
          s_msg_state <= st_end;
        when st_end     =>
          s_msg_state <= st_ready;
        when st_ready   =>
          s_msg_state <= st_idle;
        when others     =>
          s_msg_state <= st_idle;
      end case;
    end if;
  end process;

  p_midi_fsm_control: process(s_msg_state)
  begin
    case s_msg_state is
      when st_reset   =>
        s_midi_cmd_rst  <= '0';

        s_cmd_type_e    <= '0';
        s_cmd_ch_e      <= '0';
        s_cmd_data1_e   <= '0';
        s_cmd_data2_e   <= '0';

        s_out_latch_en  <= '0';

        s_cmd_ready     <= '0';

      when st_idle    =>
        s_midi_cmd_rst  <= '1';

        s_cmd_type_e    <= '0';
        s_cmd_ch_e      <= '0';
        s_cmd_data1_e   <= '0';
        s_cmd_data2_e   <= '0';

        s_out_latch_en  <= '0';

        s_cmd_ready     <= '0';

      when st_status  =>
        s_midi_cmd_rst  <= '1';

        s_cmd_type_e    <= '1';
        s_cmd_ch_e      <= '1';
        s_cmd_data1_e   <= '0';
        s_cmd_data2_e   <= '0';

        s_out_latch_en  <= '0';

        s_cmd_ready     <= '0';

      when st_wait1   =>
        s_midi_cmd_rst  <= '1';

        s_cmd_type_e    <= '0';
        s_cmd_ch_e      <= '0';
        s_cmd_data1_e   <= '0';
        s_cmd_data2_e   <= '0';

        s_out_latch_en  <= '0';

        s_cmd_ready     <= '0';

      when st_data1   =>
        s_midi_cmd_rst  <= '1';

        s_cmd_type_e    <= '0';
        s_cmd_ch_e      <= '0';
        s_cmd_data1_e   <= '1';
        s_cmd_data2_e   <= '0';

        s_out_latch_en  <= '0';

        s_cmd_ready     <= '0';

      when st_wait2   =>
        s_midi_cmd_rst  <= '1';

        s_cmd_type_e    <= '0';
        s_cmd_ch_e      <= '0';
        s_cmd_data1_e   <= '0';
        s_cmd_data2_e   <= '0';

        s_out_latch_en  <= '0';

        s_cmd_ready     <= '0';

      when st_data2   =>
        s_midi_cmd_rst  <= '1';

        s_cmd_type_e    <= '0';
        s_cmd_ch_e      <= '0';
        s_cmd_data1_e   <= '0';
        s_cmd_data2_e   <= '1';

        s_out_latch_en  <= '0';

        s_cmd_ready     <= '0';

      when st_end     =>
        s_midi_cmd_rst  <= '0';

        s_cmd_type_e    <= '0';
        s_cmd_ch_e      <= '0';
        s_cmd_data1_e   <= '0';
        s_cmd_data2_e   <= '0';

        s_out_latch_en  <= '1';

        s_cmd_ready     <= '0';

      when st_ready     =>
        s_midi_cmd_rst  <= '0';

        s_cmd_type_e    <= '0';
        s_cmd_ch_e      <= '0';
        s_cmd_data1_e   <= '0';
        s_cmd_data2_e   <= '0';

        s_out_latch_en  <= '0';

        s_cmd_ready     <= '1';

      when others     =>
        s_midi_cmd_rst  <= '0';

        s_cmd_type_e    <= '0';
        s_cmd_ch_e      <= '0';
        s_cmd_data1_e   <= '0';
        s_cmd_data2_e   <= '0';

        s_out_latch_en  <= '0';

        s_cmd_ready     <= '0';

    end case;
  end process;

  p_midi_cmd_reg: process(i_clk, s_midi_cmd_rst)
  begin
    if s_midi_cmd_rst = '0' then
      s_midi_cmd_r <= midi_unknown;
    elsif i_clk'event and i_clk = '1' then
      if s_midi_cmd_en = '1' then
        s_midi_cmd_r <= s_midi_cmd_in;
      end if;
    end if;
  end process;

  p_new_data_flag: process(i_new_data, s_rst_flag)
  begin
    if s_rst_flag = '0' then
      s_data_flag <= '0';
    elsif i_new_data'event and i_new_data = '1' then
      s_data_flag <= '1';
    end if;
  end process;

end architecture;
