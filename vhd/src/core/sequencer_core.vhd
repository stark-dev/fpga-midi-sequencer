library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SEQUENCER_CORE is
port (
  -- system inputs
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;

  i_btn_left      : in  std_logic;
  i_btn_up        : in  std_logic;
  i_btn_down      : in  std_logic;
  i_btn_right     : in  std_logic;

  i_tr_mute       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);
  i_tr_solo       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);

  -- direct midi events
  i_midi_ready    : in  std_logic;
  i_midi_data     : in  std_logic_vector(SEQ_EVENT_SIZE - 1  downto 0);

  -- playback events
  i_pb_ready      : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);
  i_pb_end        : in  std_logic_vector(SEQ_TRACKS - 1 downto 0);
  i_pb_data       : in  t_midi_data;

  -- external module ready
  i_pb_q_ready    : in  std_logic;

  -- outputs
  o_ts_seconds    : out std_logic_vector(ST_TSS_SIZE-1 downto 0);
  o_ts_fraction   : out std_logic_vector(ST_TSF_SIZE-1 downto 0);
  o_active_track  : out std_logic_vector(ST_TRACK_SIZE - 1 downto 0);
  o_rec_mode      : out std_logic;
  o_restart       : out std_logic;

  o_sound_on      : out std_logic;
  o_sg_patch      : out t_sg_patch;
  o_sg_note       : out t_sg_note;
  o_sg_vel        : out t_sg_vel;
  o_sg_start      : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
  o_sg_stop       : out std_logic_vector(SEQ_TRACKS - 1 downto 0);
  o_sg_poly       : out std_logic_vector(SEQ_TRACKS - 1 downto 0);

  o_display_a     : out t_display_array
);
end entity;

architecture BHV of SEQUENCER_CORE is

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  constant c_menu_size    : integer := 2;

  constant c_max_vol      : integer := 2**ST_VOL_SIZE - 1;
  constant c_min_vol      : integer := 0;

  constant c_btn_size     : integer := 8; -- TODO set to proper values
  constant c_btn_short    : integer := 50;
  constant c_btn_long     : integer := 150;

  constant c_ts_clock_div : integer := 12207;

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  -- track status registers
  type t_track_status is array (SEQ_TRACKS - 1 downto 0) of std_logic_vector(TR_ST_SIZE - 1 downto 0);

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

component TRACK_STATUS is
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;

  i_activate_tr   : in  std_logic;
  i_toggle_rec    : in  std_logic;

  i_patch_up      : in  std_logic;
  i_patch_dn      : in  std_logic;
  i_patch_rst     : in  std_logic;

  i_ch_up         : in  std_logic;
  i_ch_dn         : in  std_logic;
  i_ch_rst        : in  std_logic;

  i_vol_up        : in  std_logic;
  i_vol_dn        : in  std_logic;
  i_vol_rst       : in  std_logic;

  i_pan_up        : in  std_logic;
  i_pan_dn        : in  std_logic;
  i_pan_rst       : in  std_logic;

  i_mute          : in  std_logic;
  i_solo          : in  std_logic;

  i_poly_toggle   : in  std_logic;  -- polyphonic / mono
  i_omni_toggle   : in  std_logic;  -- omni channel mode

  o_track_status  : out std_logic_vector(TR_ST_SIZE - 1 downto 0)
  );
end component;

component TIMESTAMP_GEN is
generic (
  g_clock_div   : integer := 12207
);
port (
  -- system inputs
  i_clk         : in  std_logic;
  i_reset_n     : in  std_logic;
  -- play control
  i_play_stop_n : in  std_logic;
  i_restart     : in  std_logic;
  -- timestamp output
  o_ts_fraction : out std_logic_vector(ST_TSF_SIZE-1 downto 0);
  o_ts_seconds  : out std_logic_vector(ST_TSS_SIZE-1 downto 0)
  );
end component;

component SL_BUTTON is
generic (
  g_size      : integer := 12;
  g_short     : integer := 500;
  g_long      : integer := 1000
);
port (
  i_clk         : in  std_logic;
  i_reset_n     : in  std_logic;
  i_btn         : in  std_logic;
  o_long        : out std_logic;
  o_short       : out std_logic
);
end component;

component DISPLAY_CTRL is
port (
  i_state       : in  t_core_fsm;
  i_menu        : in  t_menu_option;
  i_ts_frac     : in  std_logic_vector(ST_TSF_SIZE - 1 downto 0);
  i_ts_secs     : in  std_logic_vector(ST_TSS_SIZE - 1 downto 0);
  i_main_vol    : in  std_logic_vector(ST_VOL_SIZE - 1 downto 0);
  i_active_tr   : in  std_logic_vector(ST_TRACK_SIZE - 1 downto 0);
  i_track_omni  : in  std_logic;
  i_track_poly  : in  std_logic;
  i_track_rec   : in  std_logic;
  i_track_pan   : in  std_logic_vector(TR_PAN_SIZE - 1 downto 0);
  i_track_vol   : in  std_logic_vector(TR_VOL_SIZE - 1 downto 0);
  i_track_patch : in  std_logic_vector(TR_PATCH_SIZE - 1 downto 0);
  o_display_a   : out t_display_array
);
end component;

component PLAYBACK_ENGINE is
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
end component;

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- core fsm status
  signal s_fsm_status     : t_core_fsm;

  -- timestamp registers
  signal s_ts_frac        : std_logic_vector(ST_TSF_SIZE - 1 downto 0);
  signal s_ts_secs        : std_logic_vector(ST_TSS_SIZE - 1 downto 0);

  signal s_ts_current     : std_logic_vector(SEQ_TIME_SIZE - 1 downto 0);

  signal s_ts_end_r_en    : std_logic;
  signal s_ts_end         : std_logic_vector(SEQ_TIME_SIZE - 1 downto 0);

  -- sequencer status
  signal s_play_pause_n   : std_logic;
  signal s_restart        : std_logic;

  signal s_play_end       : std_logic;

  signal s_modules_ready  : std_logic;

  -- interface buttons (s -> short, l -> long)
  signal s_btn_l_s        : std_logic;
  signal s_btn_l_l        : std_logic;
  signal s_btn_u_s        : std_logic;
  signal s_btn_u_l        : std_logic;
  signal s_btn_d_s        : std_logic;
  signal s_btn_d_l        : std_logic;
  signal s_btn_r_s        : std_logic;
  signal s_btn_r_l        : std_logic;

  -- interface display array
  signal s_display_array  : t_display_array;

  -- menu option
  signal s_menu_option    : t_menu_option;
  signal s_menu_reset     : std_logic;

  -- active track counter
  signal s_active_tr_rst  : std_logic;
  signal s_active_tr      : unsigned(ST_TRACK_SIZE - 1 downto 0);
  signal s_active_tr_tc_v : unsigned(ST_TRACK_SIZE - 1 downto 0);
  signal s_active_tr_up   : std_logic;
  signal s_active_tr_down : std_logic;

  signal s_active_tr_enc  : std_logic_vector(SEQ_TRACKS - 1 downto 0);

  -- volume control
  signal s_vol_up         : std_logic;
  signal s_vol_down       : std_logic;
  signal s_vol_rst        : std_logic;
  signal s_main_vol       : unsigned(ST_VOL_SIZE - 1 downto 0);

  -- menu option toggle
  signal s_menu_op_toggle : std_logic;

  -- track status control
  signal s_tr_reset       : std_logic_vector(SEQ_TRACKS - 1 downto 0);

  signal s_tr_toggle_rec  : std_logic;
  signal s_tr_patch_up    : std_logic;
  signal s_tr_patch_dn    : std_logic;
  signal s_tr_patch_rst   : std_logic;
  signal s_tr_ch_up       : std_logic;
  signal s_tr_ch_dn       : std_logic;
  signal s_tr_ch_rst      : std_logic;
  signal s_tr_vol_up      : std_logic;
  signal s_tr_vol_dn      : std_logic;
  signal s_tr_vol_rst     : std_logic;
  signal s_tr_pan_up      : std_logic;
  signal s_tr_pan_dn      : std_logic;
  signal s_tr_pan_rst     : std_logic;
  signal s_tr_mute        : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_tr_solo        : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_tr_poly_toggle : std_logic;
  signal s_tr_omni_toggle : std_logic;
  signal s_tr_status      : t_track_status;

  -- track status fields
  signal s_track_omni     : std_logic;
  signal s_track_poly     : std_logic;
  signal s_track_rec      : std_logic;
  signal s_track_solo     : std_logic;
  signal s_track_mono     : std_logic;
  signal s_track_pan      : std_logic_vector(TR_PAN_SIZE - 1 downto 0);
  signal s_track_vol      : std_logic_vector(TR_VOL_SIZE - 1 downto 0);
  signal s_track_patch    : std_logic_vector(TR_PATCH_SIZE - 1 downto 0);

  -- sound gen control
  signal s_sound_on       : std_logic;
  signal s_sound_rst      : std_logic;

  signal s_evt_ready      : std_logic_vector(SEQ_TRACKS - 1 downto 0);
  signal s_evt_data       : t_midi_data;

begin

  -- output assignment
  o_display_a       <= s_display_array;
  o_sound_on        <= s_sound_on;

  o_ts_seconds      <= s_ts_secs;
  o_ts_fraction     <= s_ts_frac;
  o_active_track    <= std_logic_vector(s_active_tr);
  o_restart         <= s_restart;

  p_poly_patch_assign: process(s_tr_status)
  begin
    for i in 0 to SEQ_TRACKS - 1 loop
      o_sg_poly(i)  <= s_tr_status(i)(TR_POLY_BIT);
      o_sg_patch(i) <= s_tr_status(i)(TR_PATCH_RANGE);
    end loop;
  end process;

  -- internal signal assignment
  s_active_tr_tc_v  <= to_unsigned(SEQ_TRACKS - 1, ST_TRACK_SIZE);
  s_tr_mute         <= i_tr_mute;
  s_tr_solo         <= i_tr_solo;

  -- current timestamp
  s_ts_current(ST_TS_RESERV) <= (others => '0');
  s_ts_current(ST_TSS_RANGE) <= s_ts_secs;
  s_ts_current(ST_TSF_RANGE) <= s_ts_frac;

  s_play_end        <= '1' when (s_ts_current = s_ts_end) else
                       '1' when and_reduce(i_pb_end) = '1' else
                       '0';

  s_modules_ready   <= i_pb_q_ready; -- TODO add other module ready signals in AND

  -- buttons
  s_tr_toggle_rec   <= s_btn_r_s when (s_fsm_status = st_idle) else '0';

  s_vol_up          <= s_btn_u_s when (s_fsm_status = st_idle) or (s_fsm_status = st_play) else '0';
  s_vol_down        <= s_btn_d_s when (s_fsm_status = st_idle) or (s_fsm_status = st_play) else '0';

  s_menu_op_toggle  <= s_btn_r_s when (s_fsm_status = st_menu) else '0';

  s_active_tr_up    <= s_btn_u_s when (s_fsm_status = st_menu) and (s_menu_option = op_track) else '0';
  s_active_tr_down  <= s_btn_d_s when (s_fsm_status = st_menu) and (s_menu_option = op_track) else '0';

  s_tr_patch_up     <= s_btn_u_s when (s_fsm_status = st_menu) and (s_menu_option = op_patch) else '0';
  s_tr_patch_dn     <= s_btn_d_s when (s_fsm_status = st_menu) and (s_menu_option = op_patch) else '0';
  s_tr_patch_rst    <= not(s_btn_r_l) when (s_fsm_status = st_menu) and (s_menu_option = op_patch) else '1';

  s_tr_ch_up        <= '0'; -- TODO decide if channel is controlled by sequencer or not
  s_tr_ch_dn        <= '0';
  s_tr_ch_rst       <= '1';

  s_tr_vol_up       <= s_btn_u_s when (s_fsm_status = st_menu) and (s_menu_option = op_track_vol) else '0';
  s_tr_vol_dn       <= s_btn_d_s when (s_fsm_status = st_menu) and (s_menu_option = op_track_vol) else '0';
  s_tr_vol_rst      <= not(s_btn_r_l) when (s_fsm_status = st_menu) and (s_menu_option = op_track_vol) else '1';

  s_tr_pan_up       <= s_btn_u_s when (s_fsm_status = st_menu) and (s_menu_option = op_pan) else '0';
  s_tr_pan_dn       <= s_btn_d_s when (s_fsm_status = st_menu) and (s_menu_option = op_pan) else '0';
  s_tr_pan_rst      <= not(s_btn_r_l) when (s_fsm_status = st_menu) and (s_menu_option = op_pan) else '1';

  s_tr_poly_toggle  <= (s_btn_u_s or s_btn_d_s) when (s_fsm_status = st_menu) and (s_menu_option = op_poly) else '0';
  s_tr_omni_toggle  <= (s_btn_u_s or s_btn_d_s) when (s_fsm_status = st_menu) and (s_menu_option = op_omni) else '0';

  -- track status fields
  s_track_omni      <= s_tr_status(to_integer(s_active_tr))(TR_OMNI_BIT);
  s_track_poly      <= s_tr_status(to_integer(s_active_tr))(TR_POLY_BIT);
  s_track_rec       <= s_tr_status(to_integer(s_active_tr))(TR_REC_BIT);
  s_track_solo      <= s_tr_status(to_integer(s_active_tr))(TR_SOLO_BIT);
  s_track_mono      <= s_tr_status(to_integer(s_active_tr))(TR_MONO_BIT);
  s_track_pan       <= s_tr_status(to_integer(s_active_tr))(TR_PAN_RANGE);
  s_track_vol       <= s_tr_status(to_integer(s_active_tr))(TR_VOL_RANGE);
  s_track_patch     <= s_tr_status(to_integer(s_active_tr))(TR_PATCH_RANGE);

  -- sound generator
  s_sound_rst       <= i_reset_n;   -- TODO add all sound off

  -- components
  TS_GEN : TIMESTAMP_GEN
  generic map (100) -- TODO remove comment (just for test purposes)
  -- generic map (c_ts_clock_div)
  port map (
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,
    i_play_stop_n   => s_play_pause_n,
    i_restart       => s_restart,
    o_ts_fraction   => s_ts_frac,
    o_ts_seconds    => s_ts_secs
  );

  TS_END : REGISTER_N
  generic map (SEQ_TIME_SIZE)
  port map(
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,
    i_load_en       => s_ts_end_r_en,
    i_par_in        => s_ts_current,
    o_par_out       => s_ts_end
  );

  GEN_TRACK_STATUS:
  for i in 0 to SEQ_TRACKS-1 generate
    TRACK_ST_X: TRACK_STATUS
      port map(
        i_clk           => i_clk,
        i_reset_n       => s_tr_reset(i),

        i_activate_tr   => s_active_tr_enc(i),
        i_toggle_rec    => s_tr_toggle_rec,

        i_patch_up      => s_tr_patch_up,
        i_patch_dn      => s_tr_patch_dn,
        i_patch_rst     => s_tr_patch_rst,

        i_ch_up         => s_tr_ch_up,
        i_ch_dn         => s_tr_ch_dn,
        i_ch_rst        => s_tr_ch_rst,

        i_vol_up        => s_tr_vol_up,
        i_vol_dn        => s_tr_vol_dn,
        i_vol_rst       => s_tr_vol_rst,

        i_pan_up        => s_tr_pan_up,
        i_pan_dn        => s_tr_pan_dn,
        i_pan_rst       => s_tr_pan_rst,

        i_mute          => s_tr_mute(i),
        i_solo          => s_tr_solo(i),

        i_poly_toggle   => s_tr_poly_toggle,
        i_omni_toggle   => s_tr_omni_toggle,

        o_track_status  => s_tr_status(i)
      );
  end generate;

  BTN_UP : SL_BUTTON
  generic map (
    g_size        => c_btn_size,
    g_short       => c_btn_short,
    g_long        => c_btn_long
  )
  port map (
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_btn         => i_btn_up,
    o_long        => s_btn_u_l,
    o_short       => s_btn_u_s
  );

  BTN_DOWN : SL_BUTTON
  generic map (
    g_size        => c_btn_size,
    g_short       => c_btn_short,
    g_long        => c_btn_long
  )
  port map (
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_btn         => i_btn_down,
    o_long        => s_btn_d_l,
    o_short       => s_btn_d_s
  );

  BTN_LEFT : SL_BUTTON
  generic map (
    g_size        => c_btn_size,
    g_short       => c_btn_short,
    g_long        => c_btn_long
  )
  port map (
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_btn         => i_btn_left,
    o_long        => s_btn_l_l,
    o_short       => s_btn_l_s
  );

  BTN_RIGTH : SL_BUTTON
  generic map (
    g_size        => c_btn_size,
    g_short       => c_btn_short,
    g_long        => c_btn_long
  )
  port map (
    i_clk         => i_clk,
    i_reset_n     => i_reset_n,
    i_btn         => i_btn_right,
    o_long        => s_btn_r_l,
    o_short       => s_btn_r_s
  );

  DISPLAY_C : DISPLAY_CTRL
  port map(
    i_state       => s_fsm_status,
    i_menu        => s_menu_option,
    i_ts_frac     => s_ts_frac,
    i_ts_secs     => s_ts_secs,
    i_main_vol    => std_logic_vector(s_main_vol),
    i_active_tr   => std_logic_vector(s_active_tr),
    i_track_omni  => s_track_omni,
    i_track_poly  => s_track_poly,
    i_track_rec   => s_track_rec,
    i_track_pan   => s_track_pan,
    i_track_vol   => s_track_vol,
    i_track_patch => s_track_patch,
    o_display_a   => s_display_array
  );

  PB_ENGINE_GEN:
  for i in 0 to SEQ_TRACKS - 1 generate
    PLAYBACK_ENG_X : PLAYBACK_ENGINE
    port map(
      i_clk         => i_clk,
      i_reset_n     => s_sound_rst,
      i_state       => s_fsm_status,
      i_evt_ready   => s_evt_ready(i),
      i_evt_data    => s_evt_data(i),
      o_sg_note     => o_sg_note(i),
      o_sg_vel      => o_sg_vel(i),
      o_sg_start    => o_sg_start(i),
      o_sg_stop     => o_sg_stop(i)
    );
  end generate;

  p_sound_evt_in_mux: process(s_active_tr, i_midi_ready, i_midi_data, i_pb_ready, i_pb_data)
  begin
    for i in 0 to SEQ_TRACKS - 1 loop
      if to_integer(s_active_tr) = i then
        s_evt_ready(i)  <= i_midi_ready;
        s_evt_data(i)   <= i_midi_data;
      else
        s_evt_ready(i)  <= i_pb_ready(i);
        s_evt_data(i)   <= i_pb_data(i);
      end if;
    end loop;
  end process;

  -- processes
  p_core_fsm: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_fsm_status <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_fsm_status is
        when st_reset   =>
          s_fsm_status  <= st_init;

        when st_init    =>
          if s_modules_ready = '1' then
            s_fsm_status  <= st_idle;
          else
            s_fsm_status  <= st_init;
          end if;

        when st_idle    =>
          if s_btn_l_s = '1' then
            if s_track_rec = '1' then
              s_fsm_status  <= st_rec;
            else
              s_fsm_status  <= st_play;
            end if;
          elsif s_btn_l_l = '1' then
            s_fsm_status  <= st_stop;
          elsif s_btn_r_l = '1' then
            s_fsm_status  <= st_menu;
          else
            s_fsm_status  <= st_idle;
          end if;

        when st_play    =>
          if s_btn_l_s = '1' then
            s_fsm_status  <= st_idle;
          elsif s_play_end = '1' then
            s_fsm_status  <= st_stop;
          else
            s_fsm_status  <= st_play;
          end if;

        when st_rec     =>
          if s_btn_l_s = '1' then
            s_fsm_status  <= st_end;
          else
            s_fsm_status  <= st_rec;
          end if;

        when st_end     =>
          s_fsm_status  <= st_stop;

        when st_stop    =>
          s_fsm_status  <= st_init;

        when st_menu    =>
          if s_btn_l_s = '1' then
            s_fsm_status  <= st_idle;
          else
            s_fsm_status  <= st_menu;
          end if;

        when others     =>
          s_fsm_status  <= st_reset;

      end case;
    end if;
  end process;

  p_core_fsm_ctrl: process(s_fsm_status, s_menu_option, s_ts_current, s_ts_end)
  begin
    case s_fsm_status is
      when st_reset   =>
        s_play_pause_n    <= '0';
        s_ts_end_r_en     <= '0';
        o_rec_mode        <= '0';
        s_restart         <= '1';
        s_sound_on        <= '0';
        s_active_tr_rst   <= '0';
        s_menu_reset      <= '0';
        s_vol_rst         <= '0';
        s_tr_reset        <= (others => '0');

      when st_init    =>
        s_play_pause_n    <= '0';
        s_ts_end_r_en     <= '0';
        o_rec_mode        <= '0';
        s_restart         <= '0';
        s_sound_on        <= '0';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_idle    =>
        s_play_pause_n    <= '0';
        s_ts_end_r_en     <= '0';
        o_rec_mode        <= '0';
        s_restart         <= '0';
        s_sound_on        <= '1';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_play    =>
        s_play_pause_n    <= '1';
        s_ts_end_r_en     <= '0';
        o_rec_mode        <= '0';
        s_restart         <= '0';
        s_sound_on        <= '1';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_rec     =>
        s_play_pause_n    <= '1';
        s_ts_end_r_en     <= '0';
        o_rec_mode        <= '1';
        s_restart         <= '0';
        s_sound_on        <= '1';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_stop    =>
        s_play_pause_n    <= '0';
        s_ts_end_r_en     <= '0';
        o_rec_mode        <= '0';
        s_restart         <= '1';
        s_sound_on        <= '0';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_end     =>
        s_play_pause_n    <= '0';
        if s_ts_current > s_ts_end then
          s_ts_end_r_en     <= '1';
        else
          s_ts_end_r_en     <= '0';
        end if;
        o_rec_mode        <= '0';
        s_restart         <= '0';
        s_sound_on        <= '0';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_menu    =>
        s_play_pause_n    <= '0';
        s_ts_end_r_en     <= '0';
        o_rec_mode        <= '0';
        s_restart         <= '0';
        s_sound_on        <= '0';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '1';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when others     =>
        s_play_pause_n    <= '0';
        s_ts_end_r_en     <= '0';
        o_rec_mode        <= '0';
        s_restart         <= '1';
        s_sound_on        <= '0';
        s_active_tr_rst   <= '0';
        s_menu_reset      <= '0';
        s_vol_rst         <= '0';
        s_tr_reset        <= (others => '0');
    end case;
  end process;

  p_volume_ctrl: process(i_clk, i_reset_n, s_vol_rst)
  begin
    if i_reset_n = '0' or s_vol_rst = '0' then
      s_main_vol <= (others => '1');
    elsif i_clk'event and i_clk = '1' then
      if s_vol_up = '1' then
        if s_main_vol /= to_unsigned(c_max_vol, ST_VOL_SIZE) then
          s_main_vol <= s_main_vol + 1;
        end if;
      elsif s_vol_down = '1' then
        if s_main_vol /= to_unsigned(c_min_vol, ST_VOL_SIZE) then
          s_main_vol <= s_main_vol - 1;
        end if;
      end if;
    end if;
  end process;

  p_menu_opt: process(i_clk, i_reset_n, s_menu_reset)
  begin
    if i_reset_n = '0' or s_menu_reset = '0' then
      s_menu_option <= op_track;
    elsif i_clk'event and i_clk = '1' then
      if s_fsm_status = st_menu then
        case s_menu_option is
          when op_track        =>
            if s_menu_op_toggle = '1' then
              s_menu_option <= op_patch;
            else
              s_menu_option <= op_track;
            end if;

          when op_patch        =>
            if s_menu_op_toggle = '1' then
              s_menu_option <= op_track_vol;
            else
              s_menu_option <= op_patch;
            end if;

          when op_track_vol    =>
            if s_menu_op_toggle = '1' then
              s_menu_option <= op_pan;
            else
              s_menu_option <= op_track_vol;
            end if;

          when op_pan       =>
            if s_menu_op_toggle = '1' then
              s_menu_option <= op_poly;
            else
              s_menu_option <= op_pan;
            end if;

          when op_poly      =>
            if s_menu_op_toggle = '1' then
              s_menu_option <= op_omni;
            else
              s_menu_option <= op_poly;
            end if;

          when op_omni      =>
            if s_menu_op_toggle = '1' then
              s_menu_option <= op_track_vol;
            else
              s_menu_option <= op_omni;
            end if;

          when others       =>
            s_menu_option   <= op_track;
        end case;
      end if;
    end if;
  end process;

  p_active_track_enc: process(s_active_tr)
  begin
    for i in 0 to SEQ_TRACKS - 1 loop
      if to_integer(s_active_tr) = i then
        s_active_tr_enc(i) <= '1';
      else
        s_active_tr_enc(i) <= '0';
      end if;
    end loop;
  end process;

  p_active_track_cnt: process(i_clk, s_active_tr_rst)
  begin
    if s_active_tr_rst = '0' then
      s_active_tr <= (others => '0');
    elsif i_clk'event and i_clk = '1' then
      if s_active_tr_up = '1' then
        if s_active_tr = s_active_tr_tc_v then
          s_active_tr <= (others => '0');
        else
          s_active_tr <= s_active_tr + 1;
        end if;
      elsif s_active_tr_down = '1' then
        if s_active_tr <= to_unsigned(0, ST_TRACK_SIZE) then
          s_active_tr <= s_active_tr_tc_v;
        else
          s_active_tr <= s_active_tr - 1;
        end if;
      end if;
    end if;
  end process;

end architecture;
