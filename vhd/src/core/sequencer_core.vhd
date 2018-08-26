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
  i_tr_solo       : in  std_logic_vector(SEQ_TRACKS - 1 downto 0)
  );
end entity;

architecture BHV of SEQUENCER_CORE is

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  constant c_menu_size    : integer := 2;

  constant c_max_vol      : integer := 2**ST_VOL_SIZE - 1;
  constant c_min_vol      : integer := 0;

  constant c_btn_size     : integer := 8;
  constant c_btn_short    : integer := 50;
  constant c_btn_long     : integer := 150;

  constant c_ts_clock_div : integer := 12207;

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  -- core fsm
  type t_core_fsm     is (st_reset, st_init, st_idle, st_play, st_stop, st_menu);

  -- track status registers
  type t_track_status is array (SEQ_TRACKS - 1 downto 0) of std_logic_vector(TR_ST_SIZE - 1 downto 0);

  -- menu option
  type t_menu_option  is (op_track, op_patch, op_track_vol, op_pan, op_poly, op_omni);

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

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- core fsm status
  signal s_fsm_status     : t_core_fsm;

  -- timestamp registers
  signal s_ts_frac        : std_logic_vector(ST_TSF_SIZE - 1 downto 0);
  signal s_ts_secs        : std_logic_vector(ST_TSS_SIZE - 1 downto 0);

  signal s_ts_frac_end    : std_logic_vector(ST_TSF_SIZE - 1 downto 0);
  signal s_ts_secs_end    : std_logic_vector(ST_TSS_SIZE - 1 downto 0);

  -- sequencer status
  signal s_play_pause_n   : std_logic;
  signal s_restart        : std_logic;

  signal s_play_end       : std_logic;

  -- interface buttons (s -> short, l -> long)
  signal s_btn_l_s        : std_logic;
  signal s_btn_l_l        : std_logic;
  signal s_btn_u_s        : std_logic;
  signal s_btn_u_l        : std_logic;
  signal s_btn_d_s        : std_logic;
  signal s_btn_d_l        : std_logic;
  signal s_btn_r_s        : std_logic;
  signal s_btn_r_l        : std_logic;

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

begin

  -- assignment
  s_active_tr_tc_v  <= to_unsigned(SEQ_TRACKS - 1, ST_TRACK_SIZE);
  s_tr_mute         <= i_tr_mute;
  s_tr_solo         <= i_tr_solo;

  s_ts_frac_end     <= (others => '1'); -- TODO set rec end
  s_ts_secs_end     <= (others => '1');

  s_play_end        <= '1' when (s_ts_frac = s_ts_frac_end) and (s_ts_secs = s_ts_secs_end) else '0';

  -- buttons
  s_tr_toggle_rec   <= '0'; -- TODO add button

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

  -- components
  TS_GEN : TIMESTAMP_GEN
  -- generic map (c_ts_clock_div) -- TODO uncomment (just for test purposes)
  generic map (100)
  port map (
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,
    i_play_stop_n   => s_play_pause_n,
    i_restart       => s_restart,
    o_ts_fraction   => s_ts_frac,
    o_ts_seconds    => s_ts_secs
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

  -- processes
  p_core_fsm: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_fsm_status <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_fsm_status is
        when st_reset   =>
          s_fsm_status  <= st_idle; -- TODO: add init state

        when st_init    =>
          s_fsm_status  <= st_idle;

        when st_idle    =>
          if s_btn_l_s = '1' then
            s_fsm_status  <= st_play;
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

        when st_stop    =>
          s_fsm_status  <= st_idle;

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

  p_core_fsm_ctrl: process(s_fsm_status, s_menu_option)
  begin
    case s_fsm_status is
      when st_reset   =>
        s_play_pause_n    <= '0';
        s_restart         <= '1';
        s_active_tr_rst   <= '0';
        s_menu_reset      <= '0';
        s_vol_rst         <= '0';
        s_tr_reset        <= (others => '0');

      when st_init    =>
        s_play_pause_n    <= '0';
        s_restart         <= '0';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_idle    =>
        s_play_pause_n    <= '0';
        s_restart         <= '0';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_play    =>
        s_play_pause_n    <= '1';
        s_restart         <= '0';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_stop    =>
        s_play_pause_n    <= '0';
        s_restart         <= '1';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '0';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when st_menu    =>
        s_play_pause_n    <= '0';
        s_restart         <= '0';
        s_active_tr_rst   <= '1';
        s_menu_reset      <= '1';
        s_vol_rst         <= '1';
        s_tr_reset        <= (others => '1');

      when others     =>
        s_play_pause_n    <= '0';
        s_restart         <= '1';
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
