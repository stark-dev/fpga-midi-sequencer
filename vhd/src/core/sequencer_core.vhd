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
  i_btn_right     : in  std_logic
  );
end entity;

architecture BHV of SEQUENCER_CORE is

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  constant c_tracks       : integer := 8;
  constant c_tracks_size  : integer := up_int_log2(c_tracks-1);

  constant c_menu_size    : integer := 2;

  constant c_max_vol      : integer := 2**ST_VOL_SIZE - 1;
  constant c_min_vol      : integer := 0;

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  -- core fsm
  type t_core_fsm     is (st_reset, st_init, st_play, st_menu, st_option);

  -- track status registers
  type t_track_status is array (c_tracks - 1 downto 0) of std_logic_vector(TR_ST_SIZE - 1 downto 0);

  -- menu option
  type t_menu_option  is (op_volume, op_pan, op_poly, op_omni);

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

component UP_DOWN_COUNTER is
  generic (
    N           : integer := 16);
  port (
    i_clk       : in   std_logic;
    i_reset_n   : in   std_logic;
    i_en        : in   std_logic;
    i_tc_value  : in   unsigned(N-1 downto 0);
    i_up_down   : in   std_logic;  -- 0 up, 1 down
    o_q         : out  unsigned(N-1 downto 0);
    o_tc        : out  std_logic);
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

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- core fsm status
  signal s_fsm_status     : t_core_fsm;

  -- interface buttons (s -> short, l -> long)
  signal s_btn_left_s     : std_logic;
  signal s_btn_left_l     : std_logic;
  signal s_btn_up_s       : std_logic;
  signal s_btn_up_l       : std_logic;
  signal s_btn_down_s     : std_logic;
  signal s_btn_down_l     : std_logic;
  signal s_btn_rigth_s    : std_logic;
  signal s_btn_rigth_l    : std_logic;

  -- menu option
  signal s_menu_option    : t_menu_option;
  signal s_menu_op_cnt    : unsigned(c_menu_size - 1 downto 0);
  signal s_menu_reset     : std_logic;

  -- active track counter
  signal s_active_tr_en   : std_logic;
  signal s_active_tr_rst  : std_logic;
  signal s_active_tr      : unsigned(c_tracks_size - 1 downto 0);
  signal s_active_tr_tc_v : unsigned(c_tracks_size - 1 downto 0);
  signal s_active_tr_udn  : std_logic;
  signal s_active_tr_tc   : std_logic;

  signal s_active_tr_enc  : std_logic_vector(c_tracks - 1 downto 0);

  -- volume control
  signal s_vol_up         : std_logic;
  signal s_vol_down       : std_logic;
  signal s_main_vol       : unsigned(ST_VOL_SIZE - 1 downto 0);

  -- track status control
  signal s_tr_reset       : std_logic_vector(c_tracks - 1 downto 0);

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
  signal s_tr_mute        : std_logic_vector(c_tracks - 1 downto 0);
  signal s_tr_solo        : std_logic_vector(c_tracks - 1 downto 0);
  signal s_tr_poly_toggle : std_logic;
  signal s_tr_omni_toggle : std_logic;
  signal s_tr_status      : t_track_status;

begin

  -- assignment
  s_active_tr_tc_v <= to_unsigned(c_tracks - 1, c_tracks_size);

  -- components
  ACTIVE_TRACK_R : UP_DOWN_COUNTER
  generic map (c_tracks_size)
  port map(
    i_clk         => i_clk,
    i_reset_n     => s_active_tr_rst,
    i_en          => s_active_tr_en,
    i_tc_value    => s_active_tr_tc_v,
    i_up_down     => s_active_tr_udn,
    o_q           => s_active_tr,
    o_tc          => s_active_tr_tc
  );

  GEN_TRACK_STATUS:
  for i in 0 to c_tracks-1 generate
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

  -- process
  p_volume_ctrl: process(i_clk, i_reset_n, s_vol_down, s_vol_up)
  begin
    if i_reset_n = '0' then
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

  p_core_fsm: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_fsm_status <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_fsm_status is
        when st_reset   =>
          s_fsm_status  <= st_play; -- TODO: add init state
        when st_init    =>
          s_fsm_status  <= st_play;
        when st_play    =>
          s_fsm_status  <= st_play;
        when st_menu    =>
          s_fsm_status  <= st_play;
        when st_option  =>
          s_fsm_status  <= st_play;
        when others     =>
          s_fsm_status  <= st_play;
      end case;
    end if;
  end process;

  p_core_fsm_update: process(s_fsm_status)
  begin
    s_active_tr_rst   <= '0';
    s_tr_reset        <= (others => '0');
    s_menu_reset      <= '0';

    case s_fsm_status is
      when st_reset   =>
      when st_init    =>
      when st_play    =>
      when st_menu    =>
      when st_option  =>
      when others     =>
    end case;
  end process;

  p_menu_opt: process(i_clk, i_reset_n, s_menu_reset)
  begin
    if i_reset_n = '0' or s_menu_reset = '0' then
      s_menu_op_cnt <= (others => '0');
    elsif i_clk'event and i_clk = '1' then
      if s_fsm_status = st_menu then
        if s_btn_up_s = '1' then
          s_menu_op_cnt <= s_menu_op_cnt + 1;
        elsif s_btn_down_s = '1' then
          s_menu_op_cnt <= s_menu_op_cnt - 1;
        end if;
      end if;
    end if;
  end process;

  p_menu_opt_lut: process(s_menu_op_cnt)
  begin
    case s_menu_op_cnt is
      when "00" =>
        s_menu_option <= op_volume;
      when "01" =>
        s_menu_option <= op_pan;
      when "10" =>
        s_menu_option <= op_poly;
      when "11" =>
        s_menu_option <= op_omni;
      when others =>
        s_menu_option <= op_volume;
    end case;
  end process;

  p_active_track_enc: process(s_active_tr)
  begin
    for i in 0 to c_tracks - 1 loop
      if to_integer(s_active_tr) = i then
        s_active_tr_enc(i) <= '1';
      else
        s_active_tr_enc(i) <= '0';
      end if;
    end loop;
  end process;

end architecture;
