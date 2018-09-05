library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TRACK_STATUS is
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
end entity;

architecture BHV of TRACK_STATUS is

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  constant c_max_patch    : integer := 2**TR_PATCH_SIZE - 1;
  constant c_min_patch    : integer := 0;

  constant c_max_vol      : integer := 2**TR_VOL_SIZE - 1;
  constant c_min_vol      : integer := 0;

  constant c_max_pan      : integer := 2**TR_PAN_SIZE - 1;
  constant c_mid_pan      : integer := (c_max_pan / 2) + 1;
  constant c_min_pan      : integer := 0;

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  signal s_patch_number   : unsigned(TR_PATCH_SIZE - 1 downto 0);
  signal s_ch_number      : unsigned(TR_CH_SIZE - 1 downto 0);
  signal s_volume         : unsigned(TR_VOL_SIZE - 1 downto 0);
  signal s_pan            : unsigned(TR_PAN_SIZE - 1 downto 0);
  signal s_mute           : std_logic;
  signal s_solo           : std_logic;
  signal s_rec            : std_logic;
  signal s_act            : std_logic;
  signal s_poly           : std_logic;
  signal s_omni           : std_logic;

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

begin

  o_track_status(TR_PATCH_RANGE)  <= std_logic_vector(s_patch_number);
  o_track_status(TR_CH_RANGE)     <= std_logic_vector(s_ch_number);
  o_track_status(TR_VOL_RANGE)    <= std_logic_vector(s_volume);
  o_track_status(TR_PAN_RANGE)    <= std_logic_vector(s_pan);
  o_track_status(TR_MONO_BIT)     <= s_mute;
  o_track_status(TR_SOLO_BIT)     <= s_solo;
  o_track_status(TR_REC_BIT)      <= s_rec;
  o_track_status(TR_ACT_BIT)      <= s_act;
  o_track_status(TR_POLY_BIT)     <= s_poly;
  o_track_status(TR_OMNI_BIT)     <= s_omni;
  o_track_status(TR_RESERVED)     <= '0';

  -- process
  p_patch_select: process(i_clk, i_reset_n, i_patch_rst)
  begin
    if i_reset_n = '0' or i_patch_rst = '0' then
      s_patch_number <= (others => '0');
    elsif i_clk'event and i_clk = '1' then
      if s_act = '1' then
        if i_patch_up  = '1' then
          if s_patch_number = to_unsigned(c_max_patch, TR_PATCH_SIZE) then
            s_patch_number <= (others => '0');
          else
            s_patch_number <= s_patch_number + 1;
          end if;
        elsif i_patch_dn = '1' then
          if s_patch_number = c_min_patch then
            s_patch_number <= to_unsigned(c_min_patch, TR_PATCH_SIZE);
          else
            s_patch_number <= s_patch_number - 1;
          end if;
        end if;
      end if;
    end if;
  end process;

  p_channel_select: process(i_clk, i_reset_n, i_ch_rst)
  begin
    if i_reset_n = '0' or i_ch_rst = '0' then
      s_ch_number <= (others => '0');
    elsif i_clk'event and i_clk = '1' then
      if s_act = '1' then
        if i_ch_up = '1' then
          s_ch_number <= s_ch_number + 1;
        elsif i_ch_dn = '1' then
          s_ch_number <= s_ch_number - 1;
        end if;
      end if;
    end if;
  end process;

  p_volume: process(i_clk, i_reset_n, i_vol_rst)
  begin
    if i_reset_n = '0' or i_vol_rst = '0' then
      s_volume <= (others => '1');
    elsif i_clk'event and i_clk = '1' then
      if s_act = '1' then
        if i_vol_up = '1' then
          if s_volume /= to_unsigned(c_max_vol, TR_VOL_SIZE) then
            s_volume <= s_volume + 1;
          end if;
        elsif i_vol_dn = '1' then
          if s_volume /= to_unsigned(c_min_vol, TR_VOL_SIZE) then
            s_volume <= s_volume - 1;
          end if;
        end if;
      end if;
    end if;
  end process;

  p_pan: process(i_clk, i_reset_n, i_pan_rst)
  begin
    if i_reset_n = '0' or i_pan_rst = '0' then
      s_pan <= to_unsigned(c_mid_pan, TR_PAN_SIZE);
    elsif i_clk'event and i_clk = '1' then
      if s_act = '1' then
        if i_pan_up = '1' then
          if s_pan /= to_unsigned(c_max_pan, TR_PAN_SIZE) then
            s_pan <= s_pan + 1;
          end if;
        elsif i_pan_dn = '1' then
          if s_pan /= to_unsigned(c_min_pan, TR_PAN_SIZE) then
            s_pan <= s_pan - 1;
          end if;
        end if;
      end if;
    end if;
  end process;

  p_mute: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_mute <= '0';
    elsif i_clk'event and i_clk = '1' then
      if s_act = '1' then
        if i_mute = '1' then
          s_mute <= '1';
        else
          s_mute <= '0';
        end if;
      end if;
    end if;
  end process;

  p_solo: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_solo <= '0';
    elsif i_clk'event and i_clk = '1' then
      if i_solo = '1' then
        s_solo <= '1';
      else
        s_solo <= '0';
      end if;
    end if;
  end process;

  p_poly: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_poly <= '1';
    elsif i_clk'event and i_clk = '1' then
      if s_act = '1' then
        if i_poly_toggle = '1' then
          s_poly <= not(s_poly);
        end if;
      end if;
    end if;
  end process;

  p_omni: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_omni <= '0';
    elsif i_clk'event and i_clk = '1' then
      if s_act = '1' then
        if i_omni_toggle = '1' then
          s_omni <= not(s_omni);
        end if;
      end if;
    end if;
  end process;

  p_active_track: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_act <= '0';
    elsif i_clk'event and i_clk = '1' then
      if i_activate_tr = '1' then
        s_act <= '1';
      else
        s_act <= '0';
      end if;
    end if;
  end process;

  p_toggle_record: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_rec <= '0';
    elsif i_clk'event and i_clk = '1' then
      if s_act = '1' then
        if i_toggle_rec = '1' then
          s_rec <= not(s_rec);
        end if;
      end if;
    end if;
  end process;

end architecture;
