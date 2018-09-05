library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity DISPLAY_CTRL is
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
end entity;

architecture BHV of DISPLAY_CTRL is

  signal s_display_array  : t_display_array;

  signal s_ts_frac_1      : std_logic_vector(3 downto 0);
  signal s_ts_frac_2      : std_logic_vector(3 downto 0);

  signal s_ts_secs_1      : std_logic_vector(3 downto 0);
  signal s_ts_secs_2      : std_logic_vector(3 downto 0);

  signal s_disp_4_4       : t_display_out;

  signal s_disp_3_4       : t_display_out;
  signal s_disp_3_3       : t_display_out;

  signal s_disp_2_4       : t_display_out;

  signal s_disp_1_4       : t_display_out;
  signal s_disp_1_4_in    : std_logic_vector(3 downto 0);

  signal s_disp_0_4       : t_display_out;
  signal s_disp_0_4_in    : std_logic_vector(3 downto 0);

  signal s_disp_0_3       : t_display_out;

  signal s_display_en     : std_logic;

  component DISPLAY_MUX_3 is
  port (
    i_enable  : in  std_logic;
    i_input   : in  std_logic_vector(2 downto 0);
    o_display : out t_display_out
  );
  end component;

  component DISPLAY_MUX_4 is
  port (
    i_enable  : in  std_logic;
    i_input   : in  std_logic_vector(3 downto 0);
    o_display : out t_display_out
  );
  end component;

begin

  o_display_a       <= s_display_array;

  s_ts_frac_1       <= i_ts_frac(ST_TSF_SIZE - 5 downto ST_TSF_SIZE - 8);
  s_ts_frac_2       <= i_ts_frac(ST_TSF_SIZE - 1 downto ST_TSF_SIZE - 4);

  s_ts_secs_1       <= i_ts_secs(3 downto 0);
  s_ts_secs_2       <= i_ts_secs(7 downto 4);

  -- first number is display index, second is display input width
  MUX_4_4 : DISPLAY_MUX_4
  port map(s_display_en, s_ts_secs_2, s_disp_4_4);

  MUX_3_4 : DISPLAY_MUX_4
  port map(s_display_en, s_ts_secs_1, s_disp_3_4);

  MUX_3_3 : DISPLAY_MUX_3
  port map(s_display_en, i_main_vol(6 downto 4), s_disp_3_3);

  MUX_2_4 : DISPLAY_MUX_4
  port map(s_display_en, i_main_vol(3 downto 0), s_disp_2_4);

  MUX_1_4 : DISPLAY_MUX_4
  port map(s_display_en, s_disp_1_4_in, s_disp_1_4);

  MUX_0_4 : DISPLAY_MUX_4
  port map(s_display_en, s_disp_0_4_in, s_disp_0_4);

  MUX_0_3 : DISPLAY_MUX_3
  port map(s_display_en, i_active_tr, s_disp_0_3);

  p_display_en: process(i_state)
  begin
    if i_state = st_reset then
      s_display_en <= '0';
    else
      s_display_en <= '1';
    end if;
  end process;

  p_disp_1_4_in: process(i_state, i_menu, s_ts_frac_2, i_track_patch, i_track_vol, i_track_pan)
  begin
    case i_state is
      when st_play    =>
        s_disp_1_4_in <= s_ts_frac_2;
      when st_rec     =>
        s_disp_1_4_in <= s_ts_frac_2;
      when st_menu    =>
        case i_menu is
          when op_patch     =>
            s_disp_1_4_in <= '0' & i_track_patch(6 downto 4);
          when op_track_vol =>
            s_disp_1_4_in <= '0' & i_track_vol(6 downto 4);
          when op_pan       =>
            s_disp_1_4_in <= '0' & i_track_pan(6 downto 4);
          when others       =>
            s_disp_1_4_in <= (others => '0');
        end case;
      when others     =>
        s_disp_1_4_in <= (others => '0');
    end case;
  end process;

  p_disp_0_4_in: process(i_state, i_menu, s_ts_frac_1, i_track_patch, i_track_vol, i_track_pan)
  begin
    case i_state is
      when st_menu    =>
        case i_menu is
          when op_patch     =>
            s_disp_0_4_in <= i_track_patch(3 downto 0);
          when op_track_vol =>
            s_disp_0_4_in <= i_track_vol(3 downto 0);
          when op_pan       =>
            s_disp_0_4_in <= i_track_pan(3 downto 0);
          when others       =>
            s_disp_0_4_in <= (others => '0');
        end case;
      when others     =>
        s_disp_0_4_in <= (others => '0');
    end case;
  end process;

  p_display_5_control: process(i_state, i_menu, i_track_rec)
  begin
    case i_state is
      when st_idle    =>
        if i_track_rec = '0' then
          s_display_array(5)  <= ds_0;
        else
          s_display_array(5)  <= ds_R;
        end if;
      when st_play    =>
        s_display_array(5)  <= ds_S;
      when st_rec     =>
        s_display_array(5)  <= ds_S;
      when st_menu    =>
        case i_menu is
          when op_track     =>
            s_display_array(5)  <= ds_T;
          when op_patch     =>
            s_display_array(5)  <= ds_P;
          when op_track_vol =>
            s_display_array(5)  <= ds_V;
          when op_pan       =>
            s_display_array(5)  <= ds_L;
          when op_poly      =>
            s_display_array(5)  <= ds_P;
          when op_omni      =>
            s_display_array(5)  <= ds_O;
          when others       =>
            s_display_array(5)  <= ds_OFF;
        end case;
      when others     =>
        s_display_array(5)  <= ds_OFF;
    end case;
  end process;

  p_display_4_control: process(i_state, i_menu, s_disp_4_4)
  begin
    case i_state is
      when st_idle    =>
        s_display_array(4)  <= ds_V;
      when st_play    =>
        s_display_array(4)  <= s_disp_4_4;
      when st_rec     =>
        s_display_array(4)  <= s_disp_4_4;
      when st_menu    =>
        case i_menu is
          when op_track     =>
            s_display_array(4)  <= ds_R;
          when op_patch     =>
            s_display_array(4)  <= ds_A;
          when op_track_vol =>
            s_display_array(4)  <= ds_O;
          when op_pan       =>
            s_display_array(4)  <= ds_R;
          when op_poly      =>
            s_display_array(4)  <= ds_O;
          when op_omni      =>
            s_display_array(4)  <= ds_M;
          when others       =>
            s_display_array(4)  <= ds_OFF;
        end case;
      when others     =>
        s_display_array(4)  <= ds_OFF;
    end case;
  end process;

  p_display_3_control: process(i_state, i_menu, s_disp_3_4, s_disp_3_3)
  begin
    case i_state is
      when st_idle    =>
        s_display_array(3)  <= s_disp_3_3;
      when st_play    =>
        s_display_array(3)  <= s_disp_3_4;
      when st_rec     =>
        s_display_array(3)  <= s_disp_3_4;
      when st_menu    =>
        case i_menu is
          when op_track     =>
            s_display_array(3)  <= ds_OFF;
          when op_patch     =>
            s_display_array(3)  <= ds_T;
          when op_track_vol =>
            s_display_array(3)  <= ds_L;
          when op_pan       =>
            s_display_array(3)  <= ds_OFF;
          when op_poly      =>
            s_display_array(3)  <= ds_L;
          when op_omni      =>
            s_display_array(3)  <= ds_N;
          when others       =>
            s_display_array(3)  <= ds_OFF;
        end case;
      when others     =>
        s_display_array(3)  <= ds_OFF;
    end case;
  end process;

  p_display_2_control: process(i_state, i_menu, s_disp_2_4)
  begin
    case i_state is
      when st_idle    =>
        s_display_array(2)  <= s_disp_2_4;
      when st_play    =>
        s_display_array(2)  <= ds_F;
      when st_rec     =>
        s_display_array(2)  <= ds_F;
      when st_menu    =>
        case i_menu is
          when op_track     =>
            s_display_array(2)  <= ds_OFF;
          when op_patch     =>
            s_display_array(2)  <= ds_OFF;
          when op_track_vol =>
            s_display_array(2)  <= ds_OFF;
          when op_pan       =>
            s_display_array(2)  <= ds_OFF;
          when op_poly      =>
            s_display_array(2)  <= ds_Y;
          when op_omni      =>
            s_display_array(2)  <= ds_I;
          when others       =>
            s_display_array(2)  <= ds_OFF;
        end case;
      when others     =>
        s_display_array(2)  <= ds_OFF;
    end case;
  end process;

  p_display_1_control: process(i_state, i_menu, s_disp_1_4)
  begin
    case i_state is
      when st_idle    =>
        s_display_array(1)  <= ds_T;
      when st_play    =>
        s_display_array(1)  <= s_disp_1_4;
      when st_rec     =>
        s_display_array(1)  <= s_disp_1_4;
      when st_menu    =>
        case i_menu is
          when op_track     =>
            s_display_array(1)  <= ds_OFF;
          when op_patch     =>
            s_display_array(1)  <= s_disp_1_4;
          when op_track_vol =>
            s_display_array(1)  <= s_disp_1_4;
          when op_pan       =>
            s_display_array(1)  <= s_disp_1_4;
          when op_poly      =>
            s_display_array(1)  <= ds_OFF;
          when op_omni      =>
            s_display_array(1)  <= ds_OFF;
          when others       =>
            s_display_array(1)  <= ds_OFF;
        end case;
      when others     =>
        s_display_array(1)  <= ds_OFF;
    end case;
  end process;

  p_display_0_control: process(i_state, i_menu, s_disp_0_4, s_disp_0_3, i_track_omni, i_track_poly)
  begin
    case i_state is
      when st_idle    =>
        s_display_array(0)  <= s_disp_0_3;
      when st_play    =>
        s_display_array(0)  <= ds_P;
      when st_rec     =>
        s_display_array(0)  <= ds_R;
      when st_menu    =>
        case i_menu is
          when op_track     =>
            s_display_array(0)  <= s_disp_0_3;
          when op_patch     =>
            s_display_array(0)  <= s_disp_0_4;
          when op_track_vol =>
            s_display_array(0)  <= s_disp_0_4;
          when op_pan       =>
            s_display_array(0)  <= s_disp_0_4;
          when op_poly      =>
            if i_track_poly = '1' then
              s_display_array(0)  <= ds_1;
            else
              s_display_array(0)  <= ds_0;
            end if;
          when op_omni      =>
            if i_track_omni = '1' then
              s_display_array(0)  <= ds_1;
            else
              s_display_array(0)  <= ds_0;
            end if;
          when others       =>
            s_display_array(0)  <= ds_OFF;
        end case;
      when others     =>
        s_display_array(0)  <= ds_OFF;
    end case;
  end process;

end architecture;
