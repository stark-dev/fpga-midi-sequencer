library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SL_BUTTON is
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
end entity;

architecture BHV of SL_BUTTON is

  type t_button_state   is (st_reset, st_idle, st_pressed, st_run, st_stop, st_short, st_long);

  signal s_btn_state    : t_button_state;

  signal s_btn_press    : std_logic;
  signal s_btn_rel      : std_logic;

  -- counter signals
  signal s_cnt_out      : unsigned(g_size - 1 downto 0);
  signal s_cnt_en       : std_logic;
  signal s_cnt_rst      : std_logic;
  signal s_cnt_tc       : std_logic;
  signal s_cnt_tc_val   : unsigned(g_size - 1 downto 0);

begin

  p_btn_press: process(i_reset_n, s_btn_state, i_btn)
  begin
    if i_reset_n = '0' or s_btn_state = st_pressed then
      s_btn_press <= '0';
    elsif i_btn'event and i_btn = '0' then    -- pressed down
      s_btn_press <= '1';
    end if;
  end process;

  p_btn_release: process(i_reset_n, s_btn_state, i_btn)
  begin
    if i_reset_n = '0' or s_btn_state = st_stop then
      s_btn_rel <= '0';
    elsif i_btn'event and i_btn = '1' then    -- release up
      s_btn_rel <= '1';
    end if;
  end process;

  p_btn_fsm: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_btn_state <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_btn_state is
        when st_reset     =>
          s_btn_state     <= st_idle;
        when st_idle      =>
          if s_btn_press  = '1' then
            s_btn_state   <= st_pressed;
          else
            s_btn_state   <= st_idle;
          end if;
        when st_pressed   =>
          s_btn_state     <= st_run;
        when st_run       =>
          if s_btn_rel  = '1' or s_cnt_tc = '1' then
            s_btn_state   <= st_stop;
          else
            s_btn_state   <= st_run;
          end if;
        when st_stop      =>
          if s_cnt_out > to_unsigned(g_long, g_size) then
            s_btn_state     <= st_long;
          elsif s_cnt_out > to_unsigned(g_short, g_size) then
            s_btn_state     <= st_short;
          else
            s_btn_state     <= st_reset;
          end if;
        when st_long        =>
          s_btn_state     <= st_reset;
        when st_short       =>
          s_btn_state     <= st_reset;
        when others       =>
          s_btn_state     <= st_reset;
      end case;
    end if;
  end process;

  p_btn_ctrl: process(s_btn_state)
  begin
    case s_btn_state is
      when st_reset     =>
        s_cnt_rst <= '0';
        s_cnt_en  <= '0';

        o_long    <= '0';
        o_short   <= '0';
      when st_idle      =>
        s_cnt_rst <= '1';
        s_cnt_en  <= '0';

        o_long    <= '0';
        o_short   <= '0';
      when st_pressed   =>
        s_cnt_rst <= '1';
        s_cnt_en  <= '0';

        o_long    <= '0';
        o_short   <= '0';
      when st_run       =>
        s_cnt_rst <= '1';
        s_cnt_en  <= '1';

        o_long    <= '0';
        o_short   <= '0';
      when st_stop      =>
        s_cnt_rst <= '1';
        s_cnt_en  <= '0';

        o_long    <= '0';
        o_short   <= '0';
      when st_short     =>
        s_cnt_rst <= '1';
        s_cnt_en  <= '0';

        o_long    <= '0';
        o_short   <= '1';
      when st_long      =>
        s_cnt_rst <= '1';
        s_cnt_en  <= '0';

        o_long    <= '1';
        o_short   <= '0';
      when others       =>
        s_cnt_rst <= '0';
        s_cnt_en  <= '0';

        o_long    <= '0';
        o_short   <= '0';
    end case;
  end process;

  p_up_counter: process(i_clk, s_cnt_rst)
  begin
    if s_cnt_rst = '0' then
      s_cnt_out <= (others => '0');
    elsif i_clk'event and i_clk='1' then
      if s_cnt_en ='1' and s_cnt_tc = '0' then
        s_cnt_out <= s_cnt_out + 1;
      end if;
    end if;
  end process;

  s_cnt_tc_val <= (others => '1');
  s_cnt_tc <= '1' when s_cnt_out = s_cnt_tc_val else '0';

end architecture;
