library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity UART_RX is
  generic (
    g_ext_clock   : natural := 500000000;
    g_baud_rate   : natural := 9600;
    g_databits    : natural := 8;
    g_parity      : boolean := false;
    g_stop_bits   : integer := 1
  );
  port (
    i_uart_en     : in    std_logic;  -- UART enable
    i_uart_rst_n  : in    std_logic;  -- UART reset
    i_uart_in     : in    std_logic;  -- UART IN port
    i_clk         : in    std_logic;  -- external clock
    o_uart_data   : out   std_logic_vector(g_databits-1 downto 0);
    o_uart_end    : out   std_logic
  );
end UART_RX;

architecture RTL of UART_RX is
--------------------------------------------------------------------------------
-- components
--------------------------------------------------------------------------------
  component UP_COUNTER is
    generic (
      N           : integer := 16);
    port (
      i_clk       : in   std_logic;
      i_reset_n   : in   std_logic;
      i_en        : in   std_logic;
      i_tc_value  : in   unsigned(N-1 downto 0);
      o_q         : out  unsigned(N-1 downto 0);
      o_tc        : out  std_logic);
  end component;

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  type t_uart_st is (st_idle, st_start, st_data, st_parity, st_stop1, st_stop2, st_end);

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  constant c_clock_divider : natural := g_ext_clock / (g_baud_rate * 2);

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------

  -- clock counter
  signal s_cl_cnt_en        : std_logic;
  signal s_cl_cnt_rst       : std_logic;
  signal s_cl_cnt_tc        : std_logic;
  signal s_cl_cnt_out       : unsigned(up_int_log2(c_clock_divider)-1 downto 0);

  -- data counter
  signal s_dt_cnt_en        : std_logic;
  signal s_dt_cnt_rst       : std_logic;
  signal s_dt_cnt_tc        : std_logic;
  signal s_dt_cnt_out       : unsigned(up_int_log2(g_databits)-1 downto 0);

  -- uart
  signal s_uart_state   : t_uart_st;
  signal s_uart_clk     : std_logic;
  signal s_uart_data    : std_logic_vector(g_databits-1 downto 0);

begin

  uart_clock_cnt : UP_COUNTER
  generic map (
    up_int_log2(c_clock_divider)
  )
  port map (
    i_clk       => i_clk,
    i_reset_n   => s_cl_cnt_rst,
    i_en        => s_cl_cnt_en,
    i_tc_value  => to_unsigned(c_clock_divider, up_int_log2(c_clock_divider)),
    o_q         => s_cl_cnt_out,
    o_tc        => s_cl_cnt_tc
  );

  uart_data_cnt : UP_COUNTER
  generic map (
    up_int_log2(g_databits)
  )
  port map (
    i_clk       => s_uart_clk,
    i_reset_n   => s_dt_cnt_rst,
    i_en        => s_dt_cnt_en,
    i_tc_value  => to_unsigned(g_databits-1, up_int_log2(g_databits)),
    o_q         => s_dt_cnt_out,
    o_tc        => s_dt_cnt_tc
  );

  p_uart_clock: process(i_uart_rst_n, s_cl_cnt_tc)
  begin
    if i_uart_rst_n = '0' then
      s_uart_clk <= '1';
    elsif(s_cl_cnt_tc'event and s_cl_cnt_tc = '1') then
      s_uart_clk <= not(s_uart_clk);
    end if;
  end process;

  p_uart_fsm_state: process(i_uart_en, i_uart_rst_n, i_uart_in, s_uart_clk)
  begin
    if i_uart_rst_n = '0' then
      s_uart_state <= st_idle;
    elsif i_uart_en = '1' then
      if s_uart_state = st_idle and i_uart_in = '0' then -- start bit
        s_uart_state <= st_start;
      elsif s_uart_clk'event and s_uart_clk = '1' then
        case s_uart_state is
          when st_start =>
            s_uart_state <= st_data;
          when st_data =>
            if s_dt_cnt_tc = '1' then
              if g_parity = true then
                s_uart_state <= st_parity;
              elsif g_stop_bits = 0 then
                s_uart_state <= st_end;
              else
                s_uart_state <= st_stop1;
              end if;
            end if;
          when st_parity =>
            if g_stop_bits = 0 then
              s_uart_state <= st_end;
            else
              s_uart_state <= st_stop1;
            end if;
          when st_stop1 =>
            if g_stop_bits = 2 then
              s_uart_state <= st_stop2;
            else
              s_uart_state <= st_end;
            end if;
          when st_stop2 =>
            s_uart_state <= st_end;
          when st_end =>
            s_uart_state <= st_idle;
          when others =>
            s_uart_state <= st_idle;
        end case;
      end if;
    end if;
  end process;

  p_uart_ctrl: process(s_uart_state)
  begin
    case s_uart_state is
      when st_idle    =>
        s_cl_cnt_en  <= '0';
        s_cl_cnt_rst <= '0';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '0';

        o_uart_end   <= '0';
      when st_start   =>
        s_cl_cnt_en  <= '1';
        s_cl_cnt_rst <= '1';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '0';

        o_uart_end   <= '0';
      when st_data    =>
        s_cl_cnt_en  <= '1';
        s_cl_cnt_rst <= '1';

        s_dt_cnt_en  <= '1';
        s_dt_cnt_rst <= '1';

        o_uart_end   <= '0';
      when st_parity =>
        s_cl_cnt_en  <= '1';
        s_cl_cnt_rst <= '1';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '1';

        o_uart_end   <= '0';
      when st_stop1   =>
        s_cl_cnt_en  <= '1';
        s_cl_cnt_rst <= '1';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '1';

        o_uart_end   <= '0';
      when st_stop2   =>
        s_cl_cnt_en  <= '1';
        s_cl_cnt_rst <= '1';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '1';

        o_uart_end   <= '0';
      when st_end     =>
        s_cl_cnt_en  <= '1';
        s_cl_cnt_rst <= '1';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '1';

        o_uart_end   <= '1';
      when others    =>
        s_cl_cnt_en  <= '0';
        s_cl_cnt_rst <= '0';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '0';

        o_uart_end   <= '0';
    end case;
  end process;

end architecture RTL;
