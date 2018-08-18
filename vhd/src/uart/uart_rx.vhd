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
    g_parity_odd  : boolean := false;
    g_stop_bits   : integer := 1
  );
  port (
    i_uart_en     : in    std_logic;  -- UART enable
    i_uart_rst_n  : in    std_logic;  -- UART reset
    i_uart_in     : in    std_logic;  -- UART IN port
    i_clk         : in    std_logic;  -- external clock
    o_uart_data   : out   std_logic_vector(g_databits-1 downto 0);
    o_uart_end    : out   std_logic;
    o_uart_err    : out   std_logic
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

  component R_SHIFT_REGISTER is
  generic (
    N           : integer := 16);
  port (
    i_clk       : in   std_logic;
    i_reset_n   : in   std_logic;
    i_shift_en  : in   std_logic;
    i_serial_in : in   std_logic;
    o_par_out   : out  std_logic_vector(N-1 downto 0));
  end component;

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  type t_uart_st is (st_idle, st_start, st_data, st_parity, st_stop, st_end);

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  constant c_clock_divider : natural := (g_ext_clock / g_baud_rate) - 1;
  constant c_half_period   : natural := c_clock_divider / 2;

--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- rx bit counter
  signal s_rx_cnt_en        : std_logic;
  signal s_rx_cnt_rst       : std_logic;
  signal s_rx_cnt_tc        : std_logic;
  signal s_rx_cnt_out       : unsigned(up_int_log2(c_clock_divider)-1 downto 0);

  -- data counter
  signal s_dt_cnt_en        : std_logic;
  signal s_dt_cnt_rst       : std_logic;
  signal s_dt_cnt_tc        : std_logic;
  signal s_dt_cnt_out       : unsigned(up_int_log2(g_databits)-1 downto 0);

  -- parity bit register
  signal s_par_rg_en        : std_logic;
  signal s_par_rg_rst       : std_logic;
  signal s_par_rg_val       : std_logic;

  -- uart data shift reg
  signal s_sr_sh_en         : std_logic;
  signal s_sr_rst           : std_logic;
  signal s_sr_data_out      : std_logic_vector(g_databits-1 downto 0);

  -- uart
  signal s_uart_state   : t_uart_st;

  signal s_uart_clk_e   : std_logic;
  signal s_uart_clk     : std_logic;
  signal s_uart_clk_n   : std_logic;

  signal s_data_end     : std_logic;

  -- parity check
  signal s_par_check    : std_logic;
  signal s_par_error    : std_logic;
  signal s_par_bit      : std_logic;

begin

  uart_rx_bit_cnt : UP_COUNTER
  generic map (
    up_int_log2(c_clock_divider)
  )
  port map (
    i_clk       => i_clk,
    i_reset_n   => s_rx_cnt_rst,
    i_en        => s_rx_cnt_en,
    i_tc_value  => to_unsigned(c_clock_divider, up_int_log2(c_clock_divider)),
    o_q         => s_rx_cnt_out,
    o_tc        => s_rx_cnt_tc
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

  uart_data_sr : R_SHIFT_REGISTER
  generic map (
    g_databits
  )
  port map (
    i_clk       => s_uart_clk_n,
    i_reset_n   => s_sr_rst,
    i_shift_en  => s_sr_sh_en,
    i_serial_in => i_uart_in,
    o_par_out   => s_sr_data_out
  );

  p_uart_clock: process(s_uart_clk_e, s_rx_cnt_out)
  begin
    if s_uart_clk_e = '0' then
      s_uart_clk <= '0';
    else
      if s_rx_cnt_out > c_half_period then
        s_uart_clk <= '0';
      else
        s_uart_clk <= '1';
      end if;
    end if;
  end process;

  p_uart_fsm_state: process(i_uart_en, i_uart_rst_n, i_uart_in, s_rx_cnt_tc)
  begin
    if i_uart_rst_n = '0' then
      s_uart_state <= st_idle;
    elsif i_uart_en = '1' then
      if s_uart_state = st_idle and i_uart_in = '0' then -- start bit
        s_uart_state <= st_start;
      elsif s_rx_cnt_tc'event and s_rx_cnt_tc = '0' then
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
                s_uart_state <= st_stop;
              end if;
            end if;
          when st_parity =>
            if g_stop_bits = 0 then
              s_uart_state <= st_end;
            else
              s_uart_state <= st_stop;
            end if;
          when st_stop =>
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
      when st_idle   =>
        s_uart_clk_e <= '0';

        s_rx_cnt_en  <= '0';
        s_rx_cnt_rst <= '0';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '0';

        s_sr_rst     <= '0';
        s_sr_sh_en   <= '0';

        s_par_check  <= '0';
        s_par_rg_en  <= '0';
        s_par_rg_rst <= '0';

        s_data_end   <= '0';

      when st_start  =>
        s_uart_clk_e <= '1';

        s_rx_cnt_en  <= '1';
        s_rx_cnt_rst <= '1';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '0';

        s_sr_rst     <= '0';
        s_sr_sh_en   <= '0';

        s_par_check  <= '0';
        s_par_rg_en  <= '0';
        s_par_rg_rst <= '1';

        s_data_end   <= '0';

      when st_data   =>
        s_uart_clk_e <= '1';

        s_rx_cnt_en  <= '1';
        s_rx_cnt_rst <= '1';

        s_dt_cnt_en  <= '1';
        s_dt_cnt_rst <= '1';

        s_sr_rst     <= '1';
        s_sr_sh_en   <= '1';

        s_par_check  <= '0';
        s_par_rg_en  <= '0';
        s_par_rg_rst <= '1';

        s_data_end   <= '0';

      when st_parity =>
        s_uart_clk_e <= '1';

        s_rx_cnt_en  <= '1';
        s_rx_cnt_rst <= '1';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '0';

        s_sr_rst     <= '1';
        s_sr_sh_en   <= '0';

        s_par_check  <= '0';
        s_par_rg_en  <= '1';
        s_par_rg_rst <= '1';

        s_data_end   <= '0';

      when st_stop   =>
        s_uart_clk_e <= '1';

        s_rx_cnt_en  <= '1';
        s_rx_cnt_rst <= '1';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '0';

        s_sr_rst     <= '1';
        s_sr_sh_en   <= '0';

        s_par_check  <= '0';
        s_par_rg_en  <= '0';
        s_par_rg_rst <= '1';

        s_data_end   <= '0';

      when st_end    =>
        s_uart_clk_e <= '0';

        s_rx_cnt_en  <= '1';
        s_rx_cnt_rst <= '1';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '0';

        s_sr_rst     <= '1';
        s_sr_sh_en   <= '0';

        if g_parity = true then
          s_par_check  <= '1';
        else
          s_par_check  <= '0';
        end if;
        s_par_rg_en  <= '0';
        s_par_rg_rst <= '1';

        s_data_end   <= '1';

      when others    =>
        s_uart_clk_e <= '0';

        s_rx_cnt_en  <= '0';
        s_rx_cnt_rst <= '0';

        s_dt_cnt_en  <= '0';
        s_dt_cnt_rst <= '0';

        s_sr_rst     <= '0';
        s_sr_sh_en   <= '0';

        s_par_check  <= '0';
        s_par_rg_en  <= '0';
        s_par_rg_rst <= '0';

        s_data_end   <= '0';
    end case;
  end process;

  p_parity_check_reg: process(s_par_rg_en, s_par_rg_rst, s_uart_clk_n)
  begin
    if s_par_rg_rst = '0' then
      s_par_rg_val <= '0';
    elsif s_par_rg_en ='1' then
      if s_uart_clk_n'event and s_uart_clk_n = '1' then
        s_par_rg_val <= i_uart_in;
      end if;
    end if;
  end process;


  p_parity_check: process(s_par_rg_val, s_par_bit)
  begin
    if g_parity_odd = false then
      s_par_error <= s_par_check and (s_par_bit xor s_par_rg_val);
    else
      s_par_error <= s_par_check and (s_par_bit xnor s_par_rg_val);
    end if;
  end process;

  s_uart_clk_n  <= not(s_uart_clk);
  s_par_bit     <= xor_reduce(s_sr_data_out);

  o_uart_data   <= s_sr_data_out;
  o_uart_end    <= s_data_end;
  o_uart_err    <= s_par_error;

end architecture RTL;
