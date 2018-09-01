library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SAMPLE_MEMORY is
generic (
  g_mem_size      : integer := 8
);
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;
  i_enable        : in  std_logic;
  i_read_en       : in  std_logic;
  i_write_en      : in  std_logic;
  i_address       : in  std_logic_vector(g_mem_size - 1 downto 0);
  i_load_data     : in  std_logic_vector(31 downto 0);
  o_data          : out std_logic_vector(31 downto 0);
  o_mem_ready     : out std_logic;
  o_mem_error     : out std_logic
);
end entity;

architecture BHV of SAMPLE_MEMORY is

  constant c_rd_delay   : time := 30 ns;
  constant c_wr_delay   : time := 30 ns;

  type t_mem_state is (st_reset, st_idle, st_read, st_write, st_delay1, st_delay2, st_ready, st_error);

  type t_mem_array is array (2**g_mem_size - 1 downto 0) of std_logic_vector(31 downto 0);

  signal s_fsm        : t_mem_state;
  signal s_mem        : t_mem_array;

begin

  p_fsm: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_fsm <= st_reset;
    elsif rising_edge(i_clk) then
      case s_fsm is
        when st_reset =>
          s_fsm <= st_idle;
        when st_idle  =>
          if i_enable = '1' and i_write_en = '1' then
            s_fsm <= st_write;
          elsif i_enable = '1' and i_read_en = '1' then
            s_fsm <= st_read;
          else
            s_fsm <= st_idle;
          end if;
        when st_write =>
          s_fsm <= st_delay1;
        when st_read  =>
          s_fsm <= st_delay1;
        when st_delay1 =>
          s_fsm <= st_delay2;
        when st_delay2 =>
          s_fsm <= st_ready;
        when st_ready =>
          s_fsm <= st_idle;
        when st_error =>
          s_fsm <= st_error;
        when others   =>
          s_fsm <= st_reset;
      end case;
    end if;
  end process;

  p_mem: process(s_fsm)
  begin
    case s_fsm is
      when st_reset =>
        s_mem <= (others => (others => '1'));
        s_mem(0) <= "00000000000001000010000000100100";
        s_mem(1) <= "00000000000000000000000000000010";
        s_mem(2) <= "00000000000000000010000001111111";
        s_mem(3) <= "00000000000000000000000000000100";
        s_mem(128) <= "00000000000001000001010010100100";
        s_mem(129) <= "00000000000000000000000000000011";
        s_mem(130) <= "00000000000000000001010011111111";
        s_mem(131) <= "00000000000000000000000000000100";
        o_data <= (others => 'Z');
        o_mem_ready <= '0';
        o_mem_error <= '0';
      when st_idle  =>
        o_mem_ready <= '0';
        o_mem_error <= '0';
      when st_write =>
        s_mem(to_integer(unsigned(i_address))) <= i_load_data;
        o_mem_ready <= '0';
        o_mem_error <= '0';
      when st_read  =>
        o_data <= s_mem(to_integer(unsigned(i_address)));
        o_mem_ready <= '0';
        o_mem_error <= '0';
      when st_delay1 =>
        o_mem_ready <= '0';
        o_mem_error <= '0';
      when st_delay2 =>
        o_mem_ready <= '0';
        o_mem_error <= '0';
      when st_ready =>
        o_mem_ready <= '1';
        o_mem_error <= '0';
      when st_error =>
        o_mem_ready <= '0';
        o_mem_error <= '1';
      when others   =>
        s_mem <= (others => (others => '1'));
        o_mem_ready <= '0';
        o_mem_error <= '0';

    end case;
  end process;

end architecture;
