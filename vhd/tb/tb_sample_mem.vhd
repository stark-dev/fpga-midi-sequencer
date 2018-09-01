library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_SAMPLE_MEM is
end TB_SAMPLE_MEM;

architecture TEST of TB_SAMPLE_MEM is

component SAMPLE_MEMORY is
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
end component;

  constant MEM_SIZE       : integer := 10;

  constant c_ext_clock    : integer := 50000000;
  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_clock_half_p : time := c_clock * 1 ns;

  signal s_clk            : std_logic;
  signal s_rst            : std_logic;

  signal s_chip_sel       : std_logic;
  signal s_read_en        : std_logic;
  signal s_write_en       : std_logic;
  signal s_mem_ready      : std_logic;
  signal s_mem_error      : std_logic;

  signal s_address        : std_logic_vector(MEM_SIZE - 1 downto 0);
  signal s_load_data      : std_logic_vector(31 downto 0);
  signal s_out_data       : std_logic_vector(31 downto 0);


begin

  MEM : SAMPLE_MEMORY
  generic map(MEM_SIZE)
  port map (
    i_clk           => s_clk,
    i_reset_n       => s_rst,
    i_enable        => s_chip_sel,
    i_read_en       => s_read_en,
    i_write_en      => s_write_en,
    i_address       => s_address,
    i_load_data     => s_load_data,
    o_data          => s_out_data,
    o_mem_ready     => s_mem_ready,
    o_mem_error     => s_mem_error
  );


  clock_gen : process
  begin
    s_clk <= '0';
    wait for c_clock_half_p;
    s_clk <= '1';
    wait for c_clock_half_p;
  end process;

  input_gen: process
  begin
    s_rst         <= '1';
    s_chip_sel    <= '0';
    s_read_en     <= '0';
    s_write_en    <= '0';
    s_address     <= (others => '0');
    s_load_data   <= (others => '0');
    wait for 100 ns;

    s_rst         <= '0';
    s_chip_sel    <= '0';
    s_read_en     <= '0';
    s_write_en    <= '0';
    s_address     <= (others => '0');
    s_load_data   <= (others => '0');
    wait for 60 ns;

    s_rst         <= '1';
    s_chip_sel    <= '0';
    s_read_en     <= '0';
    s_write_en    <= '0';
    s_address     <= (others => '0');
    s_load_data   <= (others => '0');
    wait for 60 ns;

    s_rst         <= '1';
    s_chip_sel    <= '1';
    s_read_en     <= '0';
    s_write_en    <= '0';
    s_address     <= (others => '0');
    s_load_data   <= (others => '0');
    wait for 60 ns;

    s_rst         <= '1';
    s_chip_sel    <= '1';
    s_read_en     <= '0';
    s_write_en    <= '1';
    s_address     <= std_logic_vector(to_unsigned(0, MEM_SIZE));
    s_load_data   <= std_logic_vector(to_unsigned(255, 32));
    wait for 3*c_clock_half_p;

    s_rst         <= '1';
    s_chip_sel    <= '1';
    s_read_en     <= '0';
    s_write_en    <= '0';
    s_address     <= std_logic_vector(to_unsigned(0, MEM_SIZE));
    s_load_data   <= std_logic_vector(to_unsigned(255, 32));
    wait for 50 ns;

    s_rst         <= '1';
    s_chip_sel    <= '1';
    s_read_en     <= '1';
    s_write_en    <= '0';
    s_address     <= std_logic_vector(to_unsigned(0, MEM_SIZE));
    s_load_data   <= std_logic_vector(to_unsigned(255, 32));
    wait for 3*c_clock_half_p;

    s_rst         <= '1';
    s_chip_sel    <= '1';
    s_read_en     <= '0';
    s_write_en    <= '0';
    s_address     <= std_logic_vector(to_unsigned(0, MEM_SIZE));
    s_load_data   <= std_logic_vector(to_unsigned(255, 32));
    wait for 50 ns;

    wait;
  end process;

end TEST;
