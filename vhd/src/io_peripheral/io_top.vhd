library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity io_top is
port (
  i_clk           : in  std_logic;
  i_reset_n       : in  std_logic;
  i_address       : in  std_logic;
  i_chip_select   : in  std_logic;
  i_write         : in  std_logic;
  i_writedata     : in  std_logic_vector(31 downto 0);
  o_readdata      : out std_logic_vector(31 downto 0);
  o_leds          : out std_logic_vector(7 downto 0)
);
end entity;

architecture bhv of io_top is

  component module is
  port (
    i_clk           : in  std_logic;
    i_reset_n       : in  std_logic;
    i_load_mask     : in  std_logic;
    i_load_leds     : in  std_logic;
    i_mask          : in  std_logic_vector(7 downto 0);
    i_leds          : in  std_logic_vector(7 downto 0);
    o_mask          : out std_logic_vector(7 downto 0);
    o_leds          : out std_logic_vector(7 downto 0)
  );
end component;

-- write enable from avalon
signal s_wr_en      : std_logic;

-- if regs load
signal s_load_mask  : std_logic;
signal s_load_leds  : std_logic;

-- registers
signal s_mask_out   : std_logic_vector(7 downto 0);
signal s_leds_out   : std_logic_vector(7 downto 0);

begin

  --read data
  o_readdata(31 downto 8) <= (others => '0');
  o_readdata(7 downto 0)  <= s_leds_out when i_address = '1' else s_mask_out;
  -- conduit
  o_leds            <= s_leds_out(7 downto 0);
  -- write signal
  s_wr_en           <= '1' when (i_chip_select = '1' and i_write = '1') else '0';
  -- load signals
  s_load_mask       <= '1' when (s_wr_en = '1' and i_address = '0') else '0';
  s_load_leds       <= '1' when (s_wr_en = '1' and i_address = '1') else '0';

  led_driver: module
  port map (
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,
    i_load_mask     => s_load_mask,
    i_load_leds     => s_load_leds,
    i_mask          => i_writedata(7 downto 0),
    i_leds          => i_writedata(7 downto 0),
    o_mask          => s_mask_out,
    o_leds          => s_leds_out
  );


end architecture;
