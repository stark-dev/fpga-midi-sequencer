library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_MEM_CTRL_IF is
end TB_MEM_CTRL_IF;

architecture TEST of TB_MEM_CTRL_IF is

  component SDRAM_CTRL_IF is
  port (
    i_clk           : in    std_logic;                        -- controller input clock (100 MHz)
    i_reset_n       : in    std_logic;                        -- interface reset
    i_btn           : in    std_logic;                        -- external button

    o_clk_en        : out   std_logic;                        -- clock enable
    o_cs_n          : out   std_logic;                        -- chip select (active low)
    o_ras_n         : out   std_logic;                        -- ras (active low)
    o_cas_n         : out   std_logic;                        -- cas (active low)
    o_we_n          : out   std_logic;                        -- we (active low)
    o_dram_addr     : out   std_logic_vector(12 downto 0);    -- address
    io_dram_dq      : inout std_logic_vector(15 downto 0);    -- data in/out
    o_dram_ba       : out   std_logic_vector(1 downto 0);     -- bank select
    o_dram_ldqm     : out   std_logic;                        -- lower byte data mask
    o_dram_udqm     : out   std_logic;                        -- upper byte data mask

    o_init          : out   std_logic;
    o_data          : out   std_logic_vector(15 downto 0)
  );
end component;

  constant MEM_SIZE       : integer := 10;

  constant c_ext_clock    : integer := 100000000;
  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_clock_half_p : time := c_clock * 1 ns;

  signal s_clk            : std_logic;
  signal s_rst            : std_logic;

  signal s_clk_en        : std_logic;
  signal s_cs_n          : std_logic;
  signal s_ras_n         : std_logic;
  signal s_cas_n         : std_logic;
  signal s_we_n          : std_logic;
  signal s_dram_addr     : std_logic_vector(12 downto 0);
  signal s_dram_dq       : std_logic_vector(15 downto 0);
  signal s_dram_ba       : std_logic_vector(1 downto 0);
  signal s_dram_ldqm     : std_logic;
  signal s_dram_udqm     : std_logic;

  signal s_init          : std_logic;
  signal s_o_data        : std_logic_vector(15 downto 0);

  signal s_btn           : std_logic;


begin

DUT : SDRAM_CTRL_IF
port map (
  i_clk           => s_clk,
  i_reset_n       => s_rst,
  i_btn           => s_btn,

  o_clk_en        => s_clk_en,
  o_cs_n          => s_cs_n,
  o_ras_n         => s_ras_n,
  o_cas_n         => s_cas_n,
  o_we_n          => s_we_n,
  o_dram_addr     => s_dram_addr,
  io_dram_dq      => s_dram_dq,
  o_dram_ba       => s_dram_ba,
  o_dram_ldqm     => s_dram_ldqm,
  o_dram_udqm     => s_dram_udqm,

  o_init          => s_init,
  o_data          => s_o_data
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
    s_dram_dq    <= (others => 'Z');
    -- reset
    s_rst         <= '0';
    s_btn         <= '1';
    wait for 10 ns;
    -- wait init
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 1600 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    s_dram_dq    <= (others => '1');
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;
    -- read button
    s_rst         <= '1';
    s_btn         <= '0';
    wait for 60 ns;
    s_rst         <= '1';
    s_btn         <= '1';
    wait for 100 ns;

    wait;
  end process;

end TEST;
