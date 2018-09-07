library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity TB_MEM_CTRL is
end TB_MEM_CTRL;

architecture TEST of TB_MEM_CTRL is

component SDRAM_CONTROLLER is
port (
  i_clk           : in    std_logic;                        -- controller input clock
  i_reset_n       : in    std_logic;                        -- controller reset
  i_address       : in    std_logic_vector(24 downto 0);    -- input address (from device)
  i_data          : in    std_logic_vector(31 downto 0);    -- input data    (from device)
  i_read          : in    std_logic;                        -- read request  (from device)
  i_write         : in    std_logic;                        -- write request (from device)

  o_clk_en        : out   std_logic;                        -- clock enable
  o_cs_n          : out   std_logic;                        -- chip select (active low)
  o_ras_n         : out   std_logic;                        -- ras (active low)
  o_cas_n         : out   std_logic;                        -- cas (active low)
  o_we_n          : out   std_logic;                        -- we (active low)
  o_dram_addr     : out   std_logic_vector(12 downto 0);    -- address
  io_dram_dq      : inout std_logic_vector(15 downto 0);  -- data in/out
  o_dram_ba       : out   std_logic_vector(1 downto 0);     -- bank select
  o_dram_ldqm     : out   std_logic;                        -- lower data mask byte
  o_dram_udqm     : out   std_logic;                        -- upper data mask byte

  o_init          : out   std_logic;                        -- inif flag (signals that memory is ready)
  o_data          : out   std_logic_vector(31 downto 0);    -- output data (to device)
  o_ready         : out   std_logic;                        -- mem ready   (to device)
  o_error         : out   std_logic                         -- mem error   (to device)
);
end component;

  constant MEM_SIZE       : integer := 10;

  constant c_ext_clock    : integer := 100000000;
  constant c_clock        : integer := 1000000000/(2*c_ext_clock);
  constant c_clock_half_p : time := c_clock * 1 ns;

  signal s_clk            : std_logic;
  signal s_rst            : std_logic;

  signal s_address       : std_logic_vector(24 downto 0);
  signal s_i_data        : std_logic_vector(31 downto 0);
  signal s_read          : std_logic;
  signal s_write         : std_logic;

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
  signal s_o_data        : std_logic_vector(31 downto 0);
  signal s_ready         : std_logic;
  signal s_error         : std_logic;


begin

DUT : SDRAM_CONTROLLER
port map (
  i_clk           => s_clk,
  i_reset_n       => s_rst,
  i_address       => s_address,
  i_data          => s_i_data,
  i_read          => s_read,
  i_write         => s_write,

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
  o_data          => s_o_data,
  o_ready         => s_ready,
  o_error         => s_error
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
    s_rst         <= '0';
    s_address     <= std_logic_vector(to_unsigned(0, 25));
    s_i_data      <= std_logic_vector(to_unsigned(255, 32));
    s_read        <= '0';
    s_write       <= '0';
    wait for 10 ns;

    s_rst         <= '1';
    s_address     <= std_logic_vector(to_unsigned(0, 25));
    s_i_data      <= std_logic_vector(to_unsigned(255, 32));
    s_read        <= '0';
    s_write       <= '0';
    wait for 800 ns;

    s_rst         <= '1';
    s_address     <= std_logic_vector(to_unsigned(0, 25));
    s_i_data      <= std_logic_vector(to_unsigned(255, 32));
    s_read        <= '1';
    s_write       <= '0';
    wait for 50 ns;

    s_rst         <= '1';
    s_address     <= std_logic_vector(to_unsigned(0, 25));
    s_i_data      <= std_logic_vector(to_unsigned(255, 32));
    s_read        <= '0';
    s_write       <= '0';
    wait for 10 ns;

    wait;
  end process;

end TEST;
