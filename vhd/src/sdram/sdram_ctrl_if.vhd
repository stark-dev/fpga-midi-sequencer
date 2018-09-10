library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SDRAM_CTRL_IF is
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
end entity;

architecture BHV of SDRAM_CTRL_IF is

  component SDRAM_CONTROLLER is
  port (
    i_clk           : in    std_logic;                        -- controller input clock
    i_reset_n       : in    std_logic;                        -- controller reset
    i_address       : in    std_logic_vector(24 downto 0);    -- input address (from device)
    i_data          : in    std_logic_vector(15 downto 0);    -- input data    (from device)
    i_read          : in    std_logic;                        -- read request  (from device)
    i_write         : in    std_logic;                        -- write request (from device)
    i_refresh       : in    std_logic;                        -- refresh request (from device)

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

    o_init          : out   std_logic;                        -- inif flag (signals that memory is ready)
    o_data          : out   std_logic_vector(15 downto 0);    -- output data (to device)
    o_ready         : out   std_logic;                        -- mem ready   (to device)
    o_error         : out   std_logic                         -- mem error   (to device)
  );
  end component;

  component SL_BUTTON is
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
  end component;

  type t_tester_fsm is (st_reset, st_init, st_idle, st_test_rd, st_test_wr, st_refresh, st_done);
  type t_dummy_data is array (7 downto 0) of std_logic_vector(15 downto 0);
  type t_dummy_addr is array (7 downto 0) of std_logic_vector(24 downto 0);

  constant c_max_ref_delay : natural := 70; -- test
  -- constant c_max_ref_delay : natural := 7000; -- max refresh delay is 7 us (must be 8192 refresh ticks within 64 ms)

  signal s_tester_fsm   : t_tester_fsm;

  signal s_test_wr_r    : natural range 0 to 8;

  signal s_dummy_data   : t_dummy_data;
  signal s_dummy_addr   : t_dummy_addr;

  signal s_dram_rd      : std_logic;
  signal s_dram_wr      : std_logic;
  signal s_dram_init    : std_logic;
  signal s_dram_ready   : std_logic;
  signal s_dram_error   : std_logic;
  signal s_dram_refresh : std_logic;
  signal s_dram_address : std_logic_vector(24 downto 0);
  signal s_dram_data_wr : std_logic_vector(15 downto 0);
  signal s_dram_data_rd : std_logic_vector(15 downto 0);
  signal s_dram_data_r  : std_logic_vector(15 downto 0);

  signal s_test_wr_dec  : std_logic;

  signal s_btn_short    : std_logic;
  signal s_btn_long     : std_logic;

  signal s_addres_cnt_r : natural range 0 to 8;

  signal s_refresh_cnt_r  : natural range 0 to c_max_ref_delay;

  signal s_read_req       : std_logic;
  signal s_read_set       : std_logic;
  signal s_read_rst       : std_logic;

begin

  o_init        <= s_dram_init;
  o_data        <= s_dram_data_r;

  s_test_wr_dec <= '1' when s_dram_ready = '1' and s_tester_fsm = st_test_wr else '0';

  DRAM_CTRL : SDRAM_CONTROLLER
  port map (
    i_clk           => i_clk,
    i_reset_n       => i_reset_n,
    i_address       => s_dram_address,
    i_data          => s_dram_data_wr,
    i_read          => s_dram_rd,
    i_write         => s_dram_wr,
    i_refresh       => s_dram_refresh,
    o_clk_en        => o_clk_en,
    o_cs_n          => o_cs_n,
    o_ras_n         => o_ras_n,
    o_cas_n         => o_cas_n,
    o_we_n          => o_we_n,
    o_dram_addr     => o_dram_addr,
    io_dram_dq      => io_dram_dq,
    o_dram_ba       => o_dram_ba,
    o_dram_ldqm     => o_dram_ldqm,
    o_dram_udqm     => o_dram_udqm,
    o_init          => s_dram_init,
    o_data          => s_dram_data_rd,
    o_ready         => s_dram_ready,
    o_error         => s_dram_error
  );

  BTN1 : SL_BUTTON
  -- generic map (32, 2500000, 25000000)
  generic map (8, 4, 150)
  port map(
    i_clk, i_reset_n, i_btn, s_btn_long, s_btn_short
  );

  p_fsm_state: process(i_reset_n, i_clk, s_dram_init, s_refresh_cnt_r, s_test_wr_r, s_read_req, s_dram_ready)
  begin
    if i_reset_n = '0' then
      s_tester_fsm <= st_reset;
    elsif i_clk'event and i_clk = '1' then
      case s_tester_fsm is
        when st_reset =>
          s_tester_fsm <= st_init;
        when st_init  =>
          if s_dram_init = '1' then
            s_tester_fsm <= st_idle;
          else
            s_tester_fsm <= st_init;
          end if;
        when st_idle  =>
          if s_refresh_cnt_r = c_max_ref_delay then
            s_tester_fsm <= st_refresh;
          elsif s_test_wr_r /= 8 then
            s_tester_fsm <= st_test_wr;
          elsif s_read_req = '1' then
            s_tester_fsm <= st_test_rd;
          else
            s_tester_fsm <= st_idle;
          end if;
        when st_test_wr =>
          if s_dram_ready = '1' then
            s_tester_fsm <= st_idle;
          else
            s_tester_fsm <= st_test_wr;
          end if;
        when st_test_rd =>
          if s_dram_ready = '1' then
            s_tester_fsm <= st_done;
          else
            s_tester_fsm <= st_test_rd;
          end if;
        when st_done  =>
          s_tester_fsm <= st_idle;
        when st_refresh =>
          if s_dram_ready = '1' then
            s_tester_fsm <= st_idle;
          else
            s_tester_fsm <= st_refresh;
          end if;
        when others   =>
          s_tester_fsm <= st_reset;
      end case;
    end if;
  end process;

  p_fsm_ctrl: process(s_tester_fsm, s_dummy_addr, s_dummy_data, s_dram_data_r, s_test_wr_r, s_addres_cnt_r, s_dram_data_rd)
  begin
    case s_tester_fsm is
      when st_reset =>
        s_dram_rd       <= '0';
        s_dram_wr       <= '0';
        s_dram_refresh  <= '0';
        s_dram_address  <= s_dummy_addr(0);
        s_dram_data_wr  <= s_dummy_data(0);
        s_dram_data_r   <= (others => '0');

      when st_init  =>
        s_dram_rd       <= '0';
        s_dram_wr       <= '0';
        s_dram_refresh  <= '0';
        s_dram_address  <= s_dummy_addr(0);
        s_dram_data_wr  <= s_dummy_data(0);
        s_dram_data_r   <= s_dram_data_r;

      when st_idle  =>
        s_dram_rd       <= '0';
        s_dram_wr       <= '0';
        s_dram_refresh  <= '0';
        s_dram_address  <= s_dummy_addr(0);
        s_dram_data_wr  <= s_dummy_data(0);
        s_dram_data_r   <= s_dram_data_r;

      when st_test_wr =>
        s_dram_rd       <= '0';
        s_dram_wr       <= '1';
        s_dram_refresh  <= '0';
        s_dram_address  <= s_dummy_addr(s_test_wr_r);
        s_dram_data_wr  <= s_dummy_data(s_test_wr_r);
        s_dram_data_r   <= s_dram_data_r;

      when st_test_rd =>
        s_dram_rd       <= '1';
        s_dram_wr       <= '0';
        s_dram_refresh  <= '0';
        s_dram_address  <= s_dummy_addr(s_addres_cnt_r);
        s_dram_data_wr  <= s_dummy_data(0);
        s_dram_data_r   <= s_dram_data_r;

      when st_done  =>
        s_dram_rd       <= '0';
        s_dram_wr       <= '0';
        s_dram_refresh  <= '0';
        s_dram_address  <= s_dummy_addr(s_addres_cnt_r);
        s_dram_data_wr  <= s_dummy_data(0);
        s_dram_data_r   <= s_dram_data_rd;

      when st_refresh =>
        s_dram_rd       <= '0';
        s_dram_wr       <= '0';
        s_dram_refresh  <= '1';
        s_dram_address  <= s_dummy_addr(0);
        s_dram_data_wr  <= s_dummy_data(0);
        s_dram_data_r   <= s_dram_data_r;

      when others   =>
        s_dram_rd       <= '0';
        s_dram_wr       <= '0';
        s_dram_refresh  <= '0';
        s_dram_address  <= s_dummy_addr(0);
        s_dram_data_wr  <= s_dummy_data(0);
        s_dram_data_r   <= s_dram_data_r;

    end case;
  end process;

  p_test_wr_cnt: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_test_wr_r <= 0;
    elsif i_clk'event and i_clk = '1' then
      if s_test_wr_dec = '1' then
        s_test_wr_r <= s_test_wr_r + 1;
      end if;
    end if;
  end process;

  p_address_cnt: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_addres_cnt_r <= 0;
    elsif i_clk'event and i_clk = '1'  then
      if s_btn_short = '1' then
        if s_addres_cnt_r = 7 then
          s_addres_cnt_r <= 0;
        else
          s_addres_cnt_r <= s_addres_cnt_r + 1;
        end if;
      end if;
    end if;
  end process;

  p_refresh: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_refresh_cnt_r <= 0;
    elsif i_clk'event and i_clk = '1' then
      if s_dram_init = '1' then
        if s_refresh_cnt_r /= c_max_ref_delay then
          s_refresh_cnt_r <= s_refresh_cnt_r + 1;
        elsif s_tester_fsm = st_refresh and s_dram_ready = '1' then
          s_refresh_cnt_r <= 0;
        end if;
      end if;
    end if;
  end process;

  p_read_req: process(s_read_set, s_read_rst)
  begin
    if s_read_rst = '1' then
      s_read_req <= '0';
    elsif s_read_set = '1' then
      s_read_req <= '1';
    end if;
  end process;

  p_read_req_set : process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_read_set <= '0';
    elsif i_clk'event and i_clk = '1' then
      if s_btn_short = '1' then
        s_read_set <= '1';
      else
        s_read_set <= '0';
      end if;
    end if;
  end process;

  p_read_req_rst : process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_read_rst <= '1';
    elsif i_clk'event and i_clk = '1' then
      if s_tester_fsm = st_test_rd and s_dram_ready = '1' then
        s_read_rst <= '1';
      else
        s_read_rst <= '0';
      end if;
    end if;
  end process;

  p_dummy_mem: process(i_reset_n)
  begin
    if i_reset_n = '0' then
      s_dummy_data(0)   <= "0000000000000000";
      s_dummy_data(1)   <= "1111111111111111";
      s_dummy_data(2)   <= "0101010101010101";
      s_dummy_data(3)   <= "1010101010101010";
      s_dummy_data(4)   <= "0011001100110011";
      s_dummy_data(5)   <= "1100110011001100";
      s_dummy_data(6)   <= "1101001101101001";
      s_dummy_data(7)   <= "0111011101110111";

      s_dummy_addr(0)   <= "0000000000000000000000000";
      s_dummy_addr(1)   <= "0000000000000000000000100";
      s_dummy_addr(2)   <= "0000000000000000000001000";
      s_dummy_addr(3)   <= "0000000000000100000000000";
      s_dummy_addr(4)   <= "1000000000000000000000000";
      s_dummy_addr(5)   <= "0100000000000000000000000";
      s_dummy_addr(6)   <= "0010000000000000000000000";
      s_dummy_addr(7)   <= "0001000000000000000000000";
    end if;
  end process;

end architecture;
