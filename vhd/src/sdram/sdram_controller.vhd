library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

--------------------------------------------
--------------------------------------------
-- SDRAM package info
-- - IS42S1632OD
-- - 8M x 16 x 4 banks
--------------------------------------------
--------------------------------------------
-- SDRAM timing parameters
-- - clk cycle time
--   | cas latency  |   5 |   6 |   7 | unit
--------------------------------------------
--   - cas = 3      |   5 |   6 |   7 | ns
--   - cas = 2      |  10 |  10 | 7.5 | ns
--------------------------------------------
--   - cas = 3      | 200 | 167 | 143 | MHz
--   - cas = 2      | 100 | 100 | 133 | MHz
--------------------------------------------
--------------------------------------------
-- Address table
-- - Auto precharge: A10
-- - Row address   : 8k (A0 - A12)
-- - Col address   : 1k (A0 - A9)
-- - Refresh count : 8K / 64 ms
--------------------------------------------
--------------------------------------------
-- Timing
-- - Tac (access time from clock)
--   | cas latency  |   5 |   6 |   7 | unit
--------------------------------------------
--   - cas = 3      |   5 | 5.4 | 5.4 | ns
--   - cas = 2      |   6 |   6 | 5.4 | ns
--------------------------------------------
--   |              |   5 |   6 |   7 | unit
--------------------------------------------
-- - Trc (REF to REF/ ACT to ACT)
--   -              |  55 |  60 |  60 | ns
-- - Tras (ACT to PRE)
--   -              |  38 |  42 |  37 | ns
-- - Trp (PRE to ACT)
--   -              |  15 |  18 |  15 | ns
-- - Trcd (active command to read/write)
--   -              |  15 |  18 |  15 | ns
-- - Txsr (self refresh exit time)
--   -              |  60 |  70 |  60 | ns
-- - Tref (refresh cycle time)
--   -              |  64 |  64 |  64 | ms
-- - Tmrd (mode register program time)
--   -              |  10 |  12 |  14 | ms
--------------------------------------------
--------------------------------------------

entity SDRAM_CONTROLLER is
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
end entity;

architecture BHV of SDRAM_CONTROLLER is

--------------------------------------------------------------------------------
-- components
--------------------------------------------------------------------------------
  component REGISTER_N is
  generic (
    N           : integer := 16);
  port (
    i_clk         : in   std_logic;
    i_reset_n     : in   std_logic;
    i_load_en     : in   std_logic;
    i_par_in      : in   std_logic_vector(N-1 downto 0);
    o_par_out     : out  std_logic_vector(N-1 downto 0));
  end component;

  component UP_COUNTER_MOD is
    generic (
      N           : integer := 16
    );
    port (
      i_clk       : in   std_logic;
      i_reset_n   : in   std_logic;
      i_en        : in   std_logic;
      i_tc_value  : in   unsigned(N-1 downto 0);
      o_cnt_out   : out  unsigned(N-1 downto 0);
      o_chain_en  : out  std_logic
    );
  end component;

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------
  -- not used, internal dram fsm
  type t_dram_fsm is (
    st_pwr,     -- power up
    st_pre,     -- precharge
    st_idle,    -- idle
    st_mrs,     -- mode register set
    st_self,    -- self refresh
    st_cbr,     -- (auto) refresh
    st_pwrd,    -- power down
    st_row,     -- row active
    st_apwr,    -- active power down
    st_wr,      -- write
    st_rd,      -- read
    st_wr_s,    -- write suspend
    st_rd_s,    -- read suspend
    st_wra,     -- write auto precharge
    st_rda,     -- read auto precharge
    st_wra_s,   -- write auto precharge suspend
    st_rda_s    -- read auto precharge suspend
  );

  type t_ctrl_fsm is (
    st_rst,
    st_init_wait,
    st_init_pre,
    st_init_pre_wait,
    st_init_ref,
    st_init_mode,
    st_idle,
    st_ref,
    st_activate,
    st_rcd,
    st_rw,
    st_ras1,
    st_ras2,
    st_pre,
    st_done
  );

  type t_dram_cmd is (
    CMD_DESL, CMD_NOP, CMD_BST, CMD_RD,
    CMD_RD_AP, CMD_WR, CMD_WR_AP, CMD_ACT,
    CMD_PRE, CMD_PALL, CMD_REF, CMD_SELF,
    CMD_MSR
  );

  type t_dram_dqm is (
    DQM_OUT_EN, DQM_OUT_DIS, DQM_UP_EN, DQM_UP_DIS,
    DQM_LOW_EN, DQM_LOW_DIS
  );


  subtype   MRS_BURST_LEN_RANGE     is integer range 2 downto 0;
  constant  MRS_BURST_TYPE          :  integer := 3;
  subtype   MRS_LATENCY_MODE_RANGE  is integer range 6 downto 4;
  subtype   MRS_OP_MODE_RANGE       is integer range 8 downto 7;
  constant  MRS_WR_BURST_MODE       :  integer := 9;
  subtype   MRS_RESERVED_RANGE      is integer range 14 downto 10;

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------
  -- auto precharge
  constant c_autopre                : boolean := false;
  -- burst length
  constant c_burst_1                : std_logic_vector(2 downto 0) := "000";
  constant c_burst_2                : std_logic_vector(2 downto 0) := "001";
  constant c_burst_8                : std_logic_vector(2 downto 0) := "011";
  constant c_burst_4                : std_logic_vector(2 downto 0) := "010";
  constant c_burst_full             : std_logic_vector(2 downto 0) := "111";
  -- burst type
  constant c_burst_seq              : std_logic := '0';
  constant c_burst_int              : std_logic := '1';
  -- latency mode
  constant c_cas_2                  : std_logic_vector(2 downto 0) := "010";
  constant c_cas_3                  : std_logic_vector(2 downto 0) := "011";
  -- operating mode
  constant c_std_op                 : std_logic_vector(1 downto 0) := "00";
  -- write burst mode
  constant c_wr_burst_prog          : std_logic := '0';
  constant c_wr_burst_single        : std_logic := '1';

  -- init delay wait counter
  -- constant c_200_us_delay           : integer := 20000; -- wait 200 us
  constant c_200_us_delay           : integer := 10; -- TODO only for test
  -- active refresh delay
  constant c_auto_rf_delay          : integer := 6;    -- auto refesh to next active (55 ns)
  -- self refresh delay
  constant c_self_rf_delay          : integer := 6;    -- self refesh delay (60 ns)
  -- init refresh cycles
  constant c_init_ref_cyles         : integer := 8;
  -- mode register program cycles
  constant c_mrd_cycles             : integer := 2;
  -- active to read/write delay
  constant c_trcd_cycles            : integer := 2;
  -- precharge to active delay
  constant c_trp_cycles             : integer := 2;

  -- burst option
  constant c_burst                  : integer := 1;


--------------------------------------------------------------------------------
-- signals
--------------------------------------------------------------------------------
  -- fsm signals
  signal s_dram_fsm         : t_ctrl_fsm;

  -- dram command signals
  signal s_dram_cmd         : t_dram_cmd;

  -- dram data mask
  signal s_dram_dqm         : t_dram_dqm;

  -- dram command
  signal s_dram_cmd_val     : std_logic_vector(3 downto 0);
  signal s_dram_dqm_val     : std_logic_vector(1 downto 0);

  -- input signals to memory
  signal s_row_address      : std_logic_vector(12 downto 0);
  signal s_col_address      : std_logic_vector(9 downto 0);
  signal s_bank_select      : std_logic_vector(1 downto 0);
  signal s_autoprecharge    : std_logic;
  signal s_precharge_all    : std_logic;

  -- tri-state dram in/out control
  signal s_buf_in_out_n     : std_logic;                     -- 1 -> write, 0 -> read
  signal s_dram_buf_in      : std_logic_vector(15 downto 0); -- write to mem

  -- input address register
  signal s_add_in_r         : std_logic_vector(24 downto 0);
  signal s_add_in_r_in      : std_logic_vector(24 downto 0);
  signal s_add_in_r_en      : std_logic;
  signal s_add_in_r_rst     : std_logic;

  -- input data register
  signal s_data_in_r        : std_logic_vector(15 downto 0);
  signal s_data_in_r_in     : std_logic_vector(15 downto 0);
  signal s_data_in_r_en     : std_logic;
  signal s_data_in_r_rst    : std_logic;

  -- output data register
  signal s_data_out_r       : std_logic_vector(15 downto 0);
  signal s_data_out_r_in    : std_logic_vector(15 downto 0);
  signal s_data_out_r_en    : std_logic;
  signal s_data_out_r_rst   : std_logic;

  -- mrs signal
  signal s_mrs_value        : std_logic_vector(14 downto 0);

  -- clock enable tio SDRAM
  signal s_clock_en         : std_logic;

  -- delay counter
  signal s_delay_cnt_i      : natural range 0 to c_200_us_delay;
  signal s_delay_cnt_r      : natural range 0 to c_200_us_delay;

  -- initial refresh counter
  signal s_init_ref_cnt_i   : natural range 0 to c_init_ref_cyles;
  signal s_init_ref_cnt_r   : natural range 0 to c_init_ref_cyles;

  -- init and ready flags
  signal s_init_flag        : std_logic;
  signal s_ready_flag       : std_logic;
  signal s_error_flag       : std_logic;

  -- refresh request
  signal s_refresh          : std_logic;

  -- delayed signals to dram (half clock cycle)
  signal o_clk_en_d         : std_logic;
  signal o_cs_n_d           : std_logic;
  signal o_ras_n_d          : std_logic;
  signal o_cas_n_d          : std_logic;
  signal o_we_n_d           : std_logic;
  signal o_dram_addr_d      : std_logic_vector(12 downto 0);
  signal io_dram_dq_d       : std_logic_vector(15 downto 0);
  signal o_dram_ba_d        : std_logic_vector(1 downto 0);
  signal o_dram_ldqm_d      : std_logic;
  signal o_dram_udqm_d      : std_logic;

begin
  -- external signal assignment

  o_clk_en_d    <= s_clock_en;

  o_cs_n_d      <= s_dram_cmd_val(3);
  o_ras_n_d     <= s_dram_cmd_val(2);
  o_cas_n_d     <= s_dram_cmd_val(1);
  o_we_n_d      <= s_dram_cmd_val(0);

  o_dram_addr_d <= s_row_address                          when s_dram_fsm = st_activate else
                   "00" & s_precharge_all & "0000000000"  when s_dram_fsm = st_init_pre else
                   "00" & s_autoprecharge & s_col_address when s_dram_fsm = st_rw else
                   "00" & s_precharge_all & s_col_address when s_dram_fsm = st_pre else
                   s_mrs_value(12 downto 0)               when s_dram_fsm = st_init_mode else
                   (others => '0');

  io_dram_dq_d  <= s_dram_buf_in when s_buf_in_out_n = '1' else (others => 'Z');

  o_dram_ba_d   <= s_mrs_value(14 downto 13)  when s_dram_fsm = st_init_mode else
                   "11"                       when s_dram_fsm = st_init_pre else
                   s_bank_select              when s_dram_fsm = st_activate else
                   s_bank_select              when s_dram_fsm = st_rw else
                   s_bank_select              when s_dram_fsm = st_pre else
                   (others => '0');

  o_dram_udqm_d <= s_dram_dqm_val(1);
  o_dram_ldqm_d <= s_dram_dqm_val(0);

  o_init        <= s_init_flag;
  o_data        <= s_data_out_r;
  o_ready       <= s_ready_flag;
  o_error       <= s_error_flag;

  -- internal signal assignment
  s_refresh                           <= i_refresh;

  s_dram_buf_in                       <= s_data_in_r;
  s_data_out_r_in                     <= io_dram_dq when s_buf_in_out_n = '0' else (others => '0'); -- TODO fix (set to io_dram_dq only when read)

  s_add_in_r_in                       <= i_address;
  s_data_in_r_in                      <= i_data;

  s_mrs_value(MRS_RESERVED_RANGE)     <= (others => '0');
  s_mrs_value(MRS_WR_BURST_MODE)      <= c_wr_burst_single;
  s_mrs_value(MRS_OP_MODE_RANGE)      <= c_std_op;
  s_mrs_value(MRS_LATENCY_MODE_RANGE) <= c_cas_2;
  s_mrs_value(MRS_BURST_TYPE)         <= c_burst_seq;
  s_mrs_value(MRS_BURST_LEN_RANGE)    <= c_burst_1;

  s_bank_select   <= s_add_in_r(24 downto 23);
  s_row_address   <= s_add_in_r(22 downto 10);
  s_col_address   <= s_add_in_r(9 downto 0);
  s_autoprecharge <= '1' when c_autopre = true else '0';

  -- components
  ADD_IN_R : REGISTER_N
  generic map (25)
  port map(
    i_clk         => i_clk,
    i_reset_n     => s_add_in_r_rst,
    i_load_en     => s_add_in_r_en,
    i_par_in      => s_add_in_r_in,
    o_par_out     => s_add_in_r
  );

  -- holds data from external input (write to memory)
  DATA_IN_R : REGISTER_N
  generic map (16)
  port map(
    i_clk         => i_clk,
    i_reset_n     => s_data_in_r_rst,
    i_load_en     => s_data_in_r_en,
    i_par_in      => s_data_in_r_in,
    o_par_out     => s_data_in_r
  );

  -- holds data from memory (to external output)
  DATA_OUT_R : REGISTER_N
  generic map (16)
  port map(
    i_clk         => i_clk,
    i_reset_n     => s_data_out_r_rst,
    i_load_en     => s_data_out_r_en,
    i_par_in      => s_data_out_r_in,
    o_par_out     => s_data_out_r
  );

  -- processes
  p_delayed_signals: process(i_clk)
  begin
    if i_clk'event and i_clk = '0' then
      o_clk_en         <= o_clk_en_d;
      o_cs_n           <= o_cs_n_d;
      o_ras_n          <= o_ras_n_d;
      o_cas_n          <= o_cas_n_d;
      o_we_n           <= o_we_n_d;
      o_dram_addr      <= o_dram_addr_d;
      io_dram_dq       <= io_dram_dq_d;
      o_dram_ba        <= o_dram_ba_d;
      o_dram_ldqm      <= o_dram_ldqm_d;
      o_dram_udqm      <= o_dram_udqm_d;
    end if;
  end process;

  p_dram_fsm_state: process(i_clk, i_reset_n)
  begin
    if i_reset_n = '0' then
      s_dram_fsm <= st_rst;
    elsif i_clk'event and i_clk = '1' then  --TODO consider using falling edge to respect timing
      case s_dram_fsm is
        when st_rst         =>
          s_dram_fsm <= st_init_wait;

        when st_init_wait   =>
          if s_delay_cnt_r = 0 then -- intial wait delay
            s_dram_fsm <= st_init_pre;
          else
            s_dram_fsm <= st_init_wait;
          end if;

        when st_init_pre    =>
          if s_delay_cnt_r = 0 then -- pre to active delay
            s_dram_fsm <= st_init_pre_wait;
          else
            s_dram_fsm <= st_init_pre;
          end if;

        when st_init_pre_wait =>
          if s_delay_cnt_r = 0 then -- pre to active delay
            s_dram_fsm <= st_init_ref;
          else
            s_dram_fsm <= st_init_pre_wait;
          end if;

        when st_init_ref    =>
          if s_init_ref_cnt_r = 0 then  -- 8 refresh cycles, waits for refresh time before update
            s_dram_fsm <= st_init_mode;
          else
            s_dram_fsm <= st_init_ref;
          end if;

        when st_init_mode   =>
          if s_delay_cnt_r = 0 then  -- mode register write delay
            s_dram_fsm <= st_idle;
          else
            s_dram_fsm <= st_init_mode;
          end if;

        when st_idle        =>
          if i_read = '1' or i_write = '1' then
            s_dram_fsm <= st_activate;
          elsif s_refresh = '1' then
            s_dram_fsm <= st_ref;
          else
            s_dram_fsm <= st_idle;
          end if;

        when st_activate    =>
          s_dram_fsm <= st_rcd;

        when st_ref         =>
          if s_delay_cnt_r = 0 then  -- refresh delay
            s_dram_fsm <= st_done;
          else
            s_dram_fsm <= st_ref;
          end if;

        when st_rcd         =>
          if s_delay_cnt_r = 0 then  -- trcd delay
            s_dram_fsm <= st_rw;
          else
            s_dram_fsm <= st_rcd;
          end if;

        when st_rw          =>
          s_dram_fsm <= st_ras2;

        when st_ras1        =>
          s_dram_fsm <= st_ras2;

        when st_ras2        =>
          if c_autopre = true then
            if s_delay_cnt_r = 0 then  -- precharge delay
              s_dram_fsm <= st_idle;
            else
              s_dram_fsm <= st_ras2;
            end if;
          else
            s_dram_fsm <= st_pre;
          end if;

        when st_pre         =>
          s_dram_fsm <= st_done;

        when st_done        =>
          if s_delay_cnt_r = 0 then  -- trcd  -1 delay
            s_dram_fsm <= st_idle;
          else
            s_dram_fsm <= st_done;
          end if;

        when others         =>
          s_dram_fsm <= st_rst;
      end case;
    end if;
  end process;

  p_dram_fsm_ctrl: process(s_dram_fsm, s_delay_cnt_r, s_init_ref_cnt_r, i_read, i_write, s_refresh)
  begin
    case s_dram_fsm is
      when st_rst         =>
        s_delay_cnt_i     <= c_200_us_delay;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '0';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '0';
        s_data_in_r_rst     <= '0';
        s_data_out_r_rst    <= '0';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '0';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_init_wait   =>
        if s_delay_cnt_r /= 0 then
          s_delay_cnt_i     <= s_delay_cnt_r - 1;
        else
          s_delay_cnt_i   <= 0;
        end if;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '0';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_init_pre    =>
        if s_delay_cnt_r /= 0 then
          s_delay_cnt_i     <= s_delay_cnt_r - 1;
        else
          s_delay_cnt_i   <= c_trp_cycles - 1;
        end if;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_PRE;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '0';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_init_pre_wait    =>
        if s_delay_cnt_r /= 0 then
          s_delay_cnt_i     <= s_delay_cnt_r - 1;
        else
          s_delay_cnt_i   <= c_auto_rf_delay;
        end if;
        s_init_ref_cnt_i  <= c_init_ref_cyles;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '1';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '0';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_init_ref    =>
        if s_delay_cnt_r /= 0 then
          s_delay_cnt_i     <= s_delay_cnt_r - 1;
        else
          if s_init_ref_cnt_r /= 0 then
            s_delay_cnt_i   <= c_auto_rf_delay;
          else
            s_delay_cnt_i   <= c_mrd_cycles;
          end if;
        end if;
        if s_delay_cnt_r = 0 then
          s_init_ref_cnt_i  <= s_init_ref_cnt_r - 1;
        end if;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        if s_delay_cnt_r = c_auto_rf_delay then
          s_dram_cmd        <= CMD_REF;
        else
          s_dram_cmd        <= CMD_NOP;
        end if;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '0';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_init_mode   =>
        s_delay_cnt_i     <= 0;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '0';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_idle        =>
        if s_refresh = '1' then
          s_delay_cnt_i     <= c_auto_rf_delay;
        end if;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        if (i_read = '1' or i_write = '1') then
          s_add_in_r_en       <= '1';
          s_data_in_r_en      <= '1';
        else
          s_add_in_r_en       <= '0';
          s_data_in_r_en      <= '0';
        end if;
        s_data_out_r_en     <= '0';

        s_init_flag         <= '1';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_activate    =>
        s_delay_cnt_i     <= 0;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_ACT;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '1';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_ref         =>
        if s_delay_cnt_r /= 0 then
          s_delay_cnt_i <= s_delay_cnt_r - 1;
        else
          s_delay_cnt_i     <= c_trp_cycles - 1;
        end if;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        if s_delay_cnt_r = c_auto_rf_delay then
          s_dram_cmd        <= CMD_REF;
        else
          s_dram_cmd        <= CMD_NOP;
        end if;

        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '1';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_rcd         =>
        s_delay_cnt_i     <= 0;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '1';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_rw          =>
        s_delay_cnt_i     <= 0;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        if i_write = '1' then
          s_buf_in_out_n    <= '1';
        else
          s_buf_in_out_n    <= '0';
        end if;

        s_precharge_all   <= '0';

        if i_read = '1' then
          s_dram_cmd        <= CMD_RD;
          s_dram_dqm        <= DQM_OUT_EN;
        elsif i_write = '1' then
          s_dram_cmd        <= CMD_WR;
          s_dram_dqm        <= DQM_OUT_EN;
        else
          s_dram_cmd        <= CMD_NOP;
          s_dram_dqm        <= DQM_OUT_DIS;
        end if;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '1';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_ras1        =>
        s_delay_cnt_i     <= 0;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '1';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_ras2        =>
        s_delay_cnt_i     <= 0;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '1';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_pre         =>
        s_delay_cnt_i     <= c_trp_cycles - 1;  -- one cycle lost due to done state
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_PRE;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '1';

        s_init_flag         <= '1';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

      when st_done        =>
        s_delay_cnt_i     <= 0;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '1';
        s_ready_flag        <= '1';
        s_error_flag        <= '0';

      when others         =>
        s_delay_cnt_i     <= 0;
        s_init_ref_cnt_i  <= 0;

        s_clock_en        <= '1';

        s_buf_in_out_n    <= '0';

        s_precharge_all   <= '0';

        s_dram_cmd        <= CMD_NOP;
        s_dram_dqm        <= DQM_OUT_DIS;

        s_add_in_r_rst      <= '1';
        s_data_in_r_rst     <= '1';
        s_data_out_r_rst    <= '1';

        s_add_in_r_en       <= '0';
        s_data_in_r_en      <= '0';
        s_data_out_r_en     <= '0';

        s_init_flag         <= '1';
        s_ready_flag        <= '0';
        s_error_flag        <= '0';

    end case;
  end process;

  p_timer_r: process(i_reset_n, i_clk)
  begin
    if i_reset_n = '0' then
      s_delay_cnt_r     <= 0;
      s_init_ref_cnt_r  <= 0;
    elsif i_clk'event and i_clk = '1' then
      s_delay_cnt_r     <= s_delay_cnt_i;
      s_init_ref_cnt_r  <= s_init_ref_cnt_i;
    end if;
  end process;

  p_dram_cmd: process(s_dram_cmd)
  begin
    case s_dram_cmd is
      when CMD_DESL   =>
        s_dram_cmd_val <= "1000";

      when CMD_NOP    =>
        s_dram_cmd_val <= "0111";

      when CMD_BST    =>
        s_dram_cmd_val <= "0110";

      when CMD_RD     =>
        s_dram_cmd_val <= "0101";

      when CMD_RD_AP  =>
        s_dram_cmd_val <= "0101";

      when CMD_WR     =>
        s_dram_cmd_val <= "0100";

      when CMD_WR_AP  =>
        s_dram_cmd_val <= "0100";

      when CMD_ACT    =>
        s_dram_cmd_val <= "0011";

      when CMD_PRE    =>
        s_dram_cmd_val <= "0010";

      when CMD_PALL   =>
        s_dram_cmd_val <= "0010";

      when CMD_REF    =>
        s_dram_cmd_val <= "0001";

      when CMD_SELF   =>
        s_dram_cmd_val <= "0001";

      when CMD_MSR    =>
        s_dram_cmd_val <= "0000";

      when others     =>
        s_dram_cmd_val <= "0111";

    end case;
  end process;

  p_dqm: process(s_dram_dqm)
  begin
    case s_dram_dqm is
      when DQM_OUT_EN   =>
        s_dram_dqm_val  <= "00";

      when DQM_OUT_DIS  =>
        s_dram_dqm_val  <= "11";

      when DQM_UP_EN    =>
        s_dram_dqm_val  <= "01";

      when DQM_UP_DIS   =>
        s_dram_dqm_val  <= "11";

      when DQM_LOW_EN   =>
        s_dram_dqm_val  <= "10";

      when DQM_LOW_DIS  =>
        s_dram_dqm_val  <= "11";

      when others       =>
        s_dram_dqm_val  <= "11";

    end case;
  end process;

end architecture;
