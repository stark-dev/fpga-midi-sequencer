library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package UTILS_PKG is

  -- constants
  constant MIDI_BAUD_RATE : natural := 31250;   -- MIDI speed
  constant MIDI_DATA_SIZE : natural := 7;       -- standard MIDI data

  -- sequencer constants (according to internal sequencer structure)
  constant MIDI_TYPE_SIZE : natural := 3;   -- configurable for future use

  constant SEQ_EVENT_SIZE : natural := 32;  -- size of internally represented MIDI event
  constant SEQ_TIME_SIZE  : natural := 32;  -- size of internally represented timestamp

  -- sequencer core constants (status register)
  constant SEQ_TRACKS     : natural := 8;   -- available tracks
  constant SEQ_NOTE_SIZE  : integer := 7;   -- size of note in sequencer
  constant SEQ_VEL_SIZE   : integer := 7;   -- size of vel in sequencer

  -- general status constants
  constant ST_TRACK_SIZE  : natural := 3;   -- bits to represent avail tracks

  constant ST_TSS_SIZE    : natural := 11;  -- timestamp in seconds
  constant ST_TSF_SIZE    : natural := 12;  -- timestamp in fractions
  constant ST_RUN_SIZE    : natural := 1;   -- play / pause
  constant ST_VOL_SIZE    : natural := 7;   -- volume
  -- constant ST_BPM_SIZE    : natural := 8;   -- bpm tempo
  -- constant ST_MTR_SIZE    : natural := 1;   -- metronome enable

  -- track status register constants
  constant TR_ST_SIZE     : natural := 32;
  constant TR_PATCH_SIZE  : natural := 7;
  constant TR_CH_SIZE     : natural := 4;
  constant TR_VOL_SIZE    : natural := 7;
  constant TR_PAN_SIZE    : natural := 7;

  -- display constants
  constant DISPLAY_SIZE   : integer := 6;

  -- memory constants (memory byte addressable)
  constant MEMORY_SIZE    : integer := 26;    -- 64 MB
  constant MEMORY_TR_SIZE : integer := 8192;  -- 8KB per track (1k sample per track)

  -- sound generator constants
  constant SAMPLE_WIDTH   : integer := 8;     -- bit width of sound samples
  constant SAMPLE_FREQ    : integer := 44100; -- sampling frequency of sound samples
  constant MAX_POLYPHONY  : integer := 7;     -- max number of notes played at the same time
  constant MAX_POLY_BIT   : integer := 3;     -- bit size of max poly counter (change accordingly)
  constant SMP_MEM_SIZE   : integer := 12;    -- address bit width of sample memory

  -- functions
  function log2 (X : positive)  return natural;                   -- Y = log2(X)
  function up_int_log2 (X : positive) return natural;             -- log2 rounded to upper int
  function and_reduce(datain : std_logic_vector) return std_logic;
  function or_reduce(datain : std_logic_vector) return std_logic;
  function xor_reduce(datain : std_logic_vector) return std_logic;

  -- custom types

  -- core fsm
  type t_core_fsm     is (st_reset, st_init, st_idle, st_play, st_rec, st_end, st_stop, st_menu);

  -- menu option
  type t_menu_option  is (op_track, op_patch, op_track_vol, op_pan, op_poly, op_omni);

  -- midi commands
  type t_midi_cmd is (
    midi_unknown, midi_note_on, midi_note_off, midi_ctrl_ch, midi_prg_ch
  );

  -- channel modes
  type t_ch_mode is (
    ctrl_unknown, ctrl_all_sn_off, ctrl_rst, ctrl_all_nt_off, ctrl_omni_on, ctrl_omni_off, ctrl_mono_on, ctrl_poly_on
  );

  -- memory write mux
  type t_mem_wr_mux is (
    mux_off, mux_midi, mux_ts
  );

  -- display values
  type t_display_out is (
    ds_A, ds_B, ds_C, ds_D, ds_E, ds_F, ds_G, ds_H,
    ds_I, ds_J, ds_K, ds_L, ds_M, ds_N, ds_O, ds_P,
    ds_Q, ds_R, ds_S, ds_T, ds_U, ds_V, ds_W, ds_X,
    ds_Y, ds_Z, ds_0, ds_1, ds_2, ds_3, ds_4, ds_5,
    ds_6, ds_7, ds_8, ds_9, ds_OFF
  );

  type t_display_array  is array (DISPLAY_SIZE - 1 downto 0) of t_display_out;
  type t_display_if     is array (DISPLAY_SIZE - 1 downto 0) of std_logic_vector(6 downto 0);

  type t_midi_data      is array (SEQ_TRACKS - 1 downto 0) of std_logic_vector(SEQ_EVENT_SIZE - 1  downto 0);
  type t_midi_ts        is array (SEQ_TRACKS - 1 downto 0) of std_logic_vector(SEQ_TIME_SIZE - 1  downto 0);

  type t_sg_patch       is array (SEQ_TRACKS - 1 downto 0) of std_logic_vector(TR_PATCH_SIZE - 1 downto 0);
  type t_sg_note        is array (SEQ_TRACKS - 1 downto 0) of std_logic_vector(SEQ_NOTE_SIZE - 1 downto 0);
  type t_sg_vel         is array (SEQ_TRACKS - 1 downto 0) of std_logic_vector(SEQ_VEL_SIZE - 1 downto 0);

  -- sound generator
  type t_sound_table    is array (2**SEQ_NOTE_SIZE - 1 downto 0) of std_logic_vector(SMP_MEM_SIZE - 1 downto 0);
  type t_sound_gen_out  is array (MAX_POLYPHONY - 1 downto 0) of std_logic_vector(SMP_MEM_SIZE - 1 downto 0);

  type t_sample_enable  is array (SEQ_TRACKS - 1 downto 0) of std_logic_vector(MAX_POLYPHONY - 1 downto 0);
  type t_sample_idx     is array (SEQ_TRACKS - 1 downto 0) of t_sound_gen_out;

  -- sample memory
  type t_sample_memory  is array (2**SMP_MEM_SIZE - 1 downto 0) of std_logic_vector(SAMPLE_WIDTH - 1 downto 0);

  subtype BANK_SEL_RANGE is natural range TR_PATCH_SIZE + SMP_MEM_SIZE - 1 downto SMP_MEM_SIZE;
  subtype SMP_IDX_RANGE  is natural range SMP_MEM_SIZE - 1 downto 0;

  -- midi message subtypes (according to MIDI protocol)
  subtype MIDI_CMD_RANGE  is natural range 7 downto 4;
  subtype MIDI_CH_RANGE   is natural range 3 downto 0;
  subtype MIDI_DATA_RANGE is natural range 6 downto 0;

  -- sequencer subtypes (according to internal sequencer structure)
  subtype SEQ_RSVD_RANGE  is natural range 31 downto (MIDI_TYPE_SIZE + 18);  -- reserved
  subtype SEQ_TYPE_RANGE  is natural range (MIDI_TYPE_SIZE + 18 - 1) downto 18;
  subtype SEQ_CH_RANGE    is natural range 17 downto 14;
  subtype SEQ_DATA1_RANGE is natural range 13 downto 7;
  subtype SEQ_DATA2_RANGE is natural range 6 downto 0;

  -- sequencer core subtypes (status register)
  subtype  ST_TS_RESERV   is natural range 31 downto (ST_TSS_SIZE + ST_TSF_SIZE);
  subtype  ST_TSS_RANGE   is natural range (ST_TSS_SIZE + ST_TSF_SIZE - 1) downto ST_TSF_SIZE;
  subtype  ST_TSF_RANGE   is natural range ST_TSF_SIZE  - 1 downto 0;

  -- track status register fields
  subtype  TR_PATCH_RANGE is natural range 6 downto 0;
  subtype  TR_CH_RANGE    is natural range 10 downto 7;
  subtype  TR_VOL_RANGE   is natural range 17 downto 11;
  subtype  TR_PAN_RANGE   is natural range 24 downto 18;
  constant TR_MONO_BIT    : natural := 25;
  constant TR_SOLO_BIT    : natural := 26;
  constant TR_REC_BIT     : natural := 27;
  constant TR_ACT_BIT     : natural := 28;
  constant TR_POLY_BIT    : natural := 29;
  constant TR_OMNI_BIT    : natural := 30;
  constant TR_RESERVED    : natural := 31;

end UTILS_PKG;

package body UTILS_PKG is

-------------------------------------------------------------------------------

function log2 (X : positive) return natural is
  variable cnt : integer;
  variable tmp : integer;
begin

  cnt := 0;
  tmp := X;
  while tmp >= 2 loop
    tmp := tmp/2;
    cnt := cnt + 1;
  end loop;

  return cnt;

end log2;

-------------------------------------------------------------------------------

function up_int_log2 (X : positive) return natural is

  variable N : natural;

begin

  N := log2(X);

  if (X > (2**N)) then
    return (N+1);
  else
    return N;
  end if;

end up_int_log2;

--------------------------------------------------------------------------------

function and_reduce(datain : std_logic_vector) return std_logic is
  variable res : std_logic := '1';
begin
  for i in datain'range loop
    res := res and datain(i);
  end loop;
  return res;
end and_reduce;

--------------------------------------------------------------------------------

function or_reduce(datain : std_logic_vector) return std_logic is
  variable res : std_logic := '0';
begin
  for i in datain'range loop
    res := res or datain(i);
  end loop;
  return res;
end or_reduce;


--------------------------------------------------------------------------------

function xor_reduce(datain : std_logic_vector) return std_logic is
  variable res : std_logic := '0';
begin
  for i in datain'range loop
    res := res xor datain(i);
  end loop;
  return res;
end xor_reduce;

--------------------------------------------------------------------------------

end UTILS_PKG;
