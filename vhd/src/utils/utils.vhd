library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package UTILS_PKG is

  -- constants

  -- functions
  function log2 (X : positive)  return natural;                   -- Y = log2(X)
  function up_int_log2 (X : positive) return natural;             -- log2 rounded to upper int
  function and_reduce(datain : std_logic_vector) return std_logic;
  function or_reduce(datain : std_logic_vector) return std_logic;
  function xor_reduce(datain : std_logic_vector) return std_logic;

  -- custom types
  type t_midi_cmd is (
    midi_unknown, midi_note_on, midi_note_off, midi_ctrl_ch, midi_prg_ch
  );

  type t_ch_mode is (
    ctrl_unknown, ctrl_all_sn_off, ctrl_rst, ctrl_all_nt_off, ctrl_omni_on, ctrl_omni_off, ctrl_mono_on, ctrl_poly_on
  );

  -- midi message subtypes (according to MIDI protocol)
  subtype MIDI_CMD_RANGE  is natural range 7 downto 4;
  subtype MIDI_CH_RANGE   is natural range 3 downto 0;
  subtype MIDI_DATA_RANGE is natural range 6 downto 0;

  -- sequencer subtypes (according to internal sequencer structure)
  constant TYPE_BITS      : natural := 3; -- configurable for future use

  subtype SEQ_RSVD_RANGE  is natural range 31 downto (TYPE_BITS + 18);  -- reserved
  subtype SEQ_TYPE_RANGE  is natural range (TYPE_BITS + 18 - 1) downto 18;
  subtype SEQ_CH_RANGE    is natural range 17 downto 14;
  subtype SEQ_DATA1_RANGE is natural range 13 downto 7;
  subtype SEQ_DATA2_RANGE is natural range 6 downto 0;

  subtype SEQ_TS_S_RANGE  is natural range 22 downto 12;
  subtype SEQ_TS_F_RANGE  is natural range 11 downto 0;

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
