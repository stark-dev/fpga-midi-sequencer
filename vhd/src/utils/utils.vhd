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

  -- custom types
  -- type irqCode is (
  --   IRQ0, IRQ1, IRQ2, IRQ3, IRQ4, IRQ5, IRQ6, IRQ7, NO_IRQ
  -- );

  -- subtype OPCODE_RANGE is natural range Nbit-1 downto Nbit-OPCODE_SIZE;

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

end UTILS_PKG;
