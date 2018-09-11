library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SAMPLE_MANAGER is
	generic (
		g_sample_width 	: integer := 8;
		g_patch_width		: integer := 7;
		g_mem_size			: integer := 12
	);
	port (
		i_clk					: in  std_logic;
		i_enable  		: in  std_logic;
		i_patch				: in	std_logic_vector(g_patch_width - 1 downto 0);
		i_address			: in  std_logic_vector(g_mem_size - 1 downto 0);
		o_mem_out			: out std_logic_vector(g_sample_width - 1 downto 0)
	);
end entity;

architecture BHV of SAMPLE_MANAGER is

	component sine_sample_rom
		port
		(
			address		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			clock		: IN STD_LOGIC  := '1';
			q		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
		);
	end component;

	signal s_sine_out 	: std_logic_vector(g_sample_width - 1 downto 0);
	signal s_square_out : std_logic_vector(g_sample_width - 1 downto 0);

begin

	sine_samples: sine_sample_rom
	port map (
			address		=> i_address,
			clock		  => i_clk,
			q		      => s_sine_out
		);

	o_mem_out <= s_sine_out;

	-- p_mem_out: process(i_enable, i_address, s_sine_out, s_square_out)
	-- begin
	-- 	if i_enable = '1' then
	-- 		case (to_integer(unsigned(i_patch))) is
	-- 			when 0      => -- sine mem
	-- 				o_mem_out <= s_sine_out;
	-- 			when 1      => --square mem
	-- 				o_mem_out <= s_square_out;
	-- 			when others =>
	-- 				o_mem_out 		<= (others => '0');
	-- 		end case;
	-- 	else
	-- 		o_mem_out 		<= (others => '0');
	-- 	end if;
	-- end process;

end architecture;
