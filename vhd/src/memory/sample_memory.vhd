library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.UTILS_PKG.all;

entity SAMPLE_MEMORY is
	generic (
		g_sample_width 	: integer := 8;
		g_patch_width		: integer := 7;
		g_mem_size			: integer := 12
	);
	port (
		i_clk					: in  std_logic;
		i_reset_n			: in  std_logic;
		i_enable  		: in  std_logic;
		i_read    		: in  std_logic;
		i_address			: in  std_logic_vector(g_patch_width + g_mem_size - 1 downto 0);
		o_mem_ready		: out std_logic;
		o_mem_out			: out std_logic_vector(g_sample_width - 1 downto 0)
	);
end entity;

architecture BHV of SAMPLE_MEMORY is

	component SINE_MEM is
		port (
			i_enable_mem		: std_logic;
			i_addr					: in std_logic_vector(11 downto 0);
			o_out 				 	: out std_logic_vector(7 downto 0)
		);
	end component;

	component SQUARE_MEM is
		port (
			i_enable_mem		: std_logic;
			i_addr					: in std_logic_vector(11 downto 0);
			o_out 				 	: out std_logic_vector(7 downto 0)
		);
	end component;

	type t_fsm is (st_reset, st_idle, st_read, st_ready);

	signal s_fsm 				: t_fsm;

	signal s_sine_out 	: std_logic_vector(g_sample_width - 1 downto 0);
	signal s_square_out : std_logic_vector(g_sample_width - 1 downto 0);

begin

	SINE_SAMPLES : SINE_MEM
	port map (
		i_enable_mem		=> i_enable,
		i_addr					=> i_address(SMP_IDX_RANGE),
		o_out 				 	=> s_sine_out
	);

	SQUARE_SAMPLES : SQUARE_MEM
	port map (
		i_enable_mem		=> i_enable,
		i_addr					=> i_address(SMP_IDX_RANGE),
		o_out 				 	=> s_square_out
	);

	o_mem_ready <= '1' when s_fsm = st_ready else '0';

	p_mem_fsm: process(i_clk, i_reset_n)
	begin
		if i_reset_n = '0' then
			s_fsm <= st_reset;
		elsif i_clk'event and i_clk = '1' then
			case s_fsm is
				when st_reset		=>
					s_fsm <= st_idle;
				when st_idle 		=>
					if i_read = '1' then
						s_fsm <= st_read;
					else
						s_fsm <= st_idle;
					end if;
				when st_read		=>
					s_fsm <= st_ready;
				when st_ready		=>
					s_fsm <= st_idle;
				when others 		=>
					s_fsm <= st_reset;
			end case;
		end if;
	end process;

	p_mem_out: process(i_enable, i_address)
	begin
		if i_enable = '1' then
			case (to_integer(unsigned(i_address(BANK_SEL_RANGE)))) is
				when 0      => -- sine mem
					o_mem_out <= s_sine_out;
				when 1      => --square mem
					o_mem_out <= s_square_out;
				when others =>
					o_mem_out 		<= (others => '0');
			end case;
		else
			o_mem_out 		<= (others => '0');
		end if;
	end process;

end architecture;
