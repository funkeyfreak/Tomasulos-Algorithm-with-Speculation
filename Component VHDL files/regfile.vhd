LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

--this is really the regfile, not the instruction que

ENTITY regfile IS
	PORT(	clock, reset			: IN	STD_LOGIC; --we know this
			write_enable			: IN	STD_LOGIC;
			read_reg1, read_reg2	: IN	STD_LOGIC_VECTOR( 5 DOWNTO 0);
			write_reg				: IN	STD_LOGIC_VECTOR( 5 DOWNTO 0);
			write_data				: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
			read_data1, read_data2	: OUT 	STD_LOGIC_VECTOR(31 DOWNTO 0)
		);
END regfile;

ARCHITECTURE behavior OF regfile IS

	TYPE REGFILE_T IS ARRAY (0 TO 63) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL registers : REGFILE_T;

BEGIN	
	
	PROCESS(reset, clock)
	BEGIN
		IF(reset = '0') THEN
			FOR i IN 0 TO 31 LOOP --loops can only execute using no logic aka assigning
				registers(i) <= X"00000000"; --flush the registers
			END LOOP;
		ELSIF(RISING_EDGE(clock)) THEN
			IF(write_enable = '1') THEN
				registers(CONV_INTEGER(write_reg)) <= write_data;
			END IF;
		END IF;
	END PROCESS;
	
	read_data1 <= registers(CONV_INTEGER(read_reg1));
	read_data2 <= registers(CONV_INTEGER(read_reg2));
	
END behavior;