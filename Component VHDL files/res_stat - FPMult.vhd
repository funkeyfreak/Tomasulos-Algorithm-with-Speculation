LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY res_stat IS
	PORT(		
				clock, reset			: IN 	STD_LOGIC; --clock on which to add items
				incoming_indicator		: IN	STD_LOGIC;
				incoming_instruction	: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				incoming_regS			: IN 	STD_LOGIC_VECTOR(31 DOWNTO 0);
				incoming_regT			: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				issuer_stall			: IN	STD_LOGIC;
				CDB_valid				: IN 	STD_LOGIC;
				CDB_addr				: IN 	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				CDB_data				: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				ALU_result				: IN 	STD_LOGIC_VECTOR(31 DOWNTO 0);
				ALU_result_complete		: IN	STD_LOGIC;
				ALU_busy				: IN 	STD_LOGIC;
				--ALU_op					: IN	STD_LOGIC;
				res_stat_full			: OUT	STD_LOGIC;
				regS					: OUT 	STD_LOGIC_VECTOR(31 DOWNTO 0);
				regT					: OUT 	STD_LOGIC_VECTOR(31 DOWNTO 0);
				fmt_out				: OUT	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				dest_reg				: OUT	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				tester				: OUT STD_LOGIC;
				tester2				: OUT STD_LOGIC;
				tester3				: OUT STD_LOGIC;
				tester4				: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0);
				tester5				: OUT STD_LOGIC_VECTOR( 4 DOWNTO 0);
				tester6				: OUT STD_LOGIC_VECTOR( 4 DOWNTO 0)
		 );
END res_stat;

ARCHITECTURE behavior OF res_stat IS

TYPE entry IS
	RECORD
		instruction				: STD_LOGIC_VECTOR( 5 DOWNTO 0); -- 0
		fmt_val					: STD_LOGIC_VECTOR( 4 DOWNTO 0); -- 1
		regS_addr				: STD_LOGIC_VECTOR( 4 DOWNTO 0); -- 2
		data_regS				: STD_LOGIC_VECTOR(31 DOWNTO 0); -- 3
		regT_addr				: STD_LOGIC_VECTOR( 4 DOWNTO 0); -- 4
		data_regT				: STD_LOGIC_VECTOR(31 DOWNTO 0); -- 5
		resultReg				: STD_LOGIC_VECTOR( 4 DOWNTO 0); -- 6
		resultValue				: STD_LOGIC_VECTOR(31 DOWNTO 0); -- 7
		timer						: STD_LOGIC_VECTOR( 4 DOWNTO 0); -- 8
		ready						: STD_LOGIC_VECTOR( 1 DOWNTO 0); -- 9
		inPipe					: STD_LOGIC;					 -- 10
		isEmpty					: STD_LOGIC;					 -- 11
	END RECORD;
TYPE res_stat_array IS ARRAY (0 TO 31) OF entry;

TYPE pipeline_item IS
	RECORD
		stored_item				: entry;		-- 0
		occupied				: STD_LOGIC;	-- 1
	END RECORD;
		

TYPE res_stat_pipe	IS ARRAY (0 TO 7)  OF pipeline_item;

SIGNAL res_stat 			: res_stat_array;
SIGNAL res_pipe			: res_stat_pipe;
--SIGNAL op					: STD_LOGIC_VECTOR( 5 DOWNTO 0);
--SIGNAL fs					: STD_LOGIC_VECTOR( 4 DOWNTO 0);
--SIGNAL ft					: STD_LOGIC_VECTOR( 4 DOWNTO 0);
--SIGNAL fd					: STD_LOGIC_VECTOR( 4 DOWNTO 0);
--SIGNAL fmt				: STD_LOGIC_VECTOR( 4 DOWNTO 0);
--SIGNAL funct				: STD_LOGIC_VECTOR( 5 DOWNTO 0);

SIGNAL op					: STD_LOGIC_VECTOR( 5 DOWNTO 0);
SIGNAL fmt					: STD_LOGIC_VECTOR( 4 DOWNTO 0);
SIGNAL ft					: STD_LOGIC_VECTOR( 4 DOWNTO 0);
SIGNAL fs					: STD_LOGIC_VECTOR( 4 DOWNTO 0);
SIGNAL fd					: STD_LOGIC_VECTOR( 4 DOWNTO 0);
SIGNAL funct				: STD_LOGIC_VECTOR( 5 DOWNTO 0);				

BEGIN
	
--	op 		<= incoming_instruction(31 DOWNTO 26);
--	fs 		<= incoming_instruction(25 DOWNTO 21);
--	ft		<= incoming_instruction(20 DOWNTO 16);
--	fd		<= incoming_instruction(15 DOWNTO 11);
--	fmt	<= incoming_instruction(10 DOWNTO 6);
--	funct	<= incoming_instruction( 5 DOWNTO 0);

	op 		<= incoming_instruction(31 DOWNTO 26);
	fmt		<= incoming_instruction(25 DOWNTO 21);
	ft			<= incoming_instruction(20 DOWNTO 16);
	fs			<= incoming_instruction(15 DOWNTO 11);
	fd			<= incoming_instruction(10 DOWNTO 6);
	funct		<= incoming_instruction( 5 DOWNTO 0);

	
	
	PROCESS(clock, reset, op, fs,ft,fd, incoming_instruction, incoming_regS, incoming_regT, fmt, res_stat, res_pipe)
	BEGIN
		IF (reset = '0') THEN
			-- IDK what to do here
		ELSIF(RISING_EDGE(clock)) THEN
			IF(incoming_indicator = '1') THEN -- insefting a new item
				IF(res_stat(0).isEmpty = '0') THEN
					res_stat(0).isEmpty <= '1';
					res_stat(0).instruction <= op;
					res_stat(0).regS_addr <= fs;
					res_stat(0).data_regS <= incoming_regS;
					res_stat(0).regT_addr <= ft;
					res_stat(0).data_regT <= incoming_regT;
					res_stat(0).resultReg <= fd;
					res_stat(0).resultValue <= X"00000000";
					res_stat(0).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(0).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(0).ready	<= "10";
					ELSE
						res_stat(0).ready	<= "11";
					END IF;
					res_stat(0).fmt_val <= fmt;
					res_stat(0).inPipe <= '0';
					res_stat(0).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(1).isEmpty = '0') THEN
					res_stat(1).isEmpty <= '1';
					res_stat(1).instruction <= op;
					res_stat(1).regS_addr <= fs;
					res_stat(1).data_regS <= incoming_regS;
					res_stat(1).regT_addr <= ft;
					res_stat(1).data_regT <= incoming_regT;
					res_stat(1).resultReg <= fd;
					res_stat(1).resultValue <= X"00000000";
					res_stat(1).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(1).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(1).ready	<= "10";
					ELSE
						res_stat(1).ready	<= "11";
					END IF;
					res_stat(1).fmt_val <= fmt;
					res_stat(1).inPipe <= '0';
					res_stat(1).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(2).isEmpty = '0') THEN
					res_stat(2).isEmpty <= '1';
					res_stat(2).instruction <= op;
					res_stat(2).regS_addr <= fs;
					res_stat(2).data_regS <= incoming_regS;
					res_stat(2).regT_addr <= ft;
					res_stat(2).data_regT <= incoming_regT;
					res_stat(2).resultReg <= fd;
					res_stat(2).resultValue <= X"00000000";
					res_stat(2).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(2).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(2).ready	<= "10";
					ELSE
						res_stat(2).ready	<= "11";
					END IF;
					res_stat(2).fmt_val <= fmt;
					res_stat(2).inPipe <= '0';
					--res_stat(2).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(3).isEmpty = '0') THEN
					res_stat(3).isEmpty <= '1';
					res_stat(3).instruction <= op;
					res_stat(3).regS_addr <= fs;
					res_stat(3).data_regS <= incoming_regS;
					res_stat(3).regT_addr <= ft;
					res_stat(3).data_regT <= incoming_regT;
					res_stat(3).resultReg <= fd;
					res_stat(3).resultValue <= X"00000000";
					res_stat(3).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(3).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(3).ready	<= "10";
					ELSE
						res_stat(3).ready	<= "11";
					END IF;
					res_stat(3).fmt_val <= fmt;
					res_stat(3).inPipe <= '0';
					--res_stat(3).isEmpty <= '1'; 
					res_stat_full <= '0';
				ELSIF(res_stat(4).isEmpty = '0') THEN
					res_stat(4).isEmpty <= '1';
					res_stat(4).instruction <= op;
					res_stat(4).regS_addr <= fs;
					res_stat(4).data_regS <= incoming_regS;
					res_stat(4).regT_addr <= ft;
					res_stat(4).data_regT <= incoming_regT;
					res_stat(4).resultReg <= fd;
					res_stat(4).resultValue <= X"00000000";
					res_stat(4).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(4).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(4).ready	<= "10";
					ELSE
						res_stat(4).ready	<= "11";
					END IF;
					res_stat(4).fmt_val <= fmt;
					res_stat(4).inPipe <= '0';
					--res_stat(4).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(5).isEmpty = '0') THEN
					res_stat(5).isEmpty <= '1';
					res_stat(5).instruction <= op;
					res_stat(5).regS_addr <= fs;
					res_stat(5).data_regS <= incoming_regS;
					res_stat(5).regT_addr <= ft;
					res_stat(5).data_regT <= incoming_regT;
					res_stat(5).resultReg <= fd;
					res_stat(5).resultValue <= X"00000000";
					res_stat(5).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(5).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(5).ready	<= "10";
					ELSE
						res_stat(5).ready	<= "11";
					END IF;
					res_stat(5).fmt_val <= fmt;
					res_stat(5).inPipe <= '0';
					--res_stat(5).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(6).isEmpty = '0') THEN
					res_stat(6).isEmpty <= '1';
					res_stat(6).instruction <= op;
					res_stat(6).regS_addr <= fs;
					res_stat(6).data_regS <= incoming_regS;
					res_stat(6).regT_addr <= ft;
					res_stat(6).data_regT <= incoming_regT;
					res_stat(6).resultReg <= fd;
					res_stat(6).resultValue <= X"00000000";
					res_stat(6).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(6).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(6).ready	<= "10";
					ELSE
						res_stat(6).ready	<= "11";
					END IF;
					res_stat(6).fmt_val <= fmt;
					res_stat(6).inPipe <= '0';
					--res_stat(6).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(7).isEmpty = '0') THEN
					res_stat(7).isEmpty <= '1';
					res_stat(7).instruction <= op;
					res_stat(7).regS_addr <= fs;
					res_stat(7).data_regS <= incoming_regS;
					res_stat(7).regT_addr <= ft;
					res_stat(7).data_regT <= incoming_regT;
					res_stat(7).resultReg <= fd;
					res_stat(7).resultValue <= X"00000000";
					res_stat(7).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(7).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(7).ready	<= "10";
					ELSE
						res_stat(7).ready	<= "11";
					END IF;
					res_stat(7).fmt_val <= fmt;
					res_stat(7).inPipe <= '0';
					--res_stat(7).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(8).isEmpty = '0') THEN
					res_stat(8).isEmpty <= '1';
					res_stat(8).instruction <= op;
					res_stat(8).regS_addr <= fs;
					res_stat(8).data_regS <= incoming_regS;
					res_stat(8).regT_addr <= ft;
					res_stat(8).data_regT <= incoming_regT;
					res_stat(8).resultReg <= fd;
					res_stat(8).resultValue <= X"00000000";
					res_stat(8).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(8).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(8).ready	<= "10";
					ELSE
						res_stat(8).ready	<= "11";
					END IF;
					res_stat(8).fmt_val <= fmt;
					res_stat(8).inPipe <= '0';
					--res_stat(8).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(9).isEmpty = '0') THEN
					res_stat(9).isEmpty <= '1';
					res_stat(9).instruction <= op;
					res_stat(9).regS_addr <= fs;
					res_stat(9).data_regS <= incoming_regS;
					res_stat(9).regT_addr <= ft;
					res_stat(9).data_regT <= incoming_regT;
					res_stat(9).resultReg <= fd;
					res_stat(9).resultValue <= X"00000000";
					res_stat(9).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(9).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(9).ready	<= "10";
					ELSE
						res_stat(9).ready	<= "11";
					END IF;
					res_stat(9).fmt_val <= fmt;
					res_stat(9).inPipe <= '0';
					--res_stat(9).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(10).isEmpty = '0') THEN
					res_stat(10).isEmpty <= '1';
					res_stat(10).instruction <= op;
					res_stat(10).regS_addr <= fs;
					res_stat(10).data_regS <= incoming_regS;
					res_stat(10).regT_addr <= ft;
					res_stat(10).data_regT <= incoming_regT;
					res_stat(10).resultReg <= fd;
					res_stat(10).resultValue <= X"00000000";
					res_stat(10).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(10).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(10).ready	<= "10";
					ELSE
						res_stat(10).ready	<= "11";
					END IF;
					res_stat(10).fmt_val <= fmt;
					res_stat(10).inPipe <= '0';
					--res_stat(10).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(11).isEmpty = '0') THEN
					res_stat(11).isEmpty <= '1';
					res_stat(11).instruction <= op;
					res_stat(11).regS_addr <= fs;
					res_stat(11).data_regS <= incoming_regS;
					res_stat(11).regT_addr <= ft;
					res_stat(11).data_regT <= incoming_regT;
					res_stat(11).resultReg <= fd;
					res_stat(11).resultValue <= X"00000000";
					res_stat(11).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(11).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(11).ready	<= "10";
					ELSE
						res_stat(11).ready	<= "11";
					END IF;
					res_stat(11).fmt_val <= fmt;
					res_stat(11).inPipe <= '0';
					--res_stat(11).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(12).isEmpty = '0') THEN
					res_stat(12).isEmpty <= '1';
					res_stat(12).instruction <= op;
					res_stat(12).regS_addr <= fs;
					res_stat(12).data_regS <= incoming_regS;
					res_stat(12).regT_addr <= ft;
					res_stat(12).data_regT <= incoming_regT;
					res_stat(12).resultReg <= fd;
					res_stat(12).resultValue <= X"00000000";
					res_stat(12).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(12).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(12).ready	<= "10";
					ELSE
						res_stat(12).ready	<= "11";
					END IF;
					res_stat(12).fmt_val <= fmt;
					res_stat(12).inPipe <= '0';
					--res_stat(12).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(13).isEmpty = '0') THEN
					res_stat(13).isEmpty <= '1';
					res_stat(13).instruction <= op;
					res_stat(13).regS_addr <= fs;
					res_stat(13).data_regS <= incoming_regS;
					res_stat(13).regT_addr <= ft;
					res_stat(13).data_regT <= incoming_regT;
					res_stat(13).resultReg <= fd;
					res_stat(13).resultValue <= X"00000000";
					res_stat(13).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(13).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(13).ready	<= "10";
					ELSE
						res_stat(13).ready	<= "11";
					END IF;
					res_stat(13).fmt_val <= fmt;
					res_stat(13).inPipe <= '0';
					--res_stat(13).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(14).isEmpty = '0') THEN
					res_stat(14).isEmpty <= '1';
					res_stat(14).instruction <= op;
					res_stat(14).regS_addr <= fs;
					res_stat(14).data_regS <= incoming_regS;
					res_stat(14).regT_addr <= ft;
					res_stat(14).data_regT <= incoming_regT;
					res_stat(14).resultReg <= fd;
					res_stat(14).resultValue <= X"00000000";
					res_stat(14).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(14).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(14).ready	<= "10";
					ELSE
						res_stat(14).ready	<= "11";
					END IF;
					res_stat(14).fmt_val <= fmt;
					res_stat(14).inPipe <= '0';
					--res_stat(14).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(15).isEmpty = '0') THEN
					res_stat(15).isEmpty <= '1';
					res_stat(15).instruction <= op;
					res_stat(15).regS_addr <= fs;
					res_stat(15).data_regS <= incoming_regS;
					res_stat(15).regT_addr <= ft;
					res_stat(15).data_regT <= incoming_regT;
					res_stat(15).resultReg <= fd;
					res_stat(15).resultValue <= X"00000000";
					res_stat(15).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(15).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(15).ready	<= "10";
					ELSE
						res_stat(15).ready	<= "11";
					END IF;
					res_stat(15).fmt_val <= fmt;
					res_stat(15).inPipe <= '0';
					--res_stat(15).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(16).isEmpty = '0') THEN
					res_stat(16).isEmpty <= '1';
					res_stat(16).instruction <= op;
					res_stat(16).regS_addr <= fs;
					res_stat(16).data_regS <= incoming_regS;
					res_stat(16).regT_addr <= ft;
					res_stat(16).data_regT <= incoming_regT;
					res_stat(16).resultReg <= fd;
					res_stat(16).resultValue <= X"00000000";
					res_stat(16).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(16).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(16).ready	<= "10";
					ELSE
						res_stat(16).ready	<= "11";
					END IF;
					res_stat(16).fmt_val <= fmt;
					res_stat(16).inPipe <= '0';
					--res_stat(16).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(17).isEmpty = '0') THEN
					res_stat(17).isEmpty <= '1';
					res_stat(17).instruction <= op;
					res_stat(17).regS_addr <= fs;
					res_stat(17).data_regS <= incoming_regS;
					res_stat(17).regT_addr <= ft;
					res_stat(17).data_regT <= incoming_regT;
					res_stat(17).resultReg <= fd;
					res_stat(17).resultValue <= X"00000000";
					res_stat(17).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(17).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(17).ready	<= "10";
					ELSE
						res_stat(17).ready	<= "11";
					END IF;
					res_stat(17).fmt_val <= fmt;
					res_stat(17).inPipe <= '0';
					--res_stat(17).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(18).isEmpty = '0') THEN
					res_stat(18).isEmpty <= '1';
					res_stat(18).instruction <= op;
					res_stat(18).regS_addr <= fs;
					res_stat(18).data_regS <= incoming_regS;
					res_stat(18).regT_addr <= ft;
					res_stat(18).data_regT <= incoming_regT;
					res_stat(18).resultReg <= fd;
					res_stat(18).resultValue <= X"00000000";
					res_stat(18).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(18).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(18).ready	<= "10";
					ELSE
						res_stat(18).ready	<= "11";
					END IF;
					res_stat(18).fmt_val <= fmt;
					res_stat(18).inPipe <= '0';
					--res_stat(18).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(19).isEmpty = '0') THEN
					res_stat(19).isEmpty <= '1';
					res_stat(19).instruction <= op;
					res_stat(19).regS_addr <= fs;
					res_stat(19).data_regS <= incoming_regS;
					res_stat(19).regT_addr <= ft;
					res_stat(19).data_regT <= incoming_regT;
					res_stat(19).resultReg <= fd;
					res_stat(19).resultValue <= X"00000000";
					res_stat(19).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(19).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(19).ready	<= "10";
					ELSE
						res_stat(19).ready	<= "11";
					END IF;
					res_stat(19).fmt_val <= fmt;
					res_stat(19).inPipe <= '0';
					--res_stat(19).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(20).isEmpty = '0') THEN	
					res_stat(20).isEmpty <= '1';
					res_stat(20).instruction <= op;
					res_stat(20).regS_addr <= fs;
					res_stat(20).data_regS <= incoming_regS;
					res_stat(20).regT_addr <= ft;
					res_stat(20).data_regT <= incoming_regT;
					res_stat(20).resultReg <= fd;
					res_stat(20).resultValue <= X"00000000";
					res_stat(20).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(20).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(20).ready	<= "10";
					ELSE
						res_stat(20).ready	<= "11";
					END IF;
					res_stat(20).fmt_val <= fmt;
					res_stat(20).inPipe <= '0';
					--res_stat(20).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(21).isEmpty = '0') THEN
					res_stat(21).isEmpty <= '1';
					res_stat(21).instruction <= op;
					res_stat(21).regS_addr <= fs;
					res_stat(21).data_regS <= incoming_regS;
					res_stat(21).regT_addr <= ft;
					res_stat(21).data_regT <= incoming_regT;
					res_stat(21).resultReg <= fd;
					res_stat(21).resultValue <= X"00000000";
					res_stat(21).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(21).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(21).ready	<= "10";
					ELSE
						res_stat(21).ready	<= "11";
					END IF;
					res_stat(21).fmt_val <= fmt;
					res_stat(21).inPipe <= '0';
					--res_stat(21).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(22).isEmpty = '0') THEN
					res_stat(22).isEmpty <= '1';
					res_stat(22).instruction <= op;
					res_stat(22).regS_addr <= fs;
					res_stat(22).data_regS <= incoming_regS;
					res_stat(22).regT_addr <= ft;
					res_stat(22).data_regT <= incoming_regT;
					res_stat(22).resultReg <= fd;
					res_stat(22).resultValue <= X"00000000";
					res_stat(22).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(22).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(22).ready	<= "10";
					ELSE
						res_stat(22).ready	<= "11";
					END IF;
					res_stat(22).fmt_val <= fmt;
					res_stat(22).inPipe <= '0';
					--res_stat(22).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(23).isEmpty = '0') THEN
					res_stat(23).isEmpty <= '1';
					res_stat(23).instruction <= op;
					res_stat(23).regS_addr <= fs;
					res_stat(23).data_regS <= incoming_regS;
					res_stat(23).regT_addr <= ft;
					res_stat(23).data_regT <= incoming_regT;
					res_stat(23).resultReg <= fd;
					res_stat(23).resultValue <= X"00000000";
					res_stat(23).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(23).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(23).ready	<= "10";
					ELSE
						res_stat(23).ready	<= "11";
					END IF;
					res_stat(23).fmt_val <= fmt;
					res_stat(23).inPipe <= '0';
					--res_stat(23).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(24).isEmpty = '0') THEN
					res_stat(24).isEmpty <= '1';
					res_stat(24).instruction <= op;
					res_stat(24).regS_addr <= fs;
					res_stat(24).data_regS <= incoming_regS;
					res_stat(24).regT_addr <= ft;
					res_stat(24).data_regT <= incoming_regT;
					res_stat(24).resultReg <= fd;
					res_stat(24).resultValue <= X"00000000";
					res_stat(24).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(24).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(24).ready	<= "10";
					ELSE
						res_stat(24).ready	<= "11";
					END IF;
					res_stat(24).fmt_val <= fmt;
					res_stat(24).inPipe <= '0';
					--res_stat(24).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(25).isEmpty = '0') THEN
					res_stat(25).isEmpty <= '1';
					res_stat(25).instruction <= op;
					res_stat(25).regS_addr <= fs;
					res_stat(25).data_regS <= incoming_regS;
					res_stat(25).regT_addr <= ft;
					res_stat(25).data_regT <= incoming_regT;
					res_stat(25).resultReg <= fd;
					res_stat(25).resultValue <= X"00000000";
					res_stat(25).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(25).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(25).ready	<= "10";
					ELSE
						res_stat(25).ready	<= "11";
					END IF;
					res_stat(25).fmt_val <= fmt;
					res_stat(25).inPipe <= '0';
					--res_stat(25).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(26).isEmpty = '0') THEN
					res_stat(26).isEmpty <= '1';
					res_stat(26).instruction <= op;
					res_stat(26).regS_addr <= fs;
					res_stat(26).data_regS <= incoming_regS;
					res_stat(26).regT_addr <= ft;
					res_stat(26).data_regT <= incoming_regT;
					res_stat(26).resultReg <= fd;
					res_stat(26).resultValue <= X"00000000";
					res_stat(26).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(26).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(26).ready	<= "10";
					ELSE
						res_stat(26).ready	<= "11";
					END IF;
					res_stat(26).fmt_val <= fmt;
					res_stat(26).inPipe <= '0';
					--res_stat(26).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(27).isEmpty = '0') THEN
					res_stat(27).isEmpty <= '1';
					res_stat(27).instruction <= op;
					res_stat(27).regS_addr <= fs;
					res_stat(27).data_regS <= incoming_regS;
					res_stat(27).regT_addr <= ft;
					res_stat(27).data_regT <= incoming_regT;
					res_stat(27).resultReg <= fd;
					res_stat(27).resultValue <= X"00000000";
					res_stat(27).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(27).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(27).ready	<= "10";
					ELSE
						res_stat(27).ready	<= "11";
					END IF;
					res_stat(27).fmt_val <= fmt;
					res_stat(27).inPipe <= '0';
					--res_stat(27).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(28).isEmpty = '0') THEN
					res_stat(28).isEmpty <= '1';
					res_stat(28).instruction <= op;
					res_stat(28).regS_addr <= fs;
					res_stat(28).data_regS <= incoming_regS;
					res_stat(28).regT_addr <= ft;
					res_stat(28).data_regT <= incoming_regT;
					res_stat(28).resultReg <= fd;
					res_stat(28).resultValue <= X"00000000";
					res_stat(28).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(28).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(28).ready	<= "10";
					ELSE
						res_stat(28).ready	<= "11";
					END IF;
					res_stat(28).fmt_val <= fmt;
					res_stat(28).inPipe <= '0';
					--res_stat(28).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(29).isEmpty = '0') THEN
					res_stat(29).isEmpty <= '1';
					res_stat(29).instruction <= op;
					res_stat(29).regS_addr <= fs;
					res_stat(29).data_regS <= incoming_regS;
					res_stat(29).regT_addr <= ft;
					res_stat(29).data_regT <= incoming_regT;
					res_stat(29).resultReg <= fd;
					res_stat(29).resultValue <= X"00000000";
					res_stat(29).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(29).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(29).ready	<= "10";
					ELSE
						res_stat(29).ready	<= "11";
					END IF;
					res_stat(29).fmt_val <= fmt;
					res_stat(29).inPipe <= '0';
					--res_stat(29).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(30).isEmpty = '0') THEN
					res_stat(30).isEmpty <= '1';
					res_stat(30).instruction <= op;
					res_stat(30).regS_addr <= fs;
					res_stat(30).data_regS <= incoming_regS;
					res_stat(30).regT_addr <= ft;
					res_stat(30).data_regT <= incoming_regT;
					res_stat(30).resultReg <= fd;
					res_stat(30).resultValue <= X"00000000";
					res_stat(30).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(30).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(30).ready	<= "10";
					ELSE
						res_stat(30).ready	<= "11";
					END IF;
					res_stat(30).fmt_val <= fmt;
					res_stat(30).inPipe <= '0';
					--res_stat(30).isEmpty <= '1';
					res_stat_full <= '0';
				ELSIF(res_stat(31).isEmpty = '0') THEN
					res_stat(31).isEmpty <= '1';
					res_stat(31).instruction <= op;
					res_stat(31).regS_addr <= fs;
					res_stat(31).data_regS <= incoming_regS;
					res_stat(31).regT_addr <= ft;
					res_stat(31).data_regT <= incoming_regT;
					res_stat(31).resultReg <= fd;
					res_stat(31).resultValue <= X"00000000";
					res_stat(31).timer <= "10000";
					IF(CONV_INTEGER(fs) = CONV_INTEGER(incoming_regS)) THEN
						res_stat(31).ready	<= "01";
					ELSIF(CONV_INTEGER(ft) = CONV_INTEGER(incoming_regT)) THEN
						res_stat(31).ready	<= "10";
					ELSE
						res_stat(31).ready	<= "11";
					END IF;
					res_stat(31).fmt_val <= fmt;
					res_stat(31).inPipe <= '0';
					--res_stat(31).isEmpty <= '1';
					res_stat_full <= '0';
				ELSE
					res_stat_full <= '1';
				END IF;
			END IF;
			
			tester <= res_stat(0).isEmpty;
			tester2 <= res_stat(1).isEmpty;
			tester3	<= res_stat(31).isEmpty;
			tester4	 <= res_stat(0).ready;
			tester5	  <= res_stat(0).resultReg;
			tester6	   <= res_pipe(0).stored_item.timer;
			
			
			IF(CDB_valid = '1') THEN -- replace stuff in res_stat
				IF(res_stat(0).isEmpty = '1') THEN --NOT(res_stat(0).ready = "11")) THEN
					IF((res_stat(0).ready = "10") AND (res_stat(0).regT_addr = CDB_addr)) THEN
						res_stat(0).data_regT <= CDB_data;
						res_stat(0).ready <= "11";
					ELSIF((res_stat(0).ready = "01") AND (res_stat(0).regS_addr = CDB_addr)) THEN
						res_stat(0).data_regS <= CDB_data;
						res_stat(0).ready <= "11";
					END IF;
				ELSIF(res_stat(1).isEmpty = '1') THEN
					IF((res_stat(1).ready = "10") AND (res_stat(1).regT_addr = CDB_addr)) THEN
						res_stat(1).data_regT <= CDB_data;
						res_stat(1).ready <= "11";
					ELSIF((res_stat(1).ready = "01") AND (res_stat(1).regS_addr = CDB_addr)) THEN
						res_stat(1).data_regS <= CDB_data;
						res_stat(1).ready <= "11";
					END IF;
				ELSIF(res_stat(2).isEmpty = '1') THEN
					IF((res_stat(2).ready = "10") AND (res_stat(2).regT_addr = CDB_addr)) THEN
						res_stat(2).data_regT <= CDB_data;
						res_stat(2).ready <= "11";
					ELSIF((res_stat(2).ready = "01") AND (res_stat(2).regS_addr = CDB_addr)) THEN
						res_stat(2).data_regS <= CDB_data;
						res_stat(2).ready <= "11";
					END IF;
				ELSIF(res_stat(3).isEmpty = '1') THEN
					IF((res_stat(3).ready = "10") AND (res_stat(3).regT_addr = CDB_addr)) THEN
						res_stat(3).data_regT <= CDB_data;
						res_stat(3).ready <= "11";
					ELSIF((res_stat(3).ready = "01") AND (res_stat(3).regS_addr = CDB_addr)) THEN
						res_stat(3).data_regS <= CDB_data;
						res_stat(3).ready <= "11";
					END IF;
				ELSIF(res_stat(4).isEmpty = '1') THEN
					IF((res_stat(4).ready = "10") AND (res_stat(4).regT_addr = CDB_addr)) THEN
						res_stat(4).data_regT <= CDB_data;
						res_stat(4).ready <= "11";
					ELSIF((res_stat(4).ready = "01") AND (res_stat(4).regS_addr = CDB_addr)) THEN
						res_stat(4).data_regS <= CDB_data;
						res_stat(4).ready <= "11";
					END IF;
				ELSIF(res_stat(5).isEmpty = '1') THEN
					IF((res_stat(5).ready = "10") AND (res_stat(5).regT_addr = CDB_addr)) THEN
						res_stat(5).data_regT <= CDB_data;
						res_stat(5).ready <= "11";
					ELSIF((res_stat(5).ready = "01") AND (res_stat(5).regS_addr = CDB_addr)) THEN
						res_stat(5).data_regS <= CDB_data;
						res_stat(5).ready <= "11";
					END IF;
				ELSIF(res_stat(6).isEmpty = '1') THEN
					IF((res_stat(6).ready = "10") AND (res_stat(6).regT_addr = CDB_addr)) THEN
						res_stat(6).data_regT <= CDB_data;
						res_stat(6).ready <= "11";
					ELSIF((res_stat(6).ready = "01") AND (res_stat(6).regS_addr = CDB_addr)) THEN
						res_stat(6).data_regS <= CDB_data;
						res_stat(6).ready <= "11";
					END IF;
				ELSIF(res_stat(7).isEmpty = '1') THEN
					IF((res_stat(7).ready = "10") AND (res_stat(7).regT_addr = CDB_addr)) THEN
						res_stat(7).data_regT <= CDB_data;
						res_stat(7).ready <= "11";
					ELSIF((res_stat(7).ready = "01") AND (res_stat(7).regS_addr = CDB_addr)) THEN
						res_stat(7).data_regS <= CDB_data;
						res_stat(7).ready <= "11";
					END IF;
				ELSIF(res_stat(8).isEmpty = '1') THEN
					IF((res_stat(8).ready = "10") AND (res_stat(8).regT_addr = CDB_addr)) THEN
						res_stat(8).data_regT <= CDB_data;
						res_stat(8).ready <= "11";
					ELSIF((res_stat(8).ready = "01") AND (res_stat(8).regS_addr = CDB_addr)) THEN
						res_stat(8).data_regS <= CDB_data;
						res_stat(8).ready <= "11";
					END IF;
				ELSIF(res_stat(9).isEmpty = '1') THEN
					IF((res_stat(9).ready = "10") AND (res_stat(9).regT_addr = CDB_addr)) THEN
						res_stat(9).data_regT <= CDB_data;
						res_stat(9).ready <= "11";
					ELSIF((res_stat(9).ready = "01") AND (res_stat(9).regS_addr = CDB_addr)) THEN
						res_stat(9).data_regS <= CDB_data;
						res_stat(9).ready <= "11";
					END IF;
				ELSIF(res_stat(10).isEmpty = '1') THEN
					IF((res_stat(10).ready = "10") AND (res_stat(10).regT_addr = CDB_addr)) THEN
						res_stat(10).data_regT <= CDB_data;
						res_stat(10).ready <= "11";
					ELSIF((res_stat(10).ready = "01") AND (res_stat(10).regS_addr = CDB_addr)) THEN
						res_stat(10).data_regS <= CDB_data;
						res_stat(10).ready <= "11";
					END IF;
				ELSIF(res_stat(11).isEmpty = '1') THEN
					IF((res_stat(11).ready = "10") AND (res_stat(11).regT_addr = CDB_addr)) THEN
						res_stat(11).data_regT <= CDB_data;
						res_stat(11).ready <= "11";
					ELSIF((res_stat(11).ready = "01") AND (res_stat(11).regS_addr = CDB_addr)) THEN
						res_stat(11).data_regS <= CDB_data;
						res_stat(11).ready <= "11";
					END IF;
				ELSIF(res_stat(12).isEmpty = '1') THEN
					IF((res_stat(12).ready = "10") AND (res_stat(12).regT_addr = CDB_addr)) THEN
						res_stat(12).data_regT <= CDB_data;
						res_stat(12).ready <= "11";
					ELSIF((res_stat(12).ready = "01") AND (res_stat(12).regS_addr = CDB_addr)) THEN
						res_stat(12).data_regS <= CDB_data;
						res_stat(12).ready <= "11";
					END IF;
				ELSIF(res_stat(13).isEmpty = '1') THEN
					IF((res_stat(13).ready = "10") AND (res_stat(13).regT_addr = CDB_addr)) THEN
						res_stat(13).data_regT <= CDB_data;
						res_stat(13).ready <= "11";
					ELSIF((res_stat(13).ready = "01") AND (res_stat(13).regS_addr = CDB_addr)) THEN
						res_stat(13).data_regS <= CDB_data;
						res_stat(13).ready <= "11";
					END IF;
				ELSIF(res_stat(14).isEmpty = '1') THEN
					IF((res_stat(14).ready = "10") AND (res_stat(14).regT_addr = CDB_addr)) THEN
						res_stat(14).data_regT <= CDB_data;
						res_stat(14).ready <= "11";
					ELSIF((res_stat(14).ready = "01") AND (res_stat(14).regS_addr = CDB_addr)) THEN
						res_stat(14).data_regS <= CDB_data;
						res_stat(14).ready <= "11";
					END IF;
				ELSIF(res_stat(15).isEmpty = '1') THEN
					IF((res_stat(15).ready = "10") AND (res_stat(15).regT_addr = CDB_addr)) THEN
						res_stat(15).data_regT <= CDB_data;
						res_stat(15).ready <= "11";
					ELSIF((res_stat(15).ready = "01") AND (res_stat(15).regS_addr = CDB_addr)) THEN
						res_stat(15).data_regS <= CDB_data;
						res_stat(15).ready <= "11";
					END IF;
				ELSIF(res_stat(16).isEmpty = '1') THEN
					IF((res_stat(16).ready = "10") AND (res_stat(16).regT_addr = CDB_addr)) THEN
						res_stat(16).data_regT <= CDB_data;
						res_stat(16).ready <= "11";
					ELSIF((res_stat(16).ready = "01") AND (res_stat(16).regS_addr = CDB_addr)) THEN
						res_stat(16).data_regS <= CDB_data;
						res_stat(16).ready <= "11";
					END IF;
				ELSIF(res_stat(17).isEmpty = '1') THEN
					IF((res_stat(17).ready = "10") AND (res_stat(17).regT_addr = CDB_addr)) THEN
						res_stat(17).data_regT <= CDB_data;
						res_stat(17).ready <= "11";
					ELSIF((res_stat(17).ready = "01") AND (res_stat(17).regS_addr = CDB_addr)) THEN
						res_stat(17).data_regS <= CDB_data;
						res_stat(17).ready <= "11";
					END IF;
				ELSIF(res_stat(18).isEmpty = '1') THEN
					IF((res_stat(18).ready = "10") AND (res_stat(18).regT_addr = CDB_addr)) THEN
						res_stat(18).data_regT <= CDB_data;
						res_stat(18).ready <= "11";
					ELSIF((res_stat(18).ready = "01") AND (res_stat(18).regS_addr = CDB_addr)) THEN
						res_stat(18).data_regS <= CDB_data;
						res_stat(18).ready <= "11";
					END IF;
				ELSIF(res_stat(19).isEmpty = '1') THEN
					IF((res_stat(19).ready = "10") AND (res_stat(19).regT_addr = CDB_addr)) THEN
						res_stat(19).data_regT <= CDB_data;
						res_stat(19).ready <= "11";
					ELSIF((res_stat(19).ready = "01") AND (res_stat(19).regS_addr = CDB_addr)) THEN
						res_stat(19).data_regS <= CDB_data;
						res_stat(19).ready <= "11";
					END IF;
				ELSIF(res_stat(20).isEmpty = '1') THEN
					IF((res_stat(20).ready = "10") AND (res_stat(20).regT_addr = CDB_addr)) THEN
						res_stat(20).data_regT <= CDB_data;
						res_stat(20).ready <= "11";
					ELSIF((res_stat(20).ready = "01") AND (res_stat(20).regS_addr = CDB_addr)) THEN
						res_stat(20).data_regS <= CDB_data;
						res_stat(20).ready <= "11";
					END IF;
				ELSIF(res_stat(21).isEmpty = '1') THEN
					IF((res_stat(21).ready = "10") AND (res_stat(21).regT_addr = CDB_addr)) THEN
						res_stat(21).data_regT <= CDB_data;
						res_stat(21).ready <= "11";
					ELSIF((res_stat(21).ready = "01") AND (res_stat(21).regS_addr = CDB_addr)) THEN
						res_stat(21).data_regS <= CDB_data;
						res_stat(21).ready <= "11";
					END IF;
				ELSIF(res_stat(22).isEmpty = '1') THEN
					IF((res_stat(22).ready = "10") AND (res_stat(22).regT_addr = CDB_addr)) THEN
						res_stat(22).data_regT <= CDB_data;
						res_stat(22).ready <= "11";
					ELSIF((res_stat(22).ready = "01") AND (res_stat(22).regS_addr = CDB_addr)) THEN
						res_stat(22).data_regS <= CDB_data;
						res_stat(22).ready <= "11";
					END IF;
				ELSIF(res_stat(23).isEmpty = '1') THEN
					IF((res_stat(23).ready = "10") AND (res_stat(23).regT_addr = CDB_addr)) THEN
						res_stat(23).data_regT <= CDB_data;
						res_stat(23).ready <= "11";
					ELSIF((res_stat(23).ready = "01") AND (res_stat(23).regS_addr = CDB_addr)) THEN
						res_stat(23).data_regS <= CDB_data;
						res_stat(23).ready <= "11";
					END IF;
				ELSIF(res_stat(24).isEmpty = '1') THEN
					IF((res_stat(24).ready = "10") AND (res_stat(24).regT_addr = CDB_addr)) THEN
						res_stat(24).data_regT <= CDB_data;
						res_stat(24).ready <= "11";
					ELSIF((res_stat(24).ready = "01") AND (res_stat(24).regS_addr = CDB_addr)) THEN
						res_stat(24).data_regS <= CDB_data;
						res_stat(24).ready <= "11";
					END IF;
				ELSIF(res_stat(25).isEmpty = '1') THEN
					IF((res_stat(25).ready = "10") AND (res_stat(25).regT_addr = CDB_addr)) THEN
						res_stat(25).data_regT <= CDB_data;
						res_stat(25).ready <= "11";
					ELSIF((res_stat(25).ready = "01") AND (res_stat(25).regS_addr = CDB_addr)) THEN
						res_stat(25).data_regS <= CDB_data;
						res_stat(25).ready <= "11";
					END IF;
				ELSIF(res_stat(26).isEmpty = '1') THEN
					IF((res_stat(26).ready = "10") AND (res_stat(26).regT_addr = CDB_addr)) THEN
						res_stat(26).data_regT <= CDB_data;
						res_stat(26).ready <= "11";
					ELSIF((res_stat(26).ready = "01") AND (res_stat(26).regS_addr = CDB_addr)) THEN
						res_stat(26).data_regS <= CDB_data;
						res_stat(26).ready <= "11";
					END IF;
				ELSIF(res_stat(27).isEmpty = '1') THEN
					IF((res_stat(27).ready = "10") AND (res_stat(27).regT_addr = CDB_addr)) THEN
						res_stat(27).data_regT <= CDB_data;
						res_stat(27).ready <= "11";
					ELSIF((res_stat(27).ready = "01") AND (res_stat(27).regS_addr = CDB_addr)) THEN
						res_stat(27).data_regS <= CDB_data;
						res_stat(27).ready <= "11";
					END IF;
				ELSIF(res_stat(28).isEmpty = '1') THEN
					IF((res_stat(28).ready = "10") AND (res_stat(28).regT_addr = CDB_addr)) THEN
						res_stat(28).data_regT <= CDB_data;
						res_stat(28).ready <= "11";
					ELSIF((res_stat(28).ready = "01") AND (res_stat(28).regS_addr = CDB_addr)) THEN
						res_stat(28).data_regS <= CDB_data;
						res_stat(28).ready <= "11";
					END IF;
				ELSIF(res_stat(29).isEmpty = '1') THEN
					IF((res_stat(29).ready = "10") AND (res_stat(29).regT_addr = CDB_addr)) THEN
						res_stat(29).data_regT <= CDB_data;
						res_stat(29).ready <= "11";
					ELSIF((res_stat(29).ready = "01") AND (res_stat(29).regS_addr = CDB_addr)) THEN
						res_stat(29).data_regS <= CDB_data;
						res_stat(29).ready <= "11";
					END IF;
				ELSIF(res_stat(30).isEmpty = '1') THEN
					IF((res_stat(30).ready = "10") AND (res_stat(30).regT_addr = CDB_addr)) THEN
						res_stat(30).data_regT <= CDB_data;
						res_stat(30).ready <= "11";
					ELSIF((res_stat(30).ready = "01") AND (res_stat(30).regS_addr = CDB_addr)) THEN
						res_stat(30).data_regS <= CDB_data;
						res_stat(30).ready <= "11";
					END IF;
				ELSIF(res_stat(31).isEmpty = '1') THEN
					IF((res_stat(31).ready = "10") AND (res_stat(31).regT_addr = CDB_addr)) THEN
						res_stat(31).data_regT <= CDB_data;
						res_stat(31).ready <= "11";
					ELSIF((res_stat(31).ready = "01") AND (res_stat(31).regS_addr = CDB_addr)) THEN
						res_stat(31).data_regS <= CDB_data;
						res_stat(31).ready <= "11";
					END IF;
				END IF;
			END IF;
		
			------
			IF(res_stat(0).isEmpty = '1') THEN -- placing item into pipeline
				IF(res_stat(0).inPipe = '0' AND res_stat(0).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(0).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(0);
						res_stat(0).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(0).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(0);
						res_stat(0).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(0).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(0);
						res_stat(0).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(0).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(0);
						res_stat(0).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(0).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(0);
						res_stat(0).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(0).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(0);
						res_stat(0).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(0).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(0);
						res_stat(0).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(0).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(0);
						res_stat(0).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(1).isEmpty = '1') THEN
				IF(res_stat(1).inPipe = '0' AND res_stat(1).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(1).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(1);
						res_stat(1).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(1).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(1);
						res_stat(1).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(1).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(1);
						res_stat(1).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(1).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(1);
						res_stat(1).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(1).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(1);
						res_stat(1).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(1).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(1);
						res_stat(1).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(1).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(1);
						res_stat(1).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(1).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(1);
						res_stat(1).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(2).isEmpty = '1') THEN
				IF(res_stat(2).inPipe = '0' AND res_stat(2).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(2).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(2);
						res_stat(2).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(2).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(2);
						res_stat(2).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(2).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(2);
						res_stat(2).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(2).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(2);
						res_stat(2).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(2).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(2);
						res_stat(2).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(2).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(2);
						res_stat(2).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(2).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(2);
						res_stat(2).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(2).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(2);
						res_stat(2).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(3).isEmpty = '1') THEN
				IF(res_stat(3).inPipe = '0' AND res_stat(3).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(3).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(3);
						res_stat(3).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(3).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(3);
						res_stat(3).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(3).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(3);
						res_stat(3).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(3).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(3);
						res_stat(3).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(3).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(3);
						res_stat(3).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(3).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(3);
						res_stat(3).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(3).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(3);
						res_stat(3).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(3).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(3);
						res_stat(3).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(4).isEmpty = '1') THEN
				IF(res_stat(4).inPipe = '0' AND res_stat(4).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(4).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(4);
						res_stat(4).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(4).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(4);
						res_stat(4).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(4).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(4);
						res_stat(4).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(4).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(4);
						res_stat(4).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(4).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(4);
						res_stat(4).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(4).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(4);
						res_stat(4).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(4).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(4);
						res_stat(4).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(4).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(4);
						res_stat(4).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(5).isEmpty = '1') THEN
				IF(res_stat(5).inPipe = '0' AND res_stat(5).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(5).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(5);
						res_stat(5).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(5).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(5);
						res_stat(5).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(5).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(5);
						res_stat(5).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(5).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(5);
						res_stat(5).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(5).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(5);
						res_stat(5).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(5).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(5);
						res_stat(5).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(5).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(5);
						res_stat(5).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(5).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(5);
						res_stat(5).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(6).isEmpty = '1') THEN
				IF(res_stat(6).inPipe = '0' AND res_stat(6).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(6).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(6);
						res_stat(6).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(6).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(6);
						res_stat(6).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(6).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(6);
						res_stat(6).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(6).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(6);
						res_stat(6).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(6).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(6);
						res_stat(6).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(6).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(6);
						res_stat(6).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(6).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(6);
						res_stat(6).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(6).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(6);
						res_stat(6).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(7).isEmpty = '1') THEN
				IF(res_stat(7).inPipe = '0' AND res_stat(7).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(7).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(7);
						res_stat(7).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(7).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(7);
						res_stat(7).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(7).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(7);
						res_stat(7).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(7).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(7);
						res_stat(7).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(7).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(7);
						res_stat(7).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(7).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(7);
						res_stat(7).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(7).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(7);
						res_stat(7).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(7).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(7);
						res_stat(7).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(8).isEmpty = '1') THEN
				IF(res_stat(8).inPipe = '0' AND res_stat(8).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(8).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(8);
						res_stat(8).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(8).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(8);
						res_stat(8).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(8).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(8);
						res_stat(8).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(8).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(8);
						res_stat(8).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(8).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(8);
						res_stat(8).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(8).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(8);
						res_stat(8).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(8).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(8);
						res_stat(8).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(8).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(8);
						res_stat(8).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(9).isEmpty = '1') THEN
				IF(res_stat(9).inPipe = '0' AND res_stat(9).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(9).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(9);
						res_stat(9).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(9).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(9);
						res_stat(9).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(9).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(9);
						res_stat(9).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(9).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(9);
						res_stat(9).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(9).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(9);
						res_stat(9).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(9).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(9);
						res_stat(9).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(9).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(9);
						res_stat(9).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(9).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(9);
						res_stat(9).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(10).isEmpty = '1') THEN
				IF(res_stat(10).inPipe = '0' AND res_stat(10).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(10).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(10);
						res_stat(10).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(10).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(10);
						res_stat(10).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(10).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(10);
						res_stat(10).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(10).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(10);
						res_stat(10).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(10).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(10);
						res_stat(10).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(10).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(10);
						res_stat(10).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(10).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(10);
						res_stat(10).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(10).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(10);
						res_stat(10).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(11).isEmpty = '1') THEN
				IF(res_stat(11).inPipe = '0' AND res_stat(11).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(11).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(11);
						res_stat(11).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(11).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(11);
						res_stat(11).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(11).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(11);
						res_stat(11).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(11).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(11);
						res_stat(11).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(11).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(11);
						res_stat(11).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(11).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(11);
						res_stat(11).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(11).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(11);
						res_stat(11).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(11).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(11);
						res_stat(11).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(12).isEmpty = '1') THEN
				IF(res_stat(12).inPipe = '0' AND res_stat(12).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(12).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(12);
						res_stat(12).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(12).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(12);
						res_stat(12).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(12).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(12);
						res_stat(12).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(12).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(12);
						res_stat(12).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(12).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(12);
						res_stat(12).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(12).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(12);
						res_stat(12).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(12).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(12);
						res_stat(12).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(12).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(12);
						res_stat(12).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(13).isEmpty = '1') THEN
				IF(res_stat(13).inPipe = '0' AND res_stat(13).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(13).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(13);
						res_stat(13).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(13).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(13);
						res_stat(13).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(13).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(13);
						res_stat(13).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(13).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(13);
						res_stat(13).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(13).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(13);
						res_stat(13).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(13).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(13);
						res_stat(13).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(13).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(13);
						res_stat(13).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(13).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(13);
						res_stat(13).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(14).isEmpty = '1') THEN
				IF(res_stat(14).inPipe = '0' AND res_stat(14).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(14).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(14);
						res_stat(14).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(14).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(14);
						res_stat(14).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(14).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(14);
						res_stat(14).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(14).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(14);
						res_stat(14).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(14).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(14);
						res_stat(14).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(14).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(14);
						res_stat(14).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(14).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(14);
						res_stat(14).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(14).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(14);
						res_stat(14).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(15).isEmpty = '1') THEN
				IF(res_stat(15).inPipe = '0' AND res_stat(15).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(15).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(15);
						res_stat(15).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(15).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(15);
						res_stat(15).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(15).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(15);
						res_stat(15).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(15).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(15);
						res_stat(15).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(15).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(15);
						res_stat(15).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(15).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(15);
						res_stat(15).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(15).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(15);
						res_stat(15).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(15).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(15);
						res_stat(15).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(16).isEmpty = '1') THEN
				IF(res_stat(16).inPipe = '0' AND res_stat(16).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(16).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(16);
						res_stat(16).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(16).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(16);
						res_stat(16).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(16).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(16);
						res_stat(16).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(16).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(16);
						res_stat(16).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(16).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(16);
						res_stat(16).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(16).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(16);
						res_stat(16).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(16).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(16);
						res_stat(16).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(16).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(16);
						res_stat(16).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(17).isEmpty = '1') THEN
				IF(res_stat(17).inPipe = '0' AND res_stat(17).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(17).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(17);
						res_stat(17).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(17).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(17);
						res_stat(17).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(17).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(17);
						res_stat(17).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(17).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(17);
						res_stat(17).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(17).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(17);
						res_stat(17).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(17).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(17);
						res_stat(17).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(17).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(17);
						res_stat(17).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(17).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(17);
						res_stat(17).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(18).isEmpty = '1') THEN
				IF(res_stat(18).inPipe = '0' AND res_stat(18).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(18).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(18);
						res_stat(18).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(18).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(18);
						res_stat(18).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(18).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(18);
						res_stat(18).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(18).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(18);
						res_stat(18).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(18).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(18);
						res_stat(18).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(18).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(18);
						res_stat(18).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(18).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(18);
						res_stat(18).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(18).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(18);
						res_stat(18).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(19).isEmpty = '1') THEN
				IF(res_stat(19).inPipe = '0' AND res_stat(19).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(19).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(19);
						res_stat(19).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(19).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(19);
						res_stat(19).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(19).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(19);
						res_stat(19).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(19).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(19);
						res_stat(19).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(19).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(19);
						res_stat(19).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(19).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(19);
						res_stat(19).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(19).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(19);
						res_stat(19).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(19).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(19);
						res_stat(19).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(20).isEmpty = '1') THEN
				IF(res_stat(20).inPipe = '0' AND res_stat(20).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(20).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(20);
						res_stat(20).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(20).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(20);
						res_stat(20).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(20).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(20);
						res_stat(20).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(20).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(20);
						res_stat(20).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(20).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(20);
						res_stat(20).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(20).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(20);
						res_stat(20).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(20).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(20);
						res_stat(20).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(20).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(20);
						res_stat(20).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(21).isEmpty = '1') THEN
				IF(res_stat(21).inPipe = '0' AND res_stat(21).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(21).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(21);
						res_stat(21).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(21).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(21);
						res_stat(21).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(21).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(21);
						res_stat(21).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(21).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(21);
						res_stat(21).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(21).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(21);
						res_stat(21).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(21).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(21);
						res_stat(21).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(21).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(21);
						res_stat(21).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(21).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(21);
						res_stat(21).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(22).isEmpty = '1') THEN
				IF(res_stat(22).inPipe = '0' AND res_stat(22).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(22).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(22);
						res_stat(22).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(22).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(22);
						res_stat(22).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(22).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(22);
						res_stat(22).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(22).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(22);
						res_stat(22).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(22).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(22);
						res_stat(22).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(22).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(22);
						res_stat(22).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(22).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(22);
						res_stat(22).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(22).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(22);
						res_stat(22).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(23).isEmpty = '1') THEN
				IF(res_stat(23).inPipe = '0' AND res_stat(23).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(23).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(23);
						res_stat(23).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(23).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(23);
						res_stat(23).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(23).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(23);
						res_stat(23).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(23).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(23);
						res_stat(23).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(23).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(23);
						res_stat(23).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(23).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(23);
						res_stat(23).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(23).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(23);
						res_stat(23).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(23).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(23);
						res_stat(23).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(24).isEmpty = '1') THEN
				IF(res_stat(24).inPipe = '0' AND res_stat(24).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(24).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(24);
						res_stat(24).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(24).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(24);
						res_stat(24).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(24).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(24);
						res_stat(24).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(24).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(24);
						res_stat(24).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(24).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(24);
						res_stat(24).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(24).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(24);
						res_stat(24).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(24).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(24);
						res_stat(24).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(24).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(24);
						res_stat(24).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(25).isEmpty = '1') THEN
				IF(res_stat(25).inPipe = '0' AND res_stat(25).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(25).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(25);
						res_stat(25).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(25).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(25);
						res_stat(25).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(25).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(25);
						res_stat(25).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(25).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(25);
						res_stat(25).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(25).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(25);
						res_stat(25).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(25).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(25);
						res_stat(25).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(25).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(25);
						res_stat(25).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(25).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(25);
						res_stat(25).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(26).isEmpty = '1') THEN
				IF(res_stat(26).inPipe = '0' AND res_stat(26).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(26).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(26);
						res_stat(26).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(26).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(26);
						res_stat(26).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(26).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(26);
						res_stat(26).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(26).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(26);
						res_stat(26).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(26).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(26);
						res_stat(26).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(26).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(26);
						res_stat(26).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(26).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(26);
						res_stat(26).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(26).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(26);
						res_stat(26).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(27).isEmpty = '1') THEN
				IF(res_stat(27).inPipe = '0' AND res_stat(27).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(27).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(27);
						res_stat(27).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(27).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(27);
						res_stat(27).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(27).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(27);
						res_stat(27).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(27).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(27);
						res_stat(27).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(27).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(27);
						res_stat(27).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(27).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(27);
						res_stat(27).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(27).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(27);
						res_stat(27).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(27).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(27);
						res_stat(27).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(28).isEmpty = '1') THEN
				IF(res_stat(28).inPipe = '0' AND res_stat(28).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(28).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(28);
						res_stat(28).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(28).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(28);
						res_stat(28).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(28).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(28);
						res_stat(28).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(28).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(28);
						res_stat(28).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(28).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(28);
						res_stat(28).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(28).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(28);
						res_stat(28).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(28).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(28);
						res_stat(28).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(28).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(28);
						res_stat(28).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(29).isEmpty = '1') THEN
				IF(res_stat(29).inPipe = '0' AND res_stat(29).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(29).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(29);
						res_stat(29).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(29).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(29);
						res_stat(29).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(29).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(29);
						res_stat(29).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(29).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(29);
						res_stat(29).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(29).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(29);
						res_stat(29).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(29).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(29);
						res_stat(29).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(29).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(29);
						res_stat(29).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(29).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(29);
						res_stat(29).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(30).isEmpty = '1') THEN
				IF(res_stat(30).inPipe = '0' AND res_stat(30).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(30).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(30);
						res_stat(30).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(30).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(30);
						res_stat(30).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(30).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(30);
						res_stat(30).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(30).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(30);
						res_stat(30).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(30).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(30);
						res_stat(30).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(30).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(30);
						res_stat(30).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(30).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(30);
						res_stat(30).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(30).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(30);
						res_stat(30).isEmpty <= '0';
					END IF;
				END IF;
			ELSIF(res_stat(31).isEmpty = '1') THEN
				IF(res_stat(31).inPipe = '0' AND res_stat(31).ready = "11") THEN
					IF(res_pipe(0).occupied = '0') THEN
						res_stat(31).inPipe <= '1';
						res_pipe(0).occupied <= '1';
						res_pipe(0).stored_item <= res_stat(31);
						res_stat(31).isEmpty <= '0';
					ELSIF(res_pipe(1).occupied = '0') THEN
						res_stat(31).inPipe <= '1';
						res_pipe(1).occupied <= '1';
						res_pipe(1).stored_item <= res_stat(31);
						res_stat(31).isEmpty <= '0';
					ELSIF(res_pipe(2).occupied = '0') THEN
						res_stat(31).inPipe <= '1';
						res_pipe(2).occupied <= '1';
						res_pipe(2).stored_item <= res_stat(31);
						res_stat(31).isEmpty <= '0';
					ELSIF(res_pipe(3).occupied = '0') THEN
						res_stat(31).inPipe <= '1';
						res_pipe(3).occupied <= '1';
						res_pipe(3).stored_item <= res_stat(31);
						res_stat(31).isEmpty <= '0';
					ELSIF(res_pipe(4).occupied = '0') THEN
						res_stat(31).inPipe <= '1';
						res_pipe(4).occupied <= '1';
						res_pipe(4).stored_item <= res_stat(31);
						res_stat(31).isEmpty <= '0';
					ELSIF(res_pipe(5).occupied = '0') THEN
						res_stat(31).inPipe <= '1';
						res_pipe(5).occupied <= '1';
						res_pipe(5).stored_item <= res_stat(31);
						res_stat(31).isEmpty <= '0';
					ELSIF(res_pipe(6).occupied = '0') THEN
						res_stat(31).inPipe <= '1';
						res_pipe(6).occupied <= '1';
						res_pipe(6).stored_item <= res_stat(31);
						res_stat(31).isEmpty <= '0';
					ELSIF(res_pipe(7).occupied = '0') THEN
						res_stat(31).inPipe <= '1';
						res_pipe(7).occupied <= '1';
						res_pipe(7).stored_item <= res_stat(31);
						res_stat(31).isEmpty <= '0';
					END IF;
				END IF;
			END IF;
			------
			IF(res_pipe(0).occupied = '1') THEN -- count down timer
				IF(res_pipe(0).stored_item.timer = "00000" AND ALU_busy = '0') THEN
					regS <= res_pipe(0).stored_item.data_regS;
					regT <= res_pipe(0).stored_item.data_regT;
					dest_reg <= res_pipe(0).stored_item.resultReg;
					fmt_out <= res_pipe(0).stored_item.fmt_val;
				ELSE
					res_pipe(0).stored_item.timer <= res_pipe(0).stored_item.timer - '1';
				END IF;
			END IF;
			IF(res_pipe(1).occupied = '1') THEN
				IF(res_pipe(1).stored_item.timer = "00000" AND ALU_busy = '0') THEN
					regS <= res_pipe(1).stored_item.data_regS;
					regT <= res_pipe(1).stored_item.data_regT;
					dest_reg <= res_pipe(1).stored_item.resultReg;
					fmt_out <= res_pipe(1).stored_item.fmt_val;
				ELSE
					res_pipe(1).stored_item.timer <= res_pipe(1).stored_item.timer - '1';
				END IF;
			END IF;
			IF(res_pipe(2).occupied = '1') THEN
				IF(res_pipe(2).stored_item.timer = "00000" AND ALU_busy = '0') THEN
					regS <= res_pipe(2).stored_item.data_regS;
					regT <= res_pipe(2).stored_item.data_regT;
					dest_reg <= res_pipe(2).stored_item.resultReg;
					fmt_out <= res_pipe(2).stored_item.fmt_val;
				ELSE
					res_pipe(1).stored_item.timer <= res_pipe(1).stored_item.timer - '1';
				END IF;
			END IF;
			IF(res_pipe(3).occupied = '1') THEN
				IF(res_pipe(3).stored_item.timer = "00000" AND ALU_busy = '0') THEN
					regS <= res_pipe(3).stored_item.data_regS;
					regT <= res_pipe(3).stored_item.data_regT;
					dest_reg <= res_pipe(3).stored_item.resultReg;
					fmt_out <= res_pipe(3).stored_item.fmt_val;
				ELSE
					res_pipe(3).stored_item.timer <= res_pipe(3).stored_item.timer - '1';
				END IF;
			END IF;
			IF(res_pipe(4).occupied = '1') THEN
				IF(res_pipe(4).stored_item.timer = "00000" AND ALU_busy = '0') THEN
					regS <= res_pipe(4).stored_item.data_regS;
					regT <= res_pipe(4).stored_item.data_regT;
					dest_reg <= res_pipe(4).stored_item.resultReg;
					fmt_out <= res_pipe(4).stored_item.fmt_val;
				ELSE
					res_pipe(4).stored_item.timer <= res_pipe(4).stored_item.timer - '1';
				END IF;
			END IF;
			IF(res_pipe(5).occupied = '1') THEN
				IF(res_pipe(5).stored_item.timer = "00000" AND ALU_busy = '0') THEN
					regS <= res_pipe(5).stored_item.data_regS;
					regT <= res_pipe(5).stored_item.data_regT;
					dest_reg <= res_pipe(5).stored_item.resultReg;
					fmt_out <= res_pipe(5).stored_item.fmt_val;
				ELSE
					res_pipe(5).stored_item.timer <= res_pipe(5).stored_item.timer - '1';
				END IF;
			END IF;
			IF(res_pipe(6).occupied = '1') THEN
				IF(res_pipe(6).stored_item.timer = "00000" AND ALU_busy = '0') THEN
					regS <= res_pipe(6).stored_item.data_regS;
					regT <= res_pipe(6).stored_item.data_regT;
					dest_reg <= res_pipe(6).stored_item.resultReg;
					fmt_out <= res_pipe(6).stored_item.fmt_val;
				ELSE
					res_pipe(6).stored_item.timer <= res_pipe(6).stored_item.timer - '1';
				END IF;
			END IF;
			IF(res_pipe(7).occupied = '1') THEN
				IF(res_pipe(7).stored_item.timer = "00000" AND ALU_busy = '0') THEN
					regS <= res_pipe(7).stored_item.data_regS;
					regT <= res_pipe(7).stored_item.data_regT;
					dest_reg <= res_pipe(7).stored_item.resultReg;
					fmt_out <= res_pipe(7).stored_item.fmt_val;
				ELSE
					res_pipe(7).stored_item.timer <= res_pipe(7).stored_item.timer - '1';
				END IF;
			END IF;

			----
			IF(CDB_valid = '1') THEN -- place CDB results into result value and empty that paft of pipe
				IF((res_pipe(0).stored_item.resultReg = CDB_addr) AND (res_pipe(0).occupied = '1')) THEN
					res_pipe(0).stored_item.resultValue <= CDB_data;
					res_pipe(0).occupied <= '0';
				ElSIF((res_pipe(1).stored_item.resultReg = CDB_addr) AND (res_pipe(1).occupied = '1')) THEN
					res_pipe(1).stored_item.resultValue <= CDB_data;
					res_pipe(1).occupied <= '0';
				ElSIF((res_pipe(2).stored_item.resultReg = CDB_addr) AND (res_pipe(2).occupied = '1')) THEN
					res_pipe(2).stored_item.resultValue <= CDB_data;
					res_pipe(2).occupied <= '0';
				ElSIF((res_pipe(3).stored_item.resultReg = CDB_addr) AND (res_pipe(3).occupied = '1')) THEN
					res_pipe(3).stored_item.resultValue <= CDB_data;
					res_pipe(3).occupied <= '0';
				ElSIF((res_pipe(4).stored_item.resultReg = CDB_addr) AND (res_pipe(4).occupied = '1')) THEN
					res_pipe(4).stored_item.resultValue <= CDB_data;
					res_pipe(4).occupied <= '0';
				ElSIF((res_pipe(5).stored_item.resultReg = CDB_addr) AND (res_pipe(5).occupied = '1')) THEN
					res_pipe(5).stored_item.resultValue <= CDB_data;
					res_pipe(5).occupied <= '0';
				ElSIF((res_pipe(6).stored_item.resultReg = CDB_addr) AND (res_pipe(6).occupied = '1')) THEN
					res_pipe(6).stored_item.resultValue <= CDB_data;
					res_pipe(6).occupied <= '0';
				ElSIF((res_pipe(7).stored_item.resultReg = CDB_addr) AND (res_pipe(7).occupied = '1')) THEN
					res_pipe(7).stored_item.resultValue <= CDB_data;
					res_pipe(7).occupied <= '0';
				END IF;
			END IF;
			--
			
	
		END IF;
	END PROCESS;
	

END behavior;