LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY ROB IS
	PORT(		
				clock, reset				: IN 	STD_LOGIC; --clock on which to add items
				incomming_instruction	: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				destination					: IN	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				incoming_regS				: IN 	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				incoming_regT				: IN 	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				issuer_stall				: IN	STD_LOGIC;
				incoming_ROB				: IN 	STD_LOGIC;
				CDB_valid					: IN 	STD_LOGIC;
				CDB_addr						: IN 	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				CDB_data						: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				reorder_buffer_full		: OUT	STD_LOGIC;
				ROB_regfile_transmit		: OUT	STD_LOGIC;
				ROB_issuer_stall			: OUT STD_LOGIC;
				regS_in_ROB					: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0);
				regT_in_ROB					: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0);
				regS_regfile_value		: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0);
				regT_regfile_value		: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0);
				result_to_regfile_addr	: OUT	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				result_to_regfile_data	: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0);
				tester, tester_head		: OUT STD_LOGIC_VECTOR( 4 DOWNTO 0);
				tester2, tester_tail		: OUT STD_LOGIC_VECTOR( 4 DOWNTO 0);
				test_push, test_101		: OUT STD_LOGIC_VECTOR( 4 DOWNTO 0);
				test_sigrs,test_sigrt	: OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
				testrs, testrt				: OUT STD_LOGIC_VECTOR( 1 DOWNTO 0);
				tester3, tester4			: OUT STD_LOGIC;
				tester5, tester6			: OUT STD_LOGIC;
				tester7, tester8			: OUT STD_LOGIC;
				tester9, tester10			: OUT STD_LOGIC;
				tester11, tester12		: OUT	STD_LOGIC
		 );
END ROB;

ARCHITECTURE behavior OF ROB IS

TYPE ROB_items IS
	RECORD
		recNum					: STD_LOGIC_VECTOR( 4 DOWNTO 0); -- 0
		resultReg				: STD_LOGIC_VECTOR( 4 DOWNTO 0); -- 1 
		resultValue				: STD_LOGIC_VECTOR(31 DOWNTO 0); -- 2
		valid						: STD_LOGIC;					 		-- 3
		ready						: STD_LOGIC;					 		-- 4
		instruction				: STD_LOGIC_VECTOR(31 DOWNTO 0); -- 5
	END RECORD;
TYPE ROB_Table IS ARRAY (0 TO 31) OF ROB_items;

--TYPE test IS
--	RECORD
--		test_item		: ROB_items;
--		item_signal		: STD_LOGIC;
--	END RECORD;
--SIGNAL test_this 				: test;

SIGNAL reorder_buffer 		: ROb_Table;
SIGNAL ROB_head				: STD_LOGIC_VECTOR( 4 DOWNTO 0) := "11111";
SIGNAL ROB_tail				: STD_LOGIC_VECTOR( 4 DOWNTO 0) := "00000";
SIGNAL ROB_full				: STD_LOGIC := '0';
--SIGNAL test_sig				: STD_LOGIC_VECTOR( 4 DOWNTO 0) := "00000";
SIGNAL toCHeck_head			: STD_LOGIC_VECTOR( 4 DOWNTO 0);
SIGNAL regS_in_ROB_sig		: STD_LOGIC_VECTOR( 1 DOWNTO 0):="00";
SIGNAL regT_in_ROB_sig		: STD_LOGIC_VECTOR( 1 DOWNTO 0):="00";
SIGNAL regS_regfile_value_sig	: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL regT_regfile_value_sig	: STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL hold_rs_ready				: STD_LOGIC_VECTOR( 1 DOWNTO 0);
SIGNAL hold_rt_ready				: STD_LOGIC_VECTOR( 1 DOWNTO 0);

BEGIN
	--ROB_head<="00000";
	--ROB_tail<="00001";
	--test_this.test_item.recNum <= "00001";
	toCHeck_head <= ROB_head + '1';
	
	testrs <= regS_in_ROB_sig;
	testrt <= regT_in_ROB_sig;
	test_sigrs <= regS_regfile_value_sig;
	test_sigrt <= regT_regfile_value_sig;
	
	PROCESS(ROB_head, ROB_tail, ROB_full, reorder_buffer)
	BEGIN
		IF((ROB_head = ROB_tail) AND (reorder_buffer(CONV_INTEGER(ROB_head)).valid = '1')) THEN
			ROB_full <= '1';
			--test_sig <= ROB_head;
			--test_sig <= ROB_tail;
		ELSE
			ROB_full <= '0';
			--test_sig <= test_sig + '1';
		END IF;
	END PROCESS;
	
	reorder_buffer_full <= ROB_full;
	tester_tail 	<= ROB_tail;
	tester_head 	<= ROB_head;
	
	PROCESS(ROB_head, ROB_full, ROB_tail, incomming_instruction, destination, CDB_valid, CDB_addr, CDB_data, clock, reset, incoming_ROB, RegS_in_ROB_sig, RegT_in_ROB_sig, RegS_regfile_value_sig, RegT_regfile_value_sig)
	BEGIN
		IF (reset = '0') THEN
			FOR i IN 0 TO 31 LOOP --loops can only execute using no logic aka assigning
				reorder_buffer(i).recNum <= "00000";
				reorder_buffer(i).resultReg <= "00000";
				reorder_buffer(i).resultValue <= X"00000000";
				reorder_buffer(i).valid <= '0';
				reorder_buffer(i).ready <= '0';
				reorder_buffer(i).instruction <= X"00000000";
			END LOOP;
			ROB_head <= "11111";
			ROB_tail <= "00000";
		ELSIF (RISING_EDGE(clock))	 THEN
			IF (issuer_stall = '0' AND incoming_ROB = '1') THEN -- make sure the issuer has not stalled and the ROB is not full
				IF(ROB_full = '0') THEN
					reorder_buffer(CONV_INTEGER(ROB_tail)).recNum <= ROB_tail;
					reorder_buffer(CONV_INTEGER(ROB_tail)).resultReg <= destination;
					reorder_buffer(CONV_INTEGER(ROB_tail)).resultValue <= X"00000000";
					reorder_buffer(CONV_INTEGER(ROB_tail)).valid <= '1';
					reorder_buffer(CONV_INTEGER(ROB_tail)).ready <= '0';
					reorder_buffer(CONV_INTEGER(ROB_tail)).instruction <= incomming_instruction;
					IF(NOT(ROB_head = ROB_tail)) THEN
						ROB_tail <= ROB_tail + '1';
					END IF;
					ROB_issuer_stall <= '0';
				ELSE
					ROB_issuer_stall <= '1';
				END IF;
			END IF;
			IF(reorder_buffer(CONV_INTEGER(toCHeck_head)).ready = '1') THEN --push an entry to the regfile
				result_to_regfile_addr <= reorder_buffer(CONV_INTEGER(toCHeck_head)).resultReg;
				result_to_regfile_data <= reorder_buffer(CONV_INTEGER(toCHeck_head)).resultValue;
				ROB_regfile_transmit	  <= '1';
				reorder_buffer(CONV_INTEGER(toCHeck_head)).recNum <= "00000";
				reorder_buffer(CONV_INTEGER(toCHeck_head)).resultReg <= "00000";
				reorder_buffer(CONV_INTEGER(toCHeck_head)).resultValue <= X"00000000";
				reorder_buffer(CONV_INTEGER(toCHeck_head)).valid <= '0';
				reorder_buffer(CONV_INTEGER(toCHeck_head)).ready <= '0';
				reorder_buffer(CONV_INTEGER(toCHeck_head)).instruction <= X"00000000";
				ROB_head <= ROB_head + '1';
			ELSE
				ROB_regfile_transmit <= '0';
			END IF;
--			IF(CDB_valid = '1') THEN
--				IF(reorder_buffer(CONV_INTEGER(CDB_addr)).valid = '1') THEN --is there an entry
--					IF(reorder_buffer(CONV_INTEGER(CDB_addr)).resultReg = CDB_addr) THEN
--						reorder_buffer(CONV_INTEGER(CDB_addr)).resultValue <= CDB_data;
--						reorder_buffer(CONV_INTEGER(CDB_addr)).ready <= '1';
--					END IF;
--				END IF;
--			END IF;
			IF(CDB_valid = '1') THEN
				--tester3 <= '1';
				IF((reorder_buffer(0).resultReg = CDB_addr) AND (reorder_buffer(0).valid = '1')) THEN
					--tester3 <= '1';
					reorder_buffer(0).resultValue <= CDB_data;
					reorder_buffer(0).ready <= '1';
				END IF;
				IF((reorder_buffer(1).resultReg = CDB_addr) AND (reorder_buffer(1).valid = '1')) THEN
					reorder_buffer(1).resultValue <= CDB_data;
					reorder_buffer(1).ready <= '1';
				END IF;
				IF((reorder_buffer(2).resultReg = CDB_addr) AND (reorder_buffer(2).valid = '1')) THEN
					reorder_buffer(2).resultValue <= CDB_data;
					reorder_buffer(2).ready <= '1';
				END IF;
				IF((reorder_buffer(3).resultReg = CDB_addr) AND (reorder_buffer(3).valid = '1')) THEN
					reorder_buffer(3).resultValue <= CDB_data;
					reorder_buffer(3).ready <= '1';
				END IF;
				IF((reorder_buffer(4).resultReg = CDB_addr) AND (reorder_buffer(4).valid = '1')) THEN
					reorder_buffer(4).resultValue <= CDB_data;
					reorder_buffer(4).ready <= '1';
				END IF;
				IF((reorder_buffer(5).resultReg = CDB_addr) AND (reorder_buffer(5).valid = '1')) THEN
					reorder_buffer(5).resultValue <= CDB_data;
					reorder_buffer(5).ready <= '1';
				END IF;
				IF((reorder_buffer(6).resultReg = CDB_addr) AND (reorder_buffer(6).valid = '1')) THEN
					reorder_buffer(6).resultValue <= CDB_data;
					reorder_buffer(6).ready <= '1';
				END IF;
				IF((reorder_buffer(7).resultReg = CDB_addr) AND (reorder_buffer(7).valid = '1')) THEN
					reorder_buffer(7).resultValue <= CDB_data;
					reorder_buffer(7).ready <= '1';
				END IF;
				IF((reorder_buffer(8).resultReg = CDB_addr) AND (reorder_buffer(8).valid = '1')) THEN
					reorder_buffer(8).resultValue <= CDB_data;
					reorder_buffer(8).ready <= '1';
				END IF;
				IF((reorder_buffer(9).resultReg = CDB_addr) AND (reorder_buffer(9).valid = '1')) THEN
					reorder_buffer(9).resultValue <= CDB_data;
					reorder_buffer(9).ready <= '1';
				END IF;
				IF((reorder_buffer(10).resultReg = CDB_addr) AND (reorder_buffer(10).valid = '1')) THEN
					reorder_buffer(10).resultValue <= CDB_data;
					reorder_buffer(10).ready <= '1';
				END IF;
				IF((reorder_buffer(11).resultReg = CDB_addr) AND (reorder_buffer(11).valid = '1')) THEN
					reorder_buffer(11).resultValue <= CDB_data;
					reorder_buffer(11).ready <= '1';
				END IF;
				IF((reorder_buffer(12).resultReg = CDB_addr) AND (reorder_buffer(12).valid = '1')) THEN
					reorder_buffer(12).resultValue <= CDB_data;
					reorder_buffer(12).ready <= '1';
				END IF;
				IF((reorder_buffer(13).resultReg = CDB_addr) AND (reorder_buffer(13).valid = '1')) THEN
					reorder_buffer(13).resultValue <= CDB_data;
					reorder_buffer(13).ready <= '1';
				END IF;
				IF((reorder_buffer(14).resultReg = CDB_addr) AND (reorder_buffer(14).valid = '1')) THEN
					reorder_buffer(14).resultValue <= CDB_data;
					reorder_buffer(14).ready <= '1';
				END IF;
				IF((reorder_buffer(15).resultReg = CDB_addr) AND (reorder_buffer(15).valid = '1')) THEN
					reorder_buffer(15).resultValue <= CDB_data;
					reorder_buffer(15).ready <= '1';
				END IF;
				IF((reorder_buffer(16).resultReg = CDB_addr) AND (reorder_buffer(16).valid = '1')) THEN
					reorder_buffer(16).resultValue <= CDB_data;
					reorder_buffer(16).ready <= '1';
				END IF;
				IF((reorder_buffer(17).resultReg = CDB_addr) AND (reorder_buffer(17).valid = '1')) THEN
					reorder_buffer(17).resultValue <= CDB_data;
					reorder_buffer(17).ready <= '1';
				END IF;
				IF((reorder_buffer(18).resultReg = CDB_addr) AND (reorder_buffer(18).valid = '1')) THEN
					reorder_buffer(18).resultValue <= CDB_data;
					reorder_buffer(18).ready <= '1';
				END IF;
				IF((reorder_buffer(19).resultReg = CDB_addr) AND (reorder_buffer(19).valid = '1')) THEN
					reorder_buffer(19).resultValue <= CDB_data;
					reorder_buffer(19).ready <= '1';
				END IF;
				IF((reorder_buffer(20).resultReg = CDB_addr) AND (reorder_buffer(20).valid = '1')) THEN
					reorder_buffer(20).resultValue <= CDB_data;
					reorder_buffer(20).ready <= '1';
				END IF;
				IF((reorder_buffer(21).resultReg = CDB_addr) AND (reorder_buffer(21).valid = '1')) THEN
					reorder_buffer(21).resultValue <= CDB_data;
					reorder_buffer(21).ready <= '1';
				END IF;
				IF((reorder_buffer(22).resultReg = CDB_addr) AND (reorder_buffer(22).valid = '1')) THEN
					reorder_buffer(22).resultValue <= CDB_data;
					reorder_buffer(22).ready <= '1';
				END IF;
				IF((reorder_buffer(23).resultReg = CDB_addr) AND (reorder_buffer(23).valid = '1')) THEN
					reorder_buffer(23).resultValue <= CDB_data;
					reorder_buffer(23).ready <= '1';
				END IF;
				IF((reorder_buffer(24).resultReg = CDB_addr) AND (reorder_buffer(24).valid = '1')) THEN
					reorder_buffer(24).resultValue <= CDB_data;
					reorder_buffer(24).ready <= '1';
				END IF;
				IF((reorder_buffer(25).resultReg = CDB_addr) AND (reorder_buffer(25).valid = '1')) THEN
					reorder_buffer(25).resultValue <= CDB_data;
					reorder_buffer(25).ready <= '1';
				END IF;
				IF((reorder_buffer(26).resultReg = CDB_addr) AND (reorder_buffer(26).valid = '1')) THEN
					reorder_buffer(26).resultValue <= CDB_data;
					reorder_buffer(26).ready <= '1';
				END IF;
				IF((reorder_buffer(27).resultReg = CDB_addr) AND (reorder_buffer(27).valid = '1')) THEN
					reorder_buffer(27).resultValue <= CDB_data;
					reorder_buffer(27).ready <= '1';
				END IF;
				IF((reorder_buffer(28).resultReg = CDB_addr) AND (reorder_buffer(28).valid = '1')) THEN
					reorder_buffer(28).resultValue <= CDB_data;
					reorder_buffer(28).ready <= '1';
				END IF;
				IF((reorder_buffer(29).resultReg = CDB_addr) AND (reorder_buffer(29).valid = '1')) THEN
					reorder_buffer(29).resultValue <= CDB_data;
					reorder_buffer(29).ready <= '1';
				END IF;
				IF((reorder_buffer(30).resultReg = CDB_addr) AND (reorder_buffer(30).valid = '1')) THEN
					reorder_buffer(30).resultValue <= CDB_data;
					reorder_buffer(30).ready <= '1';
				END IF;
				IF((reorder_buffer(31).resultReg = CDB_addr) AND (reorder_buffer(31).valid = '1')) THEN
					reorder_buffer(31).resultValue <= CDB_data;
					reorder_buffer(31).ready <= '1';
				END IF;
			END IF;
			
			-----
			IF(NOT(regS_in_ROB_sig = "00")) THEN -- set the signals
				regS_in_ROB <= regS_in_ROB_sig;
				regS_regfile_value <= regS_regfile_value_sig;
			ELSE
				RegS_in_ROB <= "00";
				regS_regfile_value <= X"00000000";
			END IF;
			
			IF(NOT(regT_in_ROB_sig = "00")) THEN -- set the signals
				regT_in_ROB <= regT_in_ROB_sig;
				regT_regfile_value <= regT_regfile_value_sig;
			ELSE
				regT_in_ROB <= "00";
				regT_regfile_value <= X"00000000";
			END IF;
		
		END IF;
	END PROCESS;
	
	--PROCESS()
	--BEGIN
	
	--END PROCESS;
	
	tester  <= CDB_addr;
	tester2 <= reorder_buffer(0).resultReg;
	tester3 <= reorder_buffer(0).ready;
	tester4 <= reorder_buffer(0).valid;
	tester5 <= reorder_buffer(30).valid;
	tester6 <= reorder_buffer(31).valid;
	tester7 <= reorder_buffer(CONV_INTEGER(toCHeck_head)).ready;
	tester8 <= reorder_buffer(30).ready;
	tester9 <= reorder_buffer(31).ready;
	tester10 <= reorder_buffer(CONV_INTEGER(ROB_head)).ready;
	test_push <= reorder_buffer(CONV_INTEGER(toCHeck_head)).resultReg;
	test_101 <= toCHeck_head;
	tester11 <= reorder_buffer(CONV_INTEGER(toCHeck_head)).ready;
	tester12 <= reorder_buffer(CONV_INTEGER(toCHeck_head)).valid;
	
	
	
	PROCESS(incoming_regS, reorder_buffer, ROB_head)
	BEGIN
	





		-- 0






		IF (ROB_head = "00000") THEN -- 0
			IF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
                 IF(reorder_buffer(0).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(0).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 1






		IF (ROB_head = "00001") THEN -- 1
			IF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
                 IF(reorder_buffer(1).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(1).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 2






		IF (ROB_head = "00010") THEN -- 2
			IF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
                 IF(reorder_buffer(2).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(2).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 3






		IF (ROB_head = "00011") THEN -- 3
			IF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
                 IF(reorder_buffer(3).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(3).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 4






		IF (ROB_head = "00100") THEN -- 4
			IF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
                 IF(reorder_buffer(4).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(4).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 5






		IF (ROB_head = "00101") THEN -- 5
			IF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
                 IF(reorder_buffer(5).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(5).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 6






		IF (ROB_head = "00110") THEN -- 6
			IF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
                 IF(reorder_buffer(6).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(6).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 7






		IF (ROB_head = "00111") THEN -- 7
			IF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
                 IF(reorder_buffer(7).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(7).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 8






		IF (ROB_head = "01000") THEN -- 8
			IF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
                 IF(reorder_buffer(8).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(8).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 9






		IF (ROB_head = "01001") THEN -- 9
			IF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
                 IF(reorder_buffer(9).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(9).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 10






		IF (ROB_head = "01010") THEN -- 10
			IF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
                 IF(reorder_buffer(10).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(10).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 11






		IF (ROB_head = "01011") THEN -- 11
			IF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
                 IF(reorder_buffer(11).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(11).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 12






		IF (ROB_head = "01100") THEN -- 12
			IF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
                 IF(reorder_buffer(12).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(12).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 13






		IF (ROB_head = "01101") THEN -- 13
			IF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
                 IF(reorder_buffer(13).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(13).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 14






		IF (ROB_head = "01110") THEN -- 14
			IF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
                 IF(reorder_buffer(14).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(14).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 15






		IF (ROB_head = "01111") THEN -- 15
			IF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
                 IF(reorder_buffer(15).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(15).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 16






		IF (ROB_head = "10000") THEN -- 16
			IF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
                 IF(reorder_buffer(16).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(16).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 17






		IF (ROB_head = "10001") THEN -- 17
			IF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
                 IF(reorder_buffer(17).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(17).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 18






		IF (ROB_head = "10010") THEN -- 18
			IF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
                 IF(reorder_buffer(18).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(18).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 19






		IF (ROB_head = "10011") THEN -- 19
			IF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
                 IF(reorder_buffer(19).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(19).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 20






		IF (ROB_head = "10100") THEN -- 20
			IF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
                 IF(reorder_buffer(20).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(20).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 21






		IF (ROB_head = "10101") THEN -- 21
			IF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
                 IF(reorder_buffer(21).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(21).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 22






		IF (ROB_head = "10110") THEN -- 22
			IF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
                 IF(reorder_buffer(22).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(22).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 23






		IF (ROB_head = "10111") THEN -- 23
			IF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
                 IF(reorder_buffer(23).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(23).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 24






		IF (ROB_head = "11000") THEN -- 24
			IF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
                 IF(reorder_buffer(24).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(24).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 25






		IF (ROB_head = "11001") THEN -- 25
			IF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
                 IF(reorder_buffer(25).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(25).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 26






		IF (ROB_head = "11010") THEN -- 26
			IF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
                 IF(reorder_buffer(26).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(26).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 27






		IF (ROB_head = "11011") THEN -- 27
			IF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
                 IF(reorder_buffer(27).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(27).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 28






		IF (ROB_head = "11100") THEN -- 28
			IF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
                 IF(reorder_buffer(28).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(28).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 29






		IF (ROB_head = "11101") THEN -- 29
			IF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
                 IF(reorder_buffer(29).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(29).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 30






		IF (ROB_head = "11110") THEN -- 30
			IF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
                 IF(reorder_buffer(30).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(30).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(31).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 31






		IF (ROB_head = "11111") THEN -- 31
			IF((reorder_buffer(31).resultReg = incoming_regS) AND (reorder_buffer(31).valid = '1')) THEN
                 IF(reorder_buffer(31).ready = '1') THEN
                     regS_regfile_value_sig <= reorder_buffer(31).resultValue;
                     regS_in_ROB_sig <= "11";
                 ELSE
                     regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
                     regS_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regS) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(0).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regS) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(1).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regS) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(2).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regS) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(3).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regS) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(4).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regS) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(5).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regS) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(6).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regS) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(7).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regS) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(8).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regS) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(9).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regS) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(10).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regS) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(11).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regS) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(12).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regS) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(13).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regS) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(14).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regS) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(15).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regS) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(16).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regS) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(17).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regS) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(18).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regS) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(19).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regS) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(20).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regS) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(21).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regS) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(22).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regS) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(23).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regS) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(24).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regS) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(25).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regS) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(26).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regS) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(27).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regS) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(28).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regS) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(29).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regS) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regS_regfile_value_sig <= reorder_buffer(30).resultValue;
					regS_in_ROB_sig <= "11";
				ELSE
					regS_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regS_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regS_in_ROB_sig <= "00";
             regS_regfile_value_sig <= X"00000000";
		END IF;






		-- 32







	END PROCESS;
	
	
	PROCESS(incoming_regT, reorder_buffer, ROB_head)
	BEGIN
	





		-- 0






		IF (ROB_head = "00000") THEN -- 0
			IF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
                 IF(reorder_buffer(0).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(0).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 1






		IF (ROB_head = "00001") THEN -- 1
			IF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
                 IF(reorder_buffer(1).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(1).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 2






		IF (ROB_head = "00010") THEN -- 2
			IF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
                 IF(reorder_buffer(2).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(2).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 3






		IF (ROB_head = "00011") THEN -- 3
			IF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
                 IF(reorder_buffer(3).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(3).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 4






		IF (ROB_head = "00100") THEN -- 4
			IF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
                 IF(reorder_buffer(4).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(4).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 5






		IF (ROB_head = "00101") THEN -- 5
			IF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
                 IF(reorder_buffer(5).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(5).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 6






		IF (ROB_head = "00110") THEN -- 6
			IF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
                 IF(reorder_buffer(6).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(6).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 7






		IF (ROB_head = "00111") THEN -- 7
			IF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
                 IF(reorder_buffer(7).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(7).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 8






		IF (ROB_head = "01000") THEN -- 8
			IF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
                 IF(reorder_buffer(8).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(8).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 9






		IF (ROB_head = "01001") THEN -- 9
			IF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
                 IF(reorder_buffer(9).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(9).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 10






		IF (ROB_head = "01010") THEN -- 10
			IF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
                 IF(reorder_buffer(10).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(10).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 11






		IF (ROB_head = "01011") THEN -- 11
			IF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
                 IF(reorder_buffer(11).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(11).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 12






		IF (ROB_head = "01100") THEN -- 12
			IF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
                 IF(reorder_buffer(12).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(12).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 13






		IF (ROB_head = "01101") THEN -- 13
			IF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
                 IF(reorder_buffer(13).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(13).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 14






		IF (ROB_head = "01110") THEN -- 14
			IF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
                 IF(reorder_buffer(14).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(14).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 15






		IF (ROB_head = "01111") THEN -- 15
			IF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
                 IF(reorder_buffer(15).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(15).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 16






		IF (ROB_head = "10000") THEN -- 16
			IF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
                 IF(reorder_buffer(16).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(16).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 17






		IF (ROB_head = "10001") THEN -- 17
			IF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
                 IF(reorder_buffer(17).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(17).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 18






		IF (ROB_head = "10010") THEN -- 18
			IF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
                 IF(reorder_buffer(18).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(18).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 19






		IF (ROB_head = "10011") THEN -- 19
			IF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
                 IF(reorder_buffer(19).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(19).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 20






		IF (ROB_head = "10100") THEN -- 20
			IF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
                 IF(reorder_buffer(20).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(20).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 21






		IF (ROB_head = "10101") THEN -- 21
			IF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
                 IF(reorder_buffer(21).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(21).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 22






		IF (ROB_head = "10110") THEN -- 22
			IF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
                 IF(reorder_buffer(22).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(22).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 23






		IF (ROB_head = "10111") THEN -- 23
			IF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
                 IF(reorder_buffer(23).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(23).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 24






		IF (ROB_head = "11000") THEN -- 24
			IF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
                 IF(reorder_buffer(24).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(24).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 25






		IF (ROB_head = "11001") THEN -- 25
			IF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
                 IF(reorder_buffer(25).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(25).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 26






		IF (ROB_head = "11010") THEN -- 26
			IF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
                 IF(reorder_buffer(26).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(26).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 27






		IF (ROB_head = "11011") THEN -- 27
			IF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
                 IF(reorder_buffer(27).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(27).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 28






		IF (ROB_head = "11100") THEN -- 28
			IF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
                 IF(reorder_buffer(28).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(28).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 29






		IF (ROB_head = "11101") THEN -- 29
			IF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
                 IF(reorder_buffer(29).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(29).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 30






		IF (ROB_head = "11110") THEN -- 30
			IF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
                 IF(reorder_buffer(30).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(30).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
				IF(reorder_buffer(31).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(31).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 31






		IF (ROB_head = "11111") THEN -- 31
			IF((reorder_buffer(31).resultReg = incoming_regT) AND (reorder_buffer(31).valid = '1')) THEN
                 IF(reorder_buffer(31).ready = '1') THEN
                     regT_regfile_value_sig <= reorder_buffer(31).resultValue;
                     regT_in_ROB_sig <= "11";
                 ELSE
                     regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(31).recNum;
                     regT_in_ROB_sig <= "10";
                 END IF;
			ELSIF((reorder_buffer(0).resultReg = incoming_regT) AND (reorder_buffer(0).valid = '1')) THEN
				IF(reorder_buffer(0).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(0).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(0).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(1).resultReg = incoming_regT) AND (reorder_buffer(1).valid = '1')) THEN
				IF(reorder_buffer(1).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(1).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(1).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(2).resultReg = incoming_regT) AND (reorder_buffer(2).valid = '1')) THEN
				IF(reorder_buffer(2).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(2).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(2).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(3).resultReg = incoming_regT) AND (reorder_buffer(3).valid = '1')) THEN
				IF(reorder_buffer(3).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(3).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(3).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(4).resultReg = incoming_regT) AND (reorder_buffer(4).valid = '1')) THEN
				IF(reorder_buffer(4).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(4).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(4).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(5).resultReg = incoming_regT) AND (reorder_buffer(5).valid = '1')) THEN
				IF(reorder_buffer(5).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(5).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(5).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(6).resultReg = incoming_regT) AND (reorder_buffer(6).valid = '1')) THEN
				IF(reorder_buffer(6).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(6).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(6).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(7).resultReg = incoming_regT) AND (reorder_buffer(7).valid = '1')) THEN
				IF(reorder_buffer(7).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(7).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(7).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(8).resultReg = incoming_regT) AND (reorder_buffer(8).valid = '1')) THEN
				IF(reorder_buffer(8).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(8).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(8).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(9).resultReg = incoming_regT) AND (reorder_buffer(9).valid = '1')) THEN
				IF(reorder_buffer(9).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(9).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(9).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(10).resultReg = incoming_regT) AND (reorder_buffer(10).valid = '1')) THEN
				IF(reorder_buffer(10).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(10).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(10).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(11).resultReg = incoming_regT) AND (reorder_buffer(11).valid = '1')) THEN
				IF(reorder_buffer(11).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(11).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(11).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(12).resultReg = incoming_regT) AND (reorder_buffer(12).valid = '1')) THEN
				IF(reorder_buffer(12).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(12).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(12).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(13).resultReg = incoming_regT) AND (reorder_buffer(13).valid = '1')) THEN
				IF(reorder_buffer(13).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(13).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(13).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(14).resultReg = incoming_regT) AND (reorder_buffer(14).valid = '1')) THEN
				IF(reorder_buffer(14).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(14).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(14).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(15).resultReg = incoming_regT) AND (reorder_buffer(15).valid = '1')) THEN
				IF(reorder_buffer(15).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(15).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(15).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(16).resultReg = incoming_regT) AND (reorder_buffer(16).valid = '1')) THEN
				IF(reorder_buffer(16).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(16).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(16).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(17).resultReg = incoming_regT) AND (reorder_buffer(17).valid = '1')) THEN
				IF(reorder_buffer(17).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(17).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(17).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(18).resultReg = incoming_regT) AND (reorder_buffer(18).valid = '1')) THEN
				IF(reorder_buffer(18).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(18).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(18).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(19).resultReg = incoming_regT) AND (reorder_buffer(19).valid = '1')) THEN
				IF(reorder_buffer(19).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(19).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(19).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(20).resultReg = incoming_regT) AND (reorder_buffer(20).valid = '1')) THEN
				IF(reorder_buffer(20).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(20).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(20).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(21).resultReg = incoming_regT) AND (reorder_buffer(21).valid = '1')) THEN
				IF(reorder_buffer(21).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(21).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(21).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(22).resultReg = incoming_regT) AND (reorder_buffer(22).valid = '1')) THEN
				IF(reorder_buffer(22).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(22).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(22).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(23).resultReg = incoming_regT) AND (reorder_buffer(23).valid = '1')) THEN
				IF(reorder_buffer(23).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(23).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(23).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(24).resultReg = incoming_regT) AND (reorder_buffer(24).valid = '1')) THEN
				IF(reorder_buffer(24).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(24).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(24).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(25).resultReg = incoming_regT) AND (reorder_buffer(25).valid = '1')) THEN
				IF(reorder_buffer(25).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(25).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(25).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(26).resultReg = incoming_regT) AND (reorder_buffer(26).valid = '1')) THEN
				IF(reorder_buffer(26).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(26).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(26).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(27).resultReg = incoming_regT) AND (reorder_buffer(27).valid = '1')) THEN
				IF(reorder_buffer(27).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(27).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(27).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(28).resultReg = incoming_regT) AND (reorder_buffer(28).valid = '1')) THEN
				IF(reorder_buffer(28).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(28).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(28).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(29).resultReg = incoming_regT) AND (reorder_buffer(29).valid = '1')) THEN
				IF(reorder_buffer(29).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(29).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(29).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			ELSIF((reorder_buffer(30).resultReg = incoming_regT) AND (reorder_buffer(30).valid = '1')) THEN
				IF(reorder_buffer(30).ready = '1') THEN
					regT_regfile_value_sig <= reorder_buffer(30).resultValue;
					regT_in_ROB_sig <= "11";
				ELSE
					regT_regfile_value_sig <= X"000000" & "000" & reorder_buffer(30).recNum;
					regT_in_ROB_sig <= "10";
				END IF;
			END IF;
		ELSE
			regT_in_ROB_sig <= "00";
         regT_regfile_value_sig <= X"00000000";
		END IF;






		-- 32







	END PROCESS;
	
	
	END behavior;
