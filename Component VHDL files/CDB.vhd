LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY CDB IS
	PORT(		
				clock, reset			: IN 	STD_LOGIC; --clock on which to add items
				incoming_data_ALU		: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				incoming_address_ALU	: IN	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				incoming_data_FPM		: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				incoming_address_FPM	: IN	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				incoming_data_FPA		: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				incoming_address_FPA	: IN	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				incoming_data_MEM		: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
				incoming_address_MEM	: IN	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				ALU, MEM, FPM, FPA	: IN  STD_LOGIC;
				--type_owner				: IN 	STD_LOGIC_VECTOR( 1 DOWNTO 0); -- 00 - ALU -- 01 - MEM -- 10 - FPA -- 11 FPM
				tester_sig				: OUT	STD_LOGIC;
				CDB_x2					: OUT STD_LOGIC;
				CDB_busy					: OUT	STD_LOGIC;
				CDB_valid				: OUT	STD_LOGIC;
				CDB_addr					: OUT	STD_LOGIC_VECTOR( 4 DOWNTO 0);
				CDB_data					: OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
				tester					: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0);
				tester2					: OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
				
		 );
END CDB;

ARCHITECTURE behavior OF CDB IS
 TYPE object IS
	RECORD
		isEmpty							: STD_LOGIC;
		data								: STD_LOGIC_VECTOR(31 DOWNTO 0);
		address							: STD_LOGIC_VECTOR( 4 DOWNTO 0);
		owner								: STD_LOGIC_VECTOR( 1 DOWNTO 0); -- 00 - ALU -- 01 - MEM -- 10 - FPA -- 11 FPM
	END RECORD;
	
	TYPE CDB_full_type IS ARRAY(0 TO 3) OF object;
	
	SIGNAL CDB_obj					: CDB_full_type; -- initilize the CDB object
	SIGNAL CDB_empty				: STD_LOGIC := '1';
	--SIGNAL CDB_busy_sig			: STD_LOGIC := '0';

BEGIN

	--CDB_busy <= CDB_busy_sig;
	PROCESS(clock, reset, CDB_obj)
	BEGIN
		IF(reset = '0') THEN
		ELSIF(RISING_EDGE(clock)) THEN
			CDB_valid 	<= '0';
			CDB_addr		<= "00000";
			CDB_data		<= X"00000000";
			IF(CDB_empty = '1') THEN --it is empty, insert items
				IF(ALU = '1') THEN
					IF(CDB_obj(0).isEmpty = '0') THEN
						CDB_obj(0).isEmpty <= '1';
						CDB_obj(0).data	 <= incoming_data_ALU;
						CDB_obj(0).address <= incoming_address_ALU;
						CDB_obj(0).owner	 <= "00";
					END IF;
				END IF;
				IF(MEM = '1') THEN
					IF(CDB_obj(1).isEmpty = '0') THEN
						CDB_obj(1).isEmpty <= '1';
						CDB_obj(1).data	 <= incoming_data_MEM;
						CDB_obj(1).address <= incoming_address_MEM;
						CDB_obj(1).owner	 <= "01";
					END IF;
				END IF;
				IF(FPA = '1') THEN
					IF(CDB_obj(2).isEmpty = '0') THEN
						CDB_obj(2).isEmpty <= '1';
						CDB_obj(2).data	 <= incoming_data_FPA;
						CDB_obj(2).address <= incoming_address_FPA;
						CDB_obj(2).owner	 <= "10";
					END IF;
				END IF;
				IF(FPM = '1') THEN
					IF(CDB_obj(3).isEmpty = '0') THEN
						CDB_obj(3).isEmpty <= '1';
						CDB_obj(3).data	 <= incoming_data_FPM;
						CDB_obj(3).address <= incoming_address_FPM;
						CDB_obj(3).owner	 <= "11";
					END IF;
				END IF;
			ELSE -- empty out the CDB
				IF(CDB_obj(0).isEmpty = '1') THEN
					CDB_valid <= '1';
					CDB_data <= CDB_obj(0).data;
					CDB_addr <= CDB_obj(0).address;
					CDB_obj(0).isEmpty <= '0';
				ELSIF(CDB_obj(1).isEmpty = '1') THEN
					CDB_valid <= '1';
					CDB_data <= CDB_obj(1).data;
					CDB_addr <= CDB_obj(1).address;
					CDB_obj(1).isEmpty <= '0';
				ELSIF(CDB_obj(2).isEmpty = '1') THEN
					CDB_valid <= '1';
					CDB_data <= CDB_obj(2).data;
					CDB_addr <= CDB_obj(2).address;
					CDB_obj(2).isEmpty <= '0';
				ELSIF(CDB_obj(3).isEmpty = '1') THEN
					CDB_valid <= '1';
					CDB_data <= CDB_obj(3).data;
					CDB_addr <= CDB_obj(3).address;
					CDB_obj(3).isEmpty <= '0';
				END IF;			
			END IF;
		END IF;
		
		
		IF(CDB_obj(0).isEmpty = '1') THEN
			CDB_empty <= '0';
			CDB_busy <= '1';
		ELSIF(CDB_obj(1).isEmpty = '1') THEN
			CDB_empty <= '0';
			CDB_busy <= '1';
		ELSIF(CDB_obj(2).isEmpty = '1') THEN
			CDB_empty <= '0';
			CDB_busy <= '1';
		ELSIF(CDB_obj(3).isEmpty = '1') THEN
			CDB_empty <= '0';
			CDB_busy <= '1';
		ELSE
			CDB_empty <= '1';
			CDB_busy <= '0';
		END IF;
	

	END PROCESS;
	
END behavior;



