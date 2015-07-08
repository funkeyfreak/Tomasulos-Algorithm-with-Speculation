LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY memory_unit IS
	PORT(		
				clock, reset			: IN 	STD_LOGIC; 							 -- clock on which to add items
				instruction_op			: IN	STD_LOGIC_VECTOR( 5 DOWNTO 0); -- the instruction which is incoming 
				address_dest			: IN	STD_LOGIC_VECTOR(31 DOWNTO 0); -- mem dest
				data_in					: IN	STD_LOGIC_VECTOR(31 DOWNTO 0); -- mem data in (store)
				-- data_from_mem			: IN  STD_LOGIC_VECTOR(31 DOWNTO 0); -- data from memory after load fetch 
				data_out					: OUT STD_LOGIC_VECTOR(31 DOWNTO 0); -- mem data out (load)
				address_out_to_mem	: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0); -- send data to mem for fetch
				busy						: OUT STD_LOGIC							 -- busy
		 );
END memory_unit;

ARCHITECTURE behavior OF memory_unit IS
-- signals gets inputs to save inputs
	SIGNAL busy_sig				: STD_LOGIC:= '0';
	SIGNAL address_dest_sig 	: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL data_in_sig			: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL counter 				: STD_LOGIC_VECTOR(3 DOWNTO 0):="1011"; --use 11 clock cycles for latency
	
	BEGIN
		PROCESS(clock, reset, address_dest, data_in, address_dest_sig, data_in_sig, busy_sig)
		BEGIN
			IF (RISING_EDGE(clock)) THEN 
				--update signals to new data if not busy
				IF (busy_sig = '0') THEN
					busy_sig <= '1';
					busy <= '1';
					address_dest_sig <= address_dest;
					data_in_sig <= data_in;
				END IF;
				
				--IF LOAD
				--IF(address_dest_sig(31 DOWNTO 26) = "100011") THEN
				IF(instruction_op = "100011") THEN 
					--if beginning of latency, ask mem for data
					IF(counter = "1011") THEN
						address_out_to_mem <= address_dest_sig;
						
					--ELSIF end of latency, set received data from memory to data_out
					ELSIF(counter = "0000") THEN
						--refetch data_in that came from mem request
						data_in_sig <= data_in;
						data_out <= data_in_sig;
						
						--reset counter/busy
						counter <= "1100"; -- 12 instead of 11 because counter will dec at the end 
						busy_sig <= '0';
						busy <= busy_sig;
					END IF;
				
				--ELSE IF STORE
				--ELSIF(address_dest_sig(31 DOWNTO 26) = "101011") THEN
				ELSIF(instruction_op = "101011") THEN
					IF(counter = "0000") THEN
						data_out <= data_in_sig;
						
						--reset counter/busy
						counter <= "1100"; -- 12 instead of 11 because counter will dec at the end 
						busy_sig <= '0';
						busy <= busy_sig;
					END IF;
				END IF;
				
				--if busy is low, we reset all outputs to zero
				IF (busy_sig = '0') THEN
					data_out <= X"00000000";
					address_out_to_mem <= X"00000000";
				END IF;
				
				IF(busy_sig = '1') THEN
					counter <= counter - '1';
				END IF;
			END IF;
		END PROCESS;

END behavior;