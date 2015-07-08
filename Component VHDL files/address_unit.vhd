LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY address_unit IS
	PORT(		
				clock, reset			: IN 	STD_LOGIC; 								--clock on which to add items
				Instruction_IN			: IN	STD_LOGIC_VECTOR(31 DOWNTO 0); 	-- inst = register + offset
				CDB_data 				: IN 	STD_LOGIC_VECTOR(31 DOWNTO 0); 	-- cdb data
				CDB_addr 				: IN 	STD_LOGIC_VECTOR(4 DOWNTO 0); 	-- cdb addr (rob id)
				CDB_valid 				: IN 	STD_LOGIC; 								-- cdb valid
				regS_value_in			: IN	STD_LOGIC_VECTOR(31 DOWNTO 0); 	-- base addr
				regS_source_in			: IN	STD_LOGIC; 								-- '0' = regfile, '1' = ROB / FROM ISSUER
				regfile_data			: IN 	STD_LOGIC_VECTOR(31 DOWNTO 0);	-- if from reg, will hold data to add to imm field
				regT_value_in			: IN	STD_LOGIC_VECTOR(31 DOWNTO 0); 	-- rob entry for lw to write to
				fu_dest					: IN	STD_LOGIC_VECTOR( 1 DOWNTO 0); 	-- is for address unit?
				address_OUT				: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0); 	-- calc'd addr meant for lbuff
				regT_value_out			: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0); 	-- rob entry id for lw to write to
				busy						: OUT STD_LOGIC
		 );
END address_unit;

ARCHITECTURE behavior OF address_unit IS
	-- signals gets inputs to save inputs
	SIGNAL busy_sig				: STD_LOGIC:= '0';
	SIGNAL Instruction_sig 		: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL CDB_data_sig 			: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL CDB_addr_sig			: STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL CDB_valid_sig			: STD_LOGIC; 					
	SIGNAL regS_value_sig		: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL regS_source_sig		: STD_LOGIC;
	SIGNAL regT_value_sig		: STD_LOGIC_VECTOR(31 DOWNTO 0);

	BEGIN
		PROCESS(reset, clock, Instruction_IN, CDB_data, CDB_addr, CDB_valid, regS_value_in, regS_source_in, regfile_data, regT_value_in, fu_dest, regT_value_sig)
		BEGIN
			IF (RISING_EDGE(clock)) THEN
			
				IF (fu_dest = "10") THEN -- meant for testing

					--update signals to new data if not busy
					IF (busy_sig = '0') THEN
						busy_sig <= '1';
						busy <= '1';
						Instruction_sig <= Instruction_IN;
						CDB_data_sig <= CDB_data;
						CDB_addr_sig <= CDB_addr;
						CDB_valid_sig <= CDB_valid;
						regS_value_sig <= regS_value_in;
						regS_source_sig <= regS_source_in;
						regT_value_sig <= regT_value_in;
					END IF;
	
					--refetch cdb fields
					CDB_data_sig <= CDB_data;
					CDB_addr_sig <= CDB_addr;
					CDB_valid_sig <= CDB_valid;

					--If from ROB/Issuer
					IF(regS_source_sig = '1') THEN
						IF(Instruction_sig(31 DOWNTO 26) = "100011") THEN --LOAD WORD 
							IF(regS_source_sig = '1' AND CDB_addr_sig = regS_value_sig(4 DOWNTO 0) AND CDB_valid_sig = '1') THEN --if from rob, on cdb and valid
								--calculate, and set busy to 0
								address_OUT <= (CDB_data_sig + Instruction_sig(15 DOWNTO 0)); --calc (cdb_data + imm)
								busy <= '0';
								busy_sig <= '0';
								regT_value_out <= regT_value_sig;

							END IF;
						END IF;
						
					--If from regfile
					ELSIF(regS_source_sig = '0') THEN
						address_OUT <= (regfile_data + Instruction_sig(15 DOWNTO 0)); --calc (cdb_data + imm)
						busy <= '0';
						busy_sig <= '0';
						regT_value_out <= regT_value_sig;
					END IF;
				END IF;
			END IF;
		END PROCESS;

END behavior;


--TODO
--Zero out out_puts after insertion
--process if statements if needed