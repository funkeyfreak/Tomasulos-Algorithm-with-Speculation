LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

--this is really the issuer, not the instruction que

ENTITY issuer IS
	PORT(	clock, reset			: IN	STD_LOGIC; --we know this
			fu_mem_unit_full		: IN	STD_LOGIC; --do we need to stall (reservation station is full)
			fu_fp_add_unit_full		: IN	STD_LOGIC; --do we need to stall (reservation station is full)
			fu_fp_mult_unit_full	: IN	STD_LOGIC; --do we need to stall (reservation station is full)
			fu_int_unit_full		: IN	STD_LOGIC; --do we need to stall (reservation station is full)
			reorder_buffer_full		: IN	STD_LOGIC;
			br_predict_fail			: IN	STD_LOGIC; --the flag sent from the branch predictor (if it fails to send)
			br_address				: IN	STD_LOGIC_VECTOR(31 DOWNTO 0); --where in the instruction queue are we - if br_predict_fail is 1 we set pc to this+
			incoming_instruction	: IN 	STD_LOGIC_VECTOR(31 DOWNTO 0); -- the instruction from pc
			regS_in_ROB				: IN	STD_LOGIC_VECTOR( 1 DOWNTO 0);
			regT_in_ROB				: IN	STD_LOGIC_VECTOR( 1 DOWNTO 0);
			regS_ROB_entry			: IN	STD_LOGIC_VECTOR( 4 DOWNTO 0);
			regT_ROB_entry			: IN	STD_LOGIC_VECTOR( 4 DOWNTO 0);
			regS_regfile_value		: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
			regT_regfile_value		: IN	STD_LOGIC_VECTOR(31 DOWNTO 0);
			stall_n_out				: OUT	STD_LOGIC; -- pass the stall information on to the ROB an locations (like the CDB)
			pc_out					: OUT 	STD_LOGIC_VECTOR(31 DOWNTO 0); --if branch fails, we set pc to this plus br_predict_fail
			instruction_out			: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0); --which instruction?
			predict_br_taken_out	: OUT	STD_LOGIC;
			predict_br_addr_out		: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0);
			regS_value_out			: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0);
			regT_value_out			: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0);
			regS_source_out			: OUT	STD_LOGIC; --'0' = regfile, '1' = ROB
			regT_source_out			: OUT	STD_LOGIC; --'0' = regfile, '1' = ROB
			regS_out, regT_out		: OUT	STD_LOGIC_VECTOR( 4 DOWNTO 0); --This will go to the ROB and the ROB will send a signal into the reg_in values, indicating that the data is in the ROB
			fu_dest					: OUT	STD_LOGIC_VECTOR( 1 DOWNTO 0) --will define type later --the number of functional units we will use(multipliers, adders, dividers, memory
		);
END issuer;

ARCHITECTURE behavior OF issuer IS

	COMPONENT imem IS
		PORT(
			address		: IN	STD_LOGIC_VECTOR( 9 DOWNTO 0);
			clock		: IN	STD_LOGIC := '1';
			clken		: IN 	STD_LOGIC := '1';
			q			: OUT	STD_LOGIC_VECTOR(31 DOWNTO 0)
		);
	END COMPONENT;

	SIGNAL instruction 		: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL pc, next_pc 		: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL pc_plus_four 	: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL jump, branch		: STD_LOGIC;
	SIGNAL opcode			: STD_LOGIC_VECTOR( 5 DOWNTO 0);
	SIGNAL func				: STD_LOGIC_VECTOR( 4 DOWNTO 0);
	SIGNAL fu_stall_n		: STD_LOGIC;
	SIGNAL stall_n			: STD_LOGIC;
	SIGNAL predict_br_addr	: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL predict_br_taken : STD_LOGIC;
	SIGNAL fu_fp_adder_unit, fu_fp_mult_unit, fu_integer_unit, fu_memory_unit : STD_LOGIC_VECTOR( 1 DOWNTO 0);
	--SIGNAL next : STD_LOGIC_VECTOR(31 DOWNTO 0);

BEGIN

	instruction <= incoming_instruction;

	fu_fp_adder_unit 	<= "00";
	fu_fp_mult_unit	<= "01";
	fu_memory_unit		<= "10";
	fu_integer_unit	<= "11";
	
	pc_out 			<= pc;
	instruction_out <= instruction;
	predict_br_taken_out <= predict_br_taken;
	predict_br_addr_out <= predict_br_addr;
	stall_n_out <= stall_n;
	
	pc_plus_four 	<= pc + 4;
	opcode 			<= instruction(31 DOWNTO 26);
	func			<= instruction( 4 DOWNTO  0);
	
	stall_n			<= fu_stall_n AND NOT(reorder_buffer_full); --or with active low logic
	
	--instr_mem: lmem; --set the memory to proper values
		--PORT MAP(
		--	address => next_pc(11 DOWNTO 2); --drop off bottom two bits for address
		--	clken <= NOT(stall_n);
		--	clock => clock,
		--	q => instruction
		--);
			
	PROCESS (reset, clock) --process for changing the pc and set on rising edge of clock
	BEGIN
		IF (reset = '0') THEN
			pc <= X"00000000"; --reset pc
		ELSIF (RISING_EDGE(clock)) THEN
			IF stall_n = '0' THEN
				pc <= next_pc;
			END IF;
		END IF;
	END PROCESS;
	
	PROCESS(regS_in_ROB, regS_ROB_entry, regS_regfile_value) --get values and send out to reservation station or ROB
	BEGIN
		
		IF(regS_in_ROB = "10") THEN
			regS_source_out <= '1';
			regS_value_out <=  X"000000"&"000"&regS_ROB_entry; -- remember, you cannot modify values of the OUT type on the left hand side of the equations
		--ELSIF(regS_in_ROB = "11") THEN
		ELSIF (regS_in_ROB = "11") THEN
			regS_source_out <= '0';
			regS_value_out <= regS_regfile_value;
		ELSE
			regS_source_out <= '0';
			regS_value_out <= X"00000000";
		END IF;
				
	END PROCESS;

		PROCESS(regT_in_ROB, regS_ROB_entry, regS_regfile_value) --get values and send out to reservation station or ROB
	BEGIN
		
		IF(regT_in_ROB = "10") THEN
			regT_source_out <= '1';
			regT_value_out <=  X"000000"&"000"&regT_ROB_entry; -- remember, you cannot modify values of the OUT type on the left hand side of the equations
		ELSIF (regT_in_ROB = "11") THEN
			regT_source_out <= '0';
			regT_value_out <= regT_regfile_value;
		ELSE
			regT_source_out <= '0';
			regT_value_out <= X"00000000";
		END IF;
				
	END PROCESS;
	
	PROCESS (br_predict_fail, jump, branch, br_address, predict_br_addr, pc_plus_four, instruction(25 DOWNTO 0)) --update pc process -- stuff on left and right-hand side has to be included
	BEGIN
		IF (br_predict_fail = '1') THEN
			next_pc <= br_address;
		ELSIF (jump = '1') THEN
			next_pc <= pc_plus_four(31 DOWNTO 28) & instruction(25 DOWNTO 0) & "00"; --See mips green sheet for explanation (look at jump instruction)
		ELSIF ((branch = '1') AND (predict_br_taken = '0')) THEN
			next_pc <= predict_br_addr;
		ELSE
			next_pc <= pc_plus_four;
		END IF;
	END PROCESS;
	
	PROCESS (instruction(15 DOWNTO 0), pc_plus_four) --predict_br_addr process --static branch prediction (if forward pointing address, predict not taken, if not forward pointing, predict not taken)
	BEGIN
		IF (instruction(15) = '1') THEN	--negative offset, backwards pointing, so predict taken
			predict_br_addr <= pc_plus_four;-- &(X"FFFF" & instruction(15 DOWNTO 0) & "00");
			predict_br_taken <= '1';
		ELSE --positive offset, forward pointing, predict not taken
			predict_br_addr <= pc_plus_four;-- &(X"0000" & instruction(15 DOWNTO 0) & "00");			
			predict_br_taken <= '1';
		END IF;
	END PROCESS;
	
	jump <= '1' WHEN ((opcode = "000010") OR (opcode = "000011")) ELSE '0'; --we will handle jump to reg as a branch, we cannot calculate address here
	
	branch <= '1' WHEN ((opcode = "00100") OR (opcode = "000101")) ELSE '0';
	
	regT_out <= instruction(20 DOWNTO 16);
	
	PROCESS (opcode, func) --changing fu_dest (sending to proper functional unit)
	BEGIN
		IF (opcode = "100011") THEN --LW
			fu_dest <= fu_memory_unit;
			regS_out <= instruction(25 DOWNTO 21);
		ELSIF (opcode = "101011") THEN --SW
			fu_dest <= fu_memory_unit;
			regS_out <= instruction(25 DOWNTO 21);
		ELSIF (opcode = "110001") THEN --LOAD FP Single
			fu_dest <= fu_memory_unit;
			regS_out <= instruction(15 DOWNTO 11);
		ELSIF (opcode = "111001") THEN --STORE FP Single
			fu_dest <= fu_memory_unit;
			regS_out <= instruction(15 DOWNTO 11);
		ELSIF (opcode = "010001") AND (func = "000000") THEN --FP ADD Single
			fu_dest <= fu_fp_adder_unit;
			regS_out <= instruction(15 DOWNTO 11);
		ELSIF (opcode = "010001") AND (func = "000001") THEN --FP SUB Single
			fu_dest <= fu_fp_adder_unit;
			regS_out <= instruction(15 DOWNTO 11);
		ELSIF (opcode = "010001") AND (func = "000010") THEN --FP SUB Single
			fu_dest <= fu_fp_mult_unit;
			regS_out <= instruction(15 DOWNTO 11);
		ELSE
			fu_dest <= fu_integer_unit;
			regS_out <= instruction(25 DOWNTO 21);
		END IF;
	END PROCESS;
	
	PROCESS (opcode, func, fu_mem_unit_full, fu_fp_add_unit_full, fu_fp_mult_unit, fu_int_unit_full) --changing fu_dest (sending to proper functional unit)
	BEGIN
		IF (opcode = "100011") THEN --LW
			IF (fu_mem_unit_full = '1') THEN
				fu_stall_n <= '0';
			ELSE
				fu_stall_n <= '1';
			END IF;
		ELSIF (opcode = "101011") THEN --SW
			IF (fu_mem_unit_full = '1') THEN
				fu_stall_n <= '0';
			ELSE
				fu_stall_n <= '1';
			END IF;
		ELSIF (opcode = "110001") THEN --LOAD FP Single
			IF (fu_mem_unit_full = '1') THEN
				fu_stall_n <= '0';
			ELSE
				fu_stall_n <= '1';
			END IF;
		ELSIF (opcode = "111001") THEN --STORE FP Single
			IF (fu_mem_unit_full = '1') THEN
				fu_stall_n <= '0';
			ELSE
				fu_stall_n <= '1';
			END IF;
		ELSIF (opcode = "010001") AND (func = "000000") THEN --FP ADD Single
			IF (fu_fp_add_unit_full = '1') THEN
				fu_stall_n <= '0';
			ELSE
				fu_stall_n <= '1';
			END IF;
		ELSIF (opcode = "010001") AND (func = "000001") THEN --FP SUB Single
			IF (fu_fp_add_unit_full = '1') THEN
				fu_stall_n <= '0';
			ELSE
				fu_stall_n <= '1';
			END IF;
		ELSIF (opcode = "010001") AND (func = "000010") THEN --FP SUB Single
			IF (fu_fp_mult_unit_full = '1') THEN
				fu_stall_n <= '0';
			ELSE
				fu_stall_n <= '1';
			END IF;
		ELSE
			IF (fu_int_unit_full = '1') THEN
				fu_stall_n <= '0';
			ELSE
				fu_stall_n <= '1';
			END IF;
		END IF;
	END PROCESS;
	
	
	
END behavior;
