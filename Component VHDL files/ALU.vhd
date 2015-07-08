LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY mips_alu IS
    PORT ( instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0 );
           inputA, inputB : IN STD_LOGIC_VECTOR ( 31 DOWNTO 0 );
           shamt        : IN STD_LOGIC_VECTOR( 4 DOWNTO 0 );
           zero            : OUT STD_LOGIC;
           alu_result    : OUT STD_LOGIC_VECTOR( 31 DOWNTO 0 ) );
END mips_alu;
-- inputB = rt; inputA = rs
ARCHITECTURE a OF mips_alu IS
SIGNAL int_shift : STD_LOGIC_VECTOR( 4 DOWNTO 0 );
SIGNAL tmp_alu_result : STD_LOGIC_VECTOR( 31 DOWNTO 0 );
SIGNAL alucontrol : STD_LOGIC_VECTOR(3 DOWNTO 0);
SIGNAL functioncb : STD_LOGIC_VECTOR(5 DOWNTO 0);
SIGNAL opcode : STD_LOGIC_VECTOR(5 DOWNTO 0);

BEGIN
    functioncb <= instruction(5 DOWNTO 0);
    opcode <= instruction(31 DOWNTO 26);

    PROCESS ( functioncb, opcode ) 
    BEGIN
        IF opcode = "000000" THEN -- r type
            IF functioncb = "100000" THEN -- add
                alucontrol <= "0010";
            ELSIF functioncb = "100001" THEN -- addu
                alucontrol <= "0010";
            ELSIF functioncb = "100100" THEN -- andi
                alucontrol <= "0000";
            ELSIF functioncb = "001000" THEN -- jr
                alucontrol <= "1111";
            ELSIF functioncb = "100111" THEN -- nor
                alucontrol <= "1100";
            ELSIF functioncb = "100101" THEN -- or
                alucontrol <= "0001";
            ELSIF functioncb = "101010" THEN -- slt
                alucontrol <= "0111"; 
            ELSIF functioncb = "101011" THEN -- sltu
                alucontrol <= "0111";
            ELSIF functioncb = "000000" THEN -- sll
                alucontrol <= "1000";
            ELSIF functioncb = "000010" THEN -- srl
                alucontrol <= "1001";
            ELSIF functioncb = "000100" THEN -- sllv
                alucontrol <= "1010";
            ELSIF functioncb = "000110" THEN -- srlv
                alucontrol <= "1011";
            ELSIF functioncb = "100010" THEN -- sub
                alucontrol <= "0110";
            ELSIF functioncb = "100011" THEN -- subi
                alucontrol <= "0110";
            ELSE
                alucontrol <= "0010";
            END IF;
        ELSIF opcode = "001000" THEN -- addi
            alucontrol <= "0010";
        ELSIF opcode = "001001" THEN --addiu
            alucontrol <= "0010";
        ELSIF opcode = "001100" THEN --andi
            alucontrol <= "0000";
        ELSIF opcode = "001101" THEN --ori
            alucontrol <= "0001";
        ELSIF opcode = "001010" THEN -- slti
            alucontrol <= "0111";
        ELSIF opcode = "001011" THEN --sltiu
            alucontrol <= "0111";
        ELSIF opcode = "000100" THEN -- beq
            alucontrol <= "0110";
        ELSIF opcode = "000101" THEN -- bne
            alucontrol <= "0110";
        ELSIF opcode = "001111" THEN -- lui
            alucontrol <= "1000";
        ELSE 
            alucontrol <= "0010";
        END IF;
    END PROCESS;

    alu_result <= tmp_alu_result;
    PROCESS ( alucontrol, inputA, inputB, int_shift, instruction, opcode )
    BEGIN
        IF alucontrol = "0000" THEN
            IF (opcode = "000000") THEN
                tmp_alu_result <= inputA AND inputB;
            ELSE
                tmp_alu_result <= inputA AND instruction(15 DOWNTO 0);
            END IF;
        ELSIF alucontrol = "0001" THEN
            IF (opcode = "000000") THEN
                tmp_alu_result <= inputA OR inputB;
            ELSE
                tmp_alu_result <= inputA OR instruction(15 DOWNTO 0);
            END IF;
        ELSIF alucontrol = "0010" THEN
            IF (opcode = "000000") THEN
                tmp_alu_result <= inputA + inputB;
            ELSE
                tmp_alu_result <= inputA + instruction(15 DOWNTO 0);
            END IF;
        ELSIF alucontrol = "0110" THEN
            tmp_alu_result <= inputA - inputB;
        ELSIF alucontrol = "0111" THEN
            IF inputA < inputB THEN
                tmp_alu_result <= X"00000001";
            ELSE 
                tmp_alu_result <= X"00000000";
            END IF;
        ELSIF (alucontrol = "1000") OR (alucontrol = "1010") THEN
            CASE int_shift IS
                WHEN "00000" => tmp_alu_result <= inputB;
                WHEN "00001" => tmp_alu_result <= inputB(30 DOWNTO 0) & '0';
                WHEN "00010" => tmp_alu_result <= inputB(29 DOWNTO 0) & "00";
                WHEN "00011" => tmp_alu_result <= inputB(28 DOWNTO 0) & "000";
                WHEN "00100" => tmp_alu_result <= inputB(27 DOWNTO 0) & "0000";
                WHEN "00101" => tmp_alu_result <= inputB(26 DOWNTO 0) & "00000";
                WHEN "00110" => tmp_alu_result <= inputB(25 DOWNTO 0) & "000000";
                WHEN "00111" => tmp_alu_result <= inputB(24 DOWNTO 0) & "0000000";
                WHEN "01000" => tmp_alu_result <= inputB(23 DOWNTO 0) & "00000000";
                WHEN "01001" => tmp_alu_result <= inputB(22 DOWNTO 0) & "000000000";
                WHEN "01010" => tmp_alu_result <= inputB(21 DOWNTO 0) & "0000000000";
                WHEN "01011" => tmp_alu_result <= inputB(20 DOWNTO 0) & "00000000000";
                WHEN "01100" => tmp_alu_result <= inputB(19 DOWNTO 0) & "000000000000";
                WHEN "01101" => tmp_alu_result <= inputB(18 DOWNTO 0) & "0000000000000";
                WHEN "01110" => tmp_alu_result <= inputB(17 DOWNTO 0) & "00000000000000";
                WHEN "01111" => tmp_alu_result <= inputB(16 DOWNTO 0) & "000000000000000";
                WHEN "10000" => tmp_alu_result <= inputB(15 DOWNTO 0) & "0000000000000000";
                WHEN "10001" => tmp_alu_result <= inputB(14 DOWNTO 0) & "00000000000000000";
                WHEN "10010" => tmp_alu_result <= inputB(13 DOWNTO 0) & "000000000000000000";
                WHEN "10011" => tmp_alu_result <= inputB(12 DOWNTO 0) & "0000000000000000000";
                WHEN "10100" => tmp_alu_result <= inputB(11 DOWNTO 0) & "00000000000000000000";
                WHEN "10101" => tmp_alu_result <= inputB(10 DOWNTO 0) & "000000000000000000000";
                WHEN "10110" => tmp_alu_result <= inputB(9 DOWNTO 0) &  "0000000000000000000000";
                WHEN "10111" => tmp_alu_result <= inputB(8 DOWNTO 0) &  "00000000000000000000000";
                WHEN "11000" => tmp_alu_result <= inputB(7 DOWNTO 0) &  "000000000000000000000000";
                WHEN "11001" => tmp_alu_result <= inputB(6 DOWNTO 0) &  "0000000000000000000000000";
                WHEN "11010" => tmp_alu_result <= inputB(5 DOWNTO 0) &  "00000000000000000000000000";
                WHEN "11011" => tmp_alu_result <= inputB(4 DOWNTO 0) &  "000000000000000000000000000";
                WHEN "11100" => tmp_alu_result <= inputB(3 DOWNTO 0) &  "0000000000000000000000000000";
                WHEN "11101" => tmp_alu_result <= inputB(2 DOWNTO 0) &  "00000000000000000000000000000";
                WHEN "11110" => tmp_alu_result <= inputB(1 DOWNTO 0) &  "000000000000000000000000000000";
                WHEN "11111" => tmp_alu_result <= inputB(0 DOWNTO 0) &  "0000000000000000000000000000000";
            END CASE;
        ELSIF (alucontrol = "1001") OR (alucontrol = "1011") THEN
            CASE int_shift IS
                WHEN "00000" => tmp_alu_result <= inputB;
                WHEN "00001" => tmp_alu_result <= '0' & inputB(31 DOWNTO 1);
                WHEN "00010" => tmp_alu_result <= "00" & inputB(31 DOWNTO 2);
                WHEN "00011" => tmp_alu_result <= "000" & inputB(31 DOWNTO 3);
                WHEN "00100" => tmp_alu_result <= "0000" & inputB(31 DOWNTO 4);
                WHEN "00101" => tmp_alu_result <= "00000" & inputB(31 DOWNTO 5);
                WHEN "00110" => tmp_alu_result <= "000000" & inputB(31 DOWNTO 6);
                WHEN "00111" => tmp_alu_result <= "0000000" & inputB(31 DOWNTO 7);
                WHEN "01000" => tmp_alu_result <= "00000000" & inputB(31 DOWNTO 8);
                WHEN "01001" => tmp_alu_result <= "000000000" & inputB(31 DOWNTO 9);
                WHEN "01010" => tmp_alu_result <= "0000000000" & inputB(31 DOWNTO 10);
                WHEN "01011" => tmp_alu_result <= "00000000000" & inputB(31 DOWNTO 11);
                WHEN "01100" => tmp_alu_result <= "000000000000" & inputB(31 DOWNTO 12);
                WHEN "01101" => tmp_alu_result <= "0000000000000" & inputB(31 DOWNTO 13);
                WHEN "01110" => tmp_alu_result <= "00000000000000" & inputB(31 DOWNTO 14);
                WHEN "01111" => tmp_alu_result <= "000000000000000" & inputB(31 DOWNTO 15);
                WHEN "10000" => tmp_alu_result <= "0000000000000000" & inputB(31 DOWNTO 16);
                WHEN "10001" => tmp_alu_result <= "00000000000000000" & inputB(31 DOWNTO 17);
                WHEN "10010" => tmp_alu_result <= "000000000000000000" & inputB(31 DOWNTO 18);
                WHEN "10011" => tmp_alu_result <= "0000000000000000000" & inputB(31 DOWNTO 19);
                WHEN "10100" => tmp_alu_result <= "00000000000000000000" & inputB(31 DOWNTO 20);
                WHEN "10101" => tmp_alu_result <= "000000000000000000000" & inputB(31 DOWNTO 21);
                WHEN "10110" => tmp_alu_result <= "0000000000000000000000" & inputB(31 DOWNTO 22);
                WHEN "10111" => tmp_alu_result <= "00000000000000000000000" & inputB(31 DOWNTO 23);
                WHEN "11000" => tmp_alu_result <= "000000000000000000000000" & inputB(31 DOWNTO 24);
                WHEN "11001" => tmp_alu_result <= "0000000000000000000000000" & inputB(31 DOWNTO 25);
                WHEN "11010" => tmp_alu_result <= "00000000000000000000000000" & inputB(31 DOWNTO 26);
                WHEN "11011" => tmp_alu_result <= "000000000000000000000000000" & inputB(31 DOWNTO 27);
                WHEN "11100" => tmp_alu_result <= "0000000000000000000000000000" & inputB(31 DOWNTO 28);
                WHEN "11101" => tmp_alu_result <= "00000000000000000000000000000" & inputB(31 DOWNTO 29);
                WHEN "11110" => tmp_alu_result <= "000000000000000000000000000000" & inputB(31 DOWNTO 30);
                WHEN "11111" => tmp_alu_result <= "0000000000000000000000000000000" & inputB(31 DOWNTO 31);
            END CASE;
        ELSIF alucontrol = "1100" THEN
            tmp_alu_result <= inputA NOR inputB;
        ELSE 
            tmp_alu_result <= X"00000000";
        END IF;
    END PROCESS;
    zero <= '1' WHEN tmp_alu_result = X"00000000" ELSE '0';
    int_shift <= shamt WHEN ALUControl(1) = '0' ELSE inputA(4 DOWNTO 0);
END a;