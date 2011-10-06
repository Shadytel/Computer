----------------------------------------------------------------------------------
-- Company: Lake Union Bell
-- Engineer: Nick Burrows
-- 
-- Create Date:    21:58:16 09/22/2011 
-- Design Name: 
-- Module Name:    MicrocodeProcessor - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity MicrocodeProcessor is
    Port ( 
	        CLK: in  STD_LOGIC;
			  EN: in  STD_LOGIC;
			  microcode: out std_logic_vector(3 downto 0);	
			  DataBus : inout STD_LOGIC_VECTOR (11 downto 0);
           AddrBus : out STD_LOGIC_VECTOR (11 downto 0);			  
           Control : out STD_LOGIC_VECTOR (11 downto 0)		  
			 );
end MicrocodeProcessor;

architecture Behavioral of MicrocodeProcessor is
   signal ProgramCounter: std_logic_vector(11 downto 0);
	signal Instruction: std_logic_vector(11 downto 0); 
	signal Flags: std_logic_vector(11 downto 0); 
   signal microcodePC: std_logic_vector(3 downto 0) := "1111";	
	signal pOut: std_logic_vector(3 downto 0) := "0000"; 	
	
   signal output: std_logic_vector(6 downto 0); 
--	alias AAA 	is Instruction(5 downto 3); 
--   alias BBB 	is Instruction(2 downto 0);
--	alias PPP	is Instruction(8 downto 6);
--	alias PPPP	is	Instruction(9 downto 6);
--   alias RR  	is Instruction(9 downto 8);
--	alias QQ  	is Instruction(7 downto 6);
--	alias KKKK  is Instruction(6 downto 3);
--	alias MajOp is Instruction(11 downto 10);
--	alias LoadType is Instruction(9 downto 8);
--	alias SubOp is Instruction(9 downto 7);
--	alias MathCmp is Instruction(9);
--	alias iMathCmp is Instruction(6);
	alias AAA 	is Instruction(8 downto 7); 
   alias BBB 	is Instruction(11 downto 9);
	alias PPP	is Instruction(6 downto 4);
	alias PPPP	is	Instruction(6 downto 3);
   alias RR  	is Instruction(4 downto 3);
	alias QQ  	is Instruction(6 downto 5);
	alias KKKK  is Instruction(8 downto 5);
	alias MajOp is Instruction(1 downto 0);
--	alias LoadType is Instruction(9 downto 8);
	alias SubOp is Instruction(4 downto 2);
	alias MathCmp is Instruction(2);
	alias iMathCmp is Instruction(6);
	constant WriteAtoR: std_logic_vector(6 downto 0):= "0100001";
	constant WriteBtoR: std_logic_vector(6 downto 0):= "0101000";
	constant WriteRtoA: std_logic_vector(6 downto 0):= "0010010";
	constant WriteRtoB: std_logic_vector(6 downto 0):= "0011000";
	constant WriteR: std_logic_vector(6 downto 0):= "0100000";
	constant ReadR: std_logic_vector(6 downto 0):= "0010000";
	constant ExeIntoR: std_logic_vector(6 downto 0):= "1100000";
	--Microcode structure:
	--6. Execute
	--5. Write to RAM
	--4. Enable RAM
	--3. Write to Register B
	--2. Enable Register B
	--1. Write to Register A
	--0. Enable Register A	
begin

   Control(0) <= output(4);
	Control(1) <= output(5);
	Control(2) <= output(1);	
   Control(3) <= output(3);
	Control(5) <= output(6);
	
	Control(11 downto 8) <= pOut;
   process (CLK,EN, microcodePC, output, Instruction, Flags, ProgramCounter, DataBus, pOut)
	begin
	   if(EN = '1') then
		  if(CLK = '1') then	
				microcode <= microcodePC;				
				microcodePC <= microcodePC + "0001";
				
            if(microcodePC = "0000") then --Load RAM Program Counter
					AddrBus <= "000000000000";
					output <= ReadR;
					ProgramCounter <= DataBus;
					
				elsif(microcodePC = "0001") then --Load Flags
					AddrBus <= "000000000001";
					output <= ReadR;					
					Flags <= DataBus;
					
				elsif(microcodePC = "0010") then --Load Assembly Instruction
					AddrBus <= ProgramCounter;
					output <= ReadR;
					Instruction <= DataBus;
					
				elsif(microcodePC = "0111") then --Inc PC
					AddrBus <= "000000000000";
					DataBus <= ProgramCounter + 1;
					output <= "0100000";
					
				else
				   --Math Opcode-------------------------------------------------------------------------------------------------------	
				   if(MajOp = "00") then 
					  if(microcodePC = "0011") then --Load MA->A
							if(MathCmp = '0') then --Check If Execute on Compare
								pOut(2 downto 0) <= PPP;
								AddrBus(2 downto 0) <= AAA; 
								output <= WriteRtoA;
							else --Check Compare Flag
								if (Flags(0)  = '1') then --Compare bit set
									pOut(2 downto 0) <= PPP;
									AddrBus(2 downto 0) <= AAA; 
									output <= WriteRtoA;
								else
								  microcode <= "0111";
								end if;							
							end if;						 
					  elsif(microcodePC = "0100") then --Load MB->B
							AddrBus(2 downto 0) <= BBB; 
							output <= WriteRtoB;							
					  elsif(microcodePC = "0101") then 
							pOut(2 downto 0) <= PPP;
							AddrBus(2 downto 0) <= BBB;
							output <= ExeIntoR; --Execute into MB
							
					  end if;
					  
					--Logic Opcode------------------------------------------------------------------------------------------------------	 
					elsif (MajOp = "10") then --Logic Opcode
					  if(microcodePC = "0011") then --Load MA->A						
							AddrBus(2 downto 0) <= AAA; 
							output <= WriteRtoA;											 
					  elsif(microcodePC = "0100") then --Load MB->B
							pOut(2 downto 0) <= PPP;
							AddrBus(2 downto 0) <= BBB;
							output <= WriteRtoB;
					  elsif(microcodePC = "0101") then --Execute into R
							pOut(3 downto 0) <= PPPP;
							AddrBus(2 downto 0) <= BBB; 
							output <= ExeIntoR;
					  end if;
					  
					--Load Opcode-------------------------------------------------------------------------------------------------------	  
					elsif (MajOp = "01") then --Load Opcode
						if(microcodePC = "0011") then --Load MA->A
							if(RR = "00") then --Direct Access
								AddrBus(2 downto 0) <= AAA; 
								output <= WriteRtoA;	
							--	microcodePC <= "0100"; --Jump past Indirect access microcode
							elsif(RR = "01") then --Indirect access
							   AddrBus(2 downto 0) <= AAA;								
								output <= ReadR;
								Flags <= DataBus; -- Dont need flags for this option
							elsif(RR = "10") then
								AddrBus(2 downto 0) <= AAA;								
								output <= ReadR;
								Flags <= DataBus; -- Dont need flags for this option
							elsif(RR = "11") then
								AddrBus(2 downto 0) <= AAA;								
								output <= ReadR;
								Flags <= DataBus - 1; -- Dont need flags for this option
							end if;
							
						elsif(microcodePC = "0100") then --Restore inc/dec pointer
							AddrBus(2 downto 0) <= AAA;
							if(RR = "10") then
								output <=WriteR;
								DataBus <= Flags + 1;
							elsif(RR = "11") then
								output <=WriteR;
								DataBus <= Flags + 1;								
							end if;
							
						elsif(microcodePC = "0101") then --Indirect access A
							if(RR /= "00") then --Indirect access
							   AddrBus <= Flags;
								output <= WriteRtoA; --Fetch Memory into A
							end if;
							
						elsif(microcodePC = "0110") then --Load MB->B
							if(QQ = "00") then --Direct Access B
								AddrBus(2 downto 0) <= BBB; 
								output <= WriteRtoB;
								--microcodePC <= "0110"; --Jump past indirect
							elsif(QQ = "01") then --Indirect access
							   AddrBus(2 downto 0) <= BBB;								
								output <= ReadR;
								Flags <= DataBus; -- Dont need flags for this option
							elsif(QQ = "10") then --Indirect access
							   AddrBus(2 downto 0) <= BBB;								
								output <= ReadR;
								Flags <= DataBus; -- Dont need flags for this option									
							elsif(QQ = "11") then --Indirect access
							   AddrBus(2 downto 0) <= BBB;								
								output <= ReadR;
								Flags <= DataBus -1; -- Dont need flags for this option
							end if;
					
					  elsif(microcodePC = "0111") then --Restore inc/dec pointer
							AddrBus(2 downto 0) <= BBB;
							if(RR = "10") then
								output <=WriteR;
								DataBus <= Flags + 1;
							elsif(RR = "11") then
								output <=WriteR;
								DataBus <= Flags + 1;								
							end if;
							
						elsif(microcodePC = "1000") then --Indirect write into B
						   if(QQ /= "00") then --Indirect access
							   AddrBus <= Flags;
								output <= WriteAtoR; 
							end if;
						end if;	
						
					--Set/Right/Immediate Math Opcode----------------------------------------------------------------------------------	
					elsif (MajOp = "11") then --Set/Right/Immediate Math
						if(SubOp = "100") then --Set subopcode
							if(microcodePC = "0011") then --Load MA->A
								AddrBus(2 downto 0) <= AAA; 
								output <= ReadR;
								Flags <= DataBus; --Not needed yet
							elsif(microcodePC = "0100") then
								AddrBus(2 downto 0) <= BBB; 
								output <= ReadR;
								if(Flags = DataBus) then
									AddrBus(3 downto 0) <= "0001";
									output <= WriteR;					
									DataBus <= "000000000001";
								else
									AddrBus(3 downto 0) <= "0001";
									output <= WriteR;					
									DataBus <= "000000000000";
								end if;							   
							end if;
							
						elsif(SubOp = "010") then --Rotate Right subopcode
						   if(microcodePC = "0011") then --Load *B->f
								AddrBus(2 downto 0) <= BBB; 
								output <= ReadR;
								Flags <= DataBus; 
							elsif(microcodePC = "0100") then
								AddrBus(2 downto 0) <= BBB; 
								output <= WriteR;
								--DataBus <= Flags ror 6;--(conv_integer(KKKK));							   
							end if;
						elsif(SubOp = "110") then --Immediate math
						   if(microcodePC = "0011") then --Load MA->A
								if(iMathCmp = '0') then --Check If Execute on Compare
									pOut(2 downto 0) <= AAA;
									AddrBus(2 downto 0) <= BBB; 
									output <= WriteRtoB;
								else --Check Compare Flag
									if (Flags(0)  = '1') then --Compare bit set
										pOut(2 downto 0) <= AAA;
										AddrBus(2 downto 0) <= BBB; 
										output <= WriteRtoB;
									else
										microcode <= "0111";
									end if;							
								end if;						 
						   elsif(microcodePC = "0100") then --Execute into MB
								pOut(2 downto 0) <= AAA;
								AddrBus(2 downto 0) <= BBB; 
								output <= ExeIntoR;
							end if;
					   end if;
				   end if;
				end if;						
				
			elsif(CLK='0') then
			end if;
			else
			   output <= "ZZZZZZZ";
				AddrBus <= "ZZZZZZZZZZZZ";
				DataBus <= "ZZZZZZZZZZZZ";
				pOut <= "ZZZZ";
				Control <= "ZZZZZZZZZZZZ";
				microcodePC <= "0000";
		  end if;		  
	end process;
	
end Behavioral;

