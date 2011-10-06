----------------------------------------------------------------------------------
-- Company: Lake Union Bell
-- Engineer: Nick Burrows
-- 
-- Create Date:    19:03:40 09/24/2011 
-- Design Name: 
-- Module Name:    ALU - Behavioral 
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

entity ALU is
Port ( 
	        En: in  STD_LOGIC;
			  LM: in  STD_LOGIC; --Logical operation mode
			  A : in  STD_LOGIC_VECTOR (11 downto 0);
           B : in  STD_LOGIC_VECTOR (11 downto 0);	
			  Func: in STD_LOGIC_VECTOR (3 downto 0);			  
           Output : out STD_LOGIC_VECTOR (11 downto 0)
			 );
end ALU;

architecture Behavioral of ALU is
begin
	process (En, A, B, Func, LM)
	begin
		if(En = '1') then
			if(LM = '0') then
				if(Func = "0000") then
					Output <= (A) + (B);
				elsif(Func = "0001") then
					Output <= (A) - (B);
				elsif(Func = "0010") then
					Output <= (B) - (A);
				elsif(Func = "0011") then
					Output <= (B) - 1;
				elsif(Func = "0100") then
					Output <= (B) + 1;
				elsif(Func = "0101") then
					Output <= 0 - (B);
				elsif(Func = "0110") then
					if((A) < (B)) then
						Output(0) <= '1';
					else 
						Output(0) <= '0';
					end if;
				elsif(Func = "0111") then
					if((A) < (B)) then
						Output(0) <= '1'; 
					else 
						Output(0) <= '0';
					end if;
				else
					Output <= "ZZZZZZZZZZZZ";
				end if;
			else
			   if(Func = "0000") then
					Output <= not B;
				elsif(Func = "0001") then
					Output <= A nor B;
				elsif(Func = "0010") then
					Output <= (not B) and A;
				elsif(Func = "0011") then
					Output <= "000000000000";
				elsif(Func = "0100") then
					Output <= B nand A;
				elsif(Func = "0101") then
					Output <= not A;
				elsif(Func = "0110") then
				   Output <= B xor A;
				elsif(Func = "0111") then
					Output <= B and (not A);
				elsif(Func = "1000") then
					Output <= (not B) or A;
				elsif(Func = "1001") then
					Output <= B xnor A;
				elsif(Func = "1010") then
					Output <= A;
				elsif(Func = "1011") then
					Output <= B and A;
				elsif(Func = "1100") then
					Output <= "000000000001";
				elsif(Func = "1101") then
					Output <= B or (not A);
				elsif(Func = "1110") then
				   Output <= B or A;
				elsif(Func = "1111") then
					if(B = 0) then
						Output(0) <= '1'; 
					else 
						Output(0) <= '0';
					end if;
				end if;
			end if;
		else
		   Output <= "ZZZZZZZZZZZZ";
		end if;
	end process;

end Behavioral;

