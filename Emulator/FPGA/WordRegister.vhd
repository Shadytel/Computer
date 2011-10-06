----------------------------------------------------------------------------------
-- Company: Lake Union Bell
-- Engineer: Nick Burrows
-- 
-- Create Date:    20:48:18 09/22/2011 
-- Design Name: 
-- Module Name:    WordRegister - Behavioral 
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
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity WordRegister is
	Port
		(
         WR: in std_logic; --Write
			OEN: in std_logic; --OutputEnable
			OUTPUT: out std_logic_vector(11 downto 0);
         DATABUS: inout std_logic_vector(11 downto 0)
		);
end WordRegister;

architecture Behavioral of WordRegister is
      signal stores: std_logic_vector(11 downto 0);
begin
   DATABUS <= stores when (OEN = '1') else "ZZZZZZZZZZZZ";
	OUTPUT  <= stores when (OEN = '1') else "ZZZZZZZZZZZZ";

   process (WR, DATABUS, stores)
	begin
	   if(WR'event and WR = '1') then
				stores <= DATABUS;	
		end if;				
	end process;
	
   
end Behavioral;

