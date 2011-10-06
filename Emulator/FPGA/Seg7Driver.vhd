----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:10:20 09/24/2011 
-- Design Name: 
-- Module Name:    Seg7Driver - Behavioral 
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
use IEEE.std_logic_unsigned.all;


-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Seg7Driver is
	Port(
		  clk: in std_logic;
		  data: in std_logic_vector(11 downto 0);
		  nibble: in std_logic_vector(3 downto 0);
		  seg: out std_logic_vector(6 downto 0); -- segment outputs
        an : out std_logic_vector(3 downto 0) -- anode select signals
		);
end Seg7Driver;

architecture Behavioral of Seg7Driver is
	signal HEX : std_logic_vector(4 downto 0);  -- a digit for 7 seg display
	signal cnt : std_logic_vector(10 downto 0):="00000000000";-- divider for 7SD
	alias cntr is cnt(10 downto 9);
begin
	process (clk, cnt)
	 begin
	  if rising_edge(clk) then
		cnt <= cnt + "00000000000000001";
	  end if;
	end process;
	
   process(cnt, data, nibble)
	begin
		if(cntr = "00") then
			HEX(3 downto 0) <= data(3 downto 0);
			HEX(4) <= '0';
		elsif(cntr= "01") then
		   HEX(3 downto 0) <= data(7 downto 4);
			HEX(4) <= '0';
		elsif(cntr = "10") then
			HEX(3 downto 0) <= data(11 downto 8);
			HEX(4) <= '0';
		elsif(cntr = "11") then
			HEX(3 downto 0) <= nibble;
			HEX(4) <= '0';
		end if;
	end process;
	
	with cntr select
	an <= "1110" when "00",
			"1101" when "01",
			"1011" when "10",
			"0111" when others; 

	--HEX-to-seven-segment decoder
	-- 
	-- segment encoding
	--      0
	--     ---  
	--  5 |   | 1
	--     ---   <- 6
	--  4 |   | 2
	--     ---
	--      3
   
   with HEX select
   seg<= "1000000" when "00000",   --0
         "1111001" when "00001",   --1
         "0100100" when "00010",   --2
         "0110000" when "00011",   --3
         "0011001" when "00100",   --4
         "0010010" when "00101",   --5
         "0000010" when "00110",   --6
         "1111000" when "00111",   --7
         "0000000" when "01000",   --8
         "0010000" when "01001",   --9
			"0001000" when "01010",   --A
			"0000011" when "01011",   --B
			"1000110" when "01100",   --C
			"0100001" when "01101",   --D
			"0000110" when "01110",   --E
			"0001110" when "01111",   --F
         "0111111" when "10000",   -- minus sign
         "1111111" when others;   -- nothing, for plus sign

end Behavioral;

