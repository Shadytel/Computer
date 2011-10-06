----------------------------------------------------------------------------------
-- Company: Lake Union Bell
-- Engineer: Nick Burrows
-- 
-- Create Date:    21:31:08 09/24/2011 
-- Design Name: 
-- Module Name:    InputController - Behavioral 
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

library work;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity InputController is
	Port(
		  clk: in std_logic;
		  RAMwrite: out std_logic;
		  RAMread: out std_logic;
		  MicroCodeCLK: out std_logic;
		  MicroCodeEN: out std_logic;
		  Data: out std_logic_vector(11 downto 0);
		  Addr: out std_logic_vector(11 downto 0);
		  outNibble: out std_logic_vector(3 downto 0);
		  sw: in std_logic_vector(7 downto 0); 		  
        RAWbtn : in std_logic_vector(3 downto 0)		  
		);
end InputController;

	--Sw7 High= Run, Low=Step 1 microcode on btn0
	--Sw6 High=AllowRun , Low=Edit
	--Sw5 Nibble Select High= Upper, Low=Middle
	--Sw4 Nibble Select High= other, Low=Lower
	--Sw3:0 1 Nibble
	
	--Btn 3: Stores Nibble
	--Btn 2: Stores Word
	--Btn 1: Increments memory counter
	--Btn 0: Run 1 step
architecture Behavioral of InputController is
	signal cnt : std_logic_vector(24 downto 0) := "0000000000000000000000000";
	signal word: std_logic_vector(11 downto 0) := "000000000000";
	signal loc:  std_logic_vector(11 downto 0) := "000000000000";
	signal btn: std_logic_vector(3 downto 0) := "0000";
	signal step: std_logic :='0';
	signal cooldown: std_logic :='0';
	signal cooldown2: std_logic :='0';
	signal cooldown3: std_logic :='0';
	signal cooldown4: std_logic :='0';
	alias running is sw(7);
	--alias step is btn(0);
	alias nibble is sw(3 downto 0);
	component Debouncer 
		Port (
		 CLK : in  STD_LOGIC;
		 x : in  STD_LOGIC;
		 DBx : out  STD_LOGIC
		); 
	end component;
begin
	
	db1: Debouncer 
	   port map (
				CLK => CLK,    -- Clock
				X => RAWbtn(0),
				DBx => btn(0)
		);
	db2: Debouncer  port map (
			CLK => CLK,    -- Clock
			X => RAWbtn(1),
			DBx => btn(1)
	);
	db3: Debouncer  port map (
			CLK => CLK,    -- Clock
			X => RAWbtn(2),
			DBx => btn(2)
	);
	db4: Debouncer  port map (
			CLK => CLK,    -- Clock
			X => RAWbtn(3),
			DBx => btn(3)
	);
	step <= btn(0);
	
--	process(cnt)
--	begin
--	   if(cnt(24 downto 23) = "00") then
--			Data <= "ZZZZZZZZZZZZ";
--			Addr <= "ZZZZZZZZZZZZ";
--			RAMread <= 'Z';
--			RAMwrite <= 'Z';
--			MicroCodeCLK <= '1';
--		else
--			MicroCodeCLK <= '0';
--		end if;
--	end process;
	--MicroCodeEN <= sw(6);
	
   process (clk, sw, btn, loc, step, word, cooldown, nibble, running, cooldown2, cooldown3, cooldown4, cnt)
	begin
		
	   if rising_edge(clk) then
			if(sw(6) = '0') then
					MicroCodeCLK <= '0';
					MicroCodeEN <= '0';
					if(btn(3) = '0' and cooldown4 = '0') then
						if(sw(5 downto 4) = "00") then
							word(3 downto 0) <= sw(3 downto 0);	
							outNibble <= sw(3 downto 0);
						elsif(sw(5 downto 4) = "01") then
							word(7 downto 4) <= sw(3 downto 0);
							outNibble <= sw(3 downto 0);
						elsif(sw(5 downto 4) = "11") then
							word(11 downto 8) <= sw(3 downto 0);
							outNibble <= sw(3 downto 0);
						end if;
						cooldown4 <= '1';
					elsif(btn(3) = '1') then
						cooldown4 <= '0';
					end if;		
					
					if(btn(1) = '0' and cooldown2 = '0') then
						loc <= loc + "000000000001";						
						cooldown2 <= '1';
					elsif(btn(1) = '1') then
						cooldown2 <= '0';
					end if;
					
					if(btn(0) = '1' and cooldown3 = '0') then						 
						loc <= loc - "000000000001";	
						cooldown3 <= '1';
					elsif(btn(0) = '0') then
						cooldown3 <= '0';
					end if;	
					
					Addr <= loc;								
					
					if(btn(2) = '1' and cooldown = '0') then
						Data <= word;
					   RAMread <= '0';
						RAMWrite <= '1';						
						cooldown <= '1';
					elsif(btn(2) = '0') then
					   cooldown <= '0';
						RAMWrite <= '0';
						RAMread <= '1';
						Data <= "ZZZZZZZZZZZZ";
					end if;					
										
				--end if;
			else
				Data <= "ZZZZZZZZZZZZ";
				Addr <= "ZZZZZZZZZZZZ";
				RAMWrite <= 'Z';
				RAMread <= 'Z';
				MicroCodeEN <= '1';
--				if(btn(0) = '0' and cooldown3 = '0') then
--					MicroCodeCLK <= '1';	
--					cooldown3 <= '1';
--				elsif(btn(0) = '1') then
--					cooldown3 <= '0';
--				end if;
				
				if(sw(7) = '1') then
					cnt <= cnt + "0000000000000000000000001";
					if(cnt = "1111111111111111111111111") then
						MicroCodeCLK <= '1';
					end if;
				else
				   if(btn(0) = '1' and cooldown3 = '0' ) then
						MicroCodeCLK <= '1';
						cooldown3 <= '1';
					elsif(btn(0) = '0') then
						cooldown3 <= '0';
					end if;
				end if;
			end if;
		end if;			
			
		if(clk = '0') then
			MicroCodeCLK <= '0';
		--	if(cooldown = '0') then
		--	Data <= "ZZZZZZZZZZZZ";	
		--	end if;
		end if;		
	   
	end process;
end Behavioral;

