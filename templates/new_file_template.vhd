-------------------------------------------------------------------------------------------------
-- Company : ...
-- Author : nooblife
-- Licensing : MIT
-------------------------------------------------------------------------------------------------
-- Version : V1
-- Version history :
-- V1 : dd-mm-yyyy : nooblife: Creation
-------------------------------------------------------------------------------------------------
-- File name : new_file_template.vhd
-- File Creation date : dd-mm-yyyy
-- Project name : VHDL Random Project
-------------------------------------------------------------------------------------------------
-- Softwares : current OS - Editor (VSCode + ...)
-------------------------------------------------------------------------------------------------
-- Description: Template file ...
--
-- Limitations : Copying means lot of work to do ...
--
-------------------------------------------------------------------------------------------------
-- Naming conventions:
--
-- i_Port: Input entity port
-- o_Port: Output entity port
-- b_Port: Bidirectional entity port
-- g_My_Generic: Generic entity port
--
-- c_My_Constant: Constant definition
-- t_My_Type: Custom type definition
--
-- sc_My_Signal : Signal between components
-- My_Signal_n: Active low signal
-- v_My_Variable: Variable
-- sm_My_Signal: FSM signal
-- pkg_Param: Element Param coming from a package
--
-- My_Signal_re: Rising edge detection of My_Signal
-- My_Signal_fe: Falling edge detection of My_Signal
-- My_Signal_rX: X times registered My_Signal signal
--
-- P_Process_Name: Process
--
-------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- my own package
-- library work;
-- use work.n2oblife_pkg.all;

-------------------------------------------------------------------------------------------------
-- ENTITY
-------------------------------------------------------------------------------------------------
entity entity_name is 
	generic();
	port (
		-- CONTROL
		clk : in std_logic; -- standard clk signal 
		reset : in std_logic; -- standard reset signal
		
		-- INPUTS
		d: in std_logic; -- input signal or set of input signals
		
		-- OUTPUTS
		q: out std_logic -- output signal or set of output signals
	);
end entity entity_name;
-------------------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------------------
-- ARCHITECTURE
-------------------------------------------------------------------------------------------------
architecture rtl of entity_name is

-------------------------------------------------------------------------------------------------
-- CONSTANTS
-------------------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------------------
-- SIGNALS
-------------------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------------------
-- COMPONENTS
-------------------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------------------
-- ATTRIBUTES
-------------------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------------------
begin
-------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------
-- MAPPING
-------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------
-- Processs
-------------------------------------------------------------------------------------------------
	-- Process Description: Change state of the entity_name on rising clock edge
	-- Process is synchronous to the main clk
	-- Additional details: Reset is synchronous,=
	entity_state: process(clk)
    begin
		if rising_edge(clk) then
			if(reset = '1') then
				-- Reset logic
			else then
				-- Run logic
			end if;				
		end if;
	end process entity_state;

-------------------------------------------------------------------------------------------------
-- OUTPUTS
-------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------
end architecture rtl;
-------------------------------------------------------------------------------------------------
