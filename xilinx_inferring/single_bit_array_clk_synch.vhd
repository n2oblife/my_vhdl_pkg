-------------------------------------------------------------------------------------------------
-- Company : ...
-- Author : nooblife
-- Licensing : MIT
-------------------------------------------------------------------------------------------------
-- Version : V1
-- Version history :
-- V1 : dd-mm-yyyy : nooblife: Creation
-------------------------------------------------------------------------------------------------
-- File name : single_bit_array_clk_synch.vhd
-- File Creation date : dd-mm-yyyy
-- Project name : VHDL Random Project
-------------------------------------------------------------------------------------------------
-- Softwares : current OS - Editor (VSCode + ...)
-------------------------------------------------------------------------------------------------
-- Description: Template file ...
-- taken from : https://docs.amd.com/r/en-US/ug953-vivado-7series-libraries/XPM_CDC_ARRAY_SINGLE
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

-- xpm_cdc_array_single: Single-bit Array Synchronizer
-- Xilinx Parameterized Macro, version 2023.2

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library xpm;
use xpm.vcomponents.all;

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
		---------------------------------------------
		clk : in std_logic; -- standard clk signal 
		reset : in std_logic; -- standard reset signal
		---------------------------------------------
		
		-- INPUTS
		d: in std_logic; -- input signal or set of input signals
		
		-- OUTPUTS
		q: out std_logic -- output signal or set of output signals
	);
end entity entity_name;
-------------------------------------------------------------------------------------------------

---------------------------------------------

---------------------------------------------

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

xpm_cdc_array_single_inst : xpm_cdc_array_single
generic map (
   DEST_SYNC_FF => 4,   -- DECIMAL; range: 2-10
   INIT_SYNC_FF => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
   SIM_ASSERT_CHK => 0, -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
   SRC_INPUT_REG => 1,  -- DECIMAL; 0=do not register input, 1=register input
   WIDTH => 2           -- DECIMAL; range: 1-1024
)
port map (
   dest_out => dest_out, -- WIDTH-bit output: src_in synchronized to the destination clock domain. This
                         -- output is registered.

   dest_clk => dest_clk, -- 1-bit input: Clock signal for the destination clock domain.
   src_clk => src_clk,   -- 1-bit input: optional; required when SRC_INPUT_REG = 1
   src_in => src_in      -- WIDTH-bit input: Input single-bit array to be synchronized to destination clock
                         -- domain. It is assumed that each bit of the array is unrelated to the others.
                         -- This is reflected in the constraints applied to this macro. To transfer a binary
                         -- value losslessly across the two clock domains, use the XPM_CDC_GRAY macro
                         -- instead.

);

-- End of xpm_cdc_array_single_inst instantiation

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
