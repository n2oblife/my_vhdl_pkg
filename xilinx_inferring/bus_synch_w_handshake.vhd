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
-- taken from : https://docs.amd.com/r/en-US/ug953-vivado-7series-libraries/XPM_CDC_HANDSHAKE
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

-- xpm_cdc_handshake: Bus Synchronizer with Full Handshake
-- Xilinx Parameterized Macro, version 2023.2

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

Library xpm;
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

xpm_cdc_handshake_inst : xpm_cdc_handshake
generic map (
   DEST_EXT_HSK => 1,   -- DECIMAL; 0=internal handshake, 1=external handshake
   DEST_SYNC_FF => 4,   -- DECIMAL; range: 2-10
   INIT_SYNC_FF => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
   SIM_ASSERT_CHK => 0, -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
   SRC_SYNC_FF => 4,    -- DECIMAL; range: 2-10
   WIDTH => 1           -- DECIMAL; range: 1-1024
)
port map (
   dest_out => dest_out, -- WIDTH-bit output: Input bus (src_in) synchronized to destination clock domain.
                         -- This output is registered.

   dest_req => dest_req, -- 1-bit output: Assertion of this signal indicates that new dest_out data has been
                         -- received and is ready to be used or captured by the destination logic. When
                         -- DEST_EXT_HSK = 1, this signal will deassert once the source handshake
                         -- acknowledges that the destination clock domain has received the transferred
                         -- data. When DEST_EXT_HSK = 0, this signal asserts for one clock period when
                         -- dest_out bus is valid. This output is registered.

   src_rcv => src_rcv,   -- 1-bit output: Acknowledgement from destination logic that src_in has been
                         -- received. This signal will be deasserted once destination handshake has fully
                         -- completed, thus completing a full data transfer. This output is registered.

   dest_ack => dest_ack, -- 1-bit input: optional; required when DEST_EXT_HSK = 1
   dest_clk => dest_clk, -- 1-bit input: Destination clock.
   src_clk => src_clk,   -- 1-bit input: Source clock.
   src_in => src_in,     -- WIDTH-bit input: Input bus that will be synchronized to the destination clock
                         -- domain.

   src_send => src_send  -- 1-bit input: Assertion of this signal allows the src_in bus to be synchronized
                         -- to the destination clock domain. This signal should only be asserted when
                         -- src_rcv is deasserted, indicating that the previous data transfer is complete.
                         -- This signal should only be deasserted once src_rcv is asserted, acknowledging
                         -- that the src_in has been received by the destination logic.

);

-- End of xpm_cdc_handshake_inst instantiation

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
