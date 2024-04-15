-------------------------------------------------------------------------------------------------
-- Company : ...
-- Author : nooblife
-- Licensing : MIT
-------------------------------------------------------------------------------------------------
-- Version : V1
-- Version history :
-- V1 : 05-04-2024 : nooblife: Creation
-------------------------------------------------------------------------------------------------
-- File name : tb_template.vhd
-- File Creation date : 05-04-2024
-- Project name : My VHDL package
-------------------------------------------------------------------------------------------------
-- Softwares : Windows (Windows 10 + 11) + Ubuntu (22LTS) - Editor (VSCode + Libero + ...)
-------------------------------------------------------------------------------------------------
-- Description: ...
--
-- Limitations : ...
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
-- f_My_function : Custom function
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

-------------------------------------------------------------------------------------------------
-- ENTITY
-------------------------------------------------------------------------------------------------
entity tb is
end entity tb;

-------------------------------------------------------------------------------------------------
-- ARCHITECTURE
-------------------------------------------------------------------------------------------------
architecture rtl of tb is

-------------------------------------------------------------------------------------------------
-- DESIGN UNDER TEST
-------------------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------------------
-- CONSTANTS
-------------------------------------------------------------------------------------------------
    
    -- time duration and time scale
    -- fs, ps, ns, us, ms, sec, min, hr
    ---------------------------------------------
    constant c_time     : time := 10 ns; -- half period
    ---------------------------------------------

    constant c_waiting  : time := 10*c_time;

-------------------------------------------------------------------------------------------------
-- SIGNALS
-------------------------------------------------------------------------------------------------
    
    signal clk      : std_logic :='0';
    signal reset    : std_logic := '0';

    signal sc_test1 : std_logic_vector;
    signal sc_test2 : std_logic_vector;

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
-- PROCESS
-------------------------------------------------------------------------------------------------

    -- CLK_PROCESS 
    clock : process 
    begin
        clk <= not clk after c_time;
    end process clock;    
    
-------------------------------------------------------------------------------------------------
    
    -- CTRL_PROCESS
    -- Process Description: a process which waits
    -- Process has no sensitivity list
    -- Additional details: 
    ctrl : process(clk)
    begin
        reset <= '1', '0' after 1000*c_time;
    end process ctrl; 

-------------------------------------------------------------------------------------------------

    -- stimulus_PROCESS
    -- Process Description: a process which waits
    -- Process has no sensitivity list
    -- Additional details: 
    STIMULUS : process
    begin
        -- wait reset release
        wait until reset = '0';

        -- Generate each of in turn, waiting 1 clock periods between
        -- each iteration to allow for propagation times
        sc_test1 <= "00";
        wait for 2*c_time;

        sc_test1 <= "01";
        wait for 2*c_time;

        sc_test1 <= "10";
        wait for 2*c_time;

        sc_test1 <= "11";
        -- Testing complete
        wait;

    end process stimulus;

-------------------------------------------------------------------------------------------------

    -- WAITING_PROCESS
    -- Process Description: a process which waits
    -- Process has no sensitivity list
    -- Additional details: 
    waiting : process
    begin
        wait for c_waiting;
        wait until <condition> for c_waiting;
        wait on sc_test1, sc_test2;
    end process waiting;

-------------------------------------------------------------------------------------------------
end architecture rtl;
-------------------------------------------------------------------------------------------------