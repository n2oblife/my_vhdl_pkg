-------------------------------------------------------------------------------------------------
-- Company : ...
-- Author : nooblife
-- Licensing : MIT
-------------------------------------------------------------------------------------------------
-- Version : V1
-- Version history :
-- V1 : 05-04-2024 : nooblife: Creation
-------------------------------------------------------------------------------------------------
-- File name : n2oblife_pkg.vhd
-- File Creation date : 05-04-2024
-- Project name : My VHDL package
-------------------------------------------------------------------------------------------------
-- Softwares : Windows (Windows 10 + 11) + Ubuntu (22LTS) - Editor (VSCode + Libero + ...)
-------------------------------------------------------------------------------------------------
-- Description: This file is my VHDL package which contains useful functions and custom types
-- sorted by use case
--
-- Limitations : still learning
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
------------------------------------------ DECLARATION ------------------------------------------
-------------------------------------------------------------------------------------------------
package n2oblife_pkg is
-- Declaration of
    -- types and subtypes
    -- subprograms
    -- constants, signals etc.

-------------------------------------------------------------------------------------------------

    -- Basic Types
    subtypes sl     is std_logic;
    subtypes slv    is std_logic_vector;
    subtypes sgn    is signed;
    subtypes usgn   is unsigned;
    subtypes int    is integer;
    subtypes nat    is natural;
    subtypes pos    is positive;

    -- Data strcuture
    type buffer is array (natural <range>) of sl;
    type row    is array (natural <range>) of slv;
    type matrix is array (natural <range>) of row;


-------------------------------------------------------------------------------------------------

    -- Type conversion 
    function sgn        (vec : slv)                 return sgn;
    function usgn       (vec : slv)                 return usgn;
    function slv        (usig : usgn)               return slv;
    function sgn        (usig : usgn)               return sgn;
    function to_int     (usig : usgn)               return int;
    function slv        (sig : sgn)                 return slv;
    function usgn       (sig : sgn)                 return usgn;
    function to_int     (sig : sgn)                 return int;
    function to_sgn     (integ : int; length : int) return sgn;
    function to_usgn    (integ : int; length : int) return usgn;
    
    
    -- Operators overloading (return size handled), element wise operations
    -- TODO, add relations operators (= and /=)
    function "+"    (L:slv;     R:slv)      return slv;
    function "-"    (L:slv;     R:slv)      return slv;
    function "*"    (L:slv;     R:slv)      return slv;
    function "not"  (L:slv;     R:slv)      return slv;
    function "and"  (L:slv;     R:slv)      return slv;
    function "or"   (L:slv;     R:slv)      return slv;
    function "nand" (L:slv;     R:slv)      return slv;
    function "nor"  (L:slv;     R:slv)      return slv;
    function "xor"  (L:slv;     R:slv)      return slv;
    function "xnor" (L:slv;     R:slv)      return slv;
    ---------------------------------------------
    function "+"    (L:row;     R:row)      return row;
    function "*"    (L:row;     R:row)      return row;
    function "not"  (L:row;     R:row)      return row;
    function "and"  (L:row;     R:row)      return row;
    function "or"   (L:row;     R:row)      return row;
    function "nand" (L:row;     R:row)      return row;
    function "nor"  (L:row;     R:row)      return row;
    function "xor"  (L:row;     R:row)      return row;
    function "xnor" (L:row;     R:row)      return row;
    ---------------------------------------------
    function "+"    (L:matrix;  R:matrix)   return matrix;
    function "*"    (L:matrix;  R:matrix)   return matrix;
    function "not"  (L:matrix;  R:matrix)   return matrix;
    function "and"  (L:matrix;  R:matrix)   return matrix;
    function "or"   (L:matrix;  R:matrix)   return matrix;
    function "nand" (L:matrix;  R:matrix)   return matrix;
    function "nor"  (L:matrix;  R:matrix)   return matrix;
    function "xor"  (L:matrix;  R:matrix)   return matrix;
    function "xnor" (L:matrix;  R:matrix)   return matrix;

    
    -- Operation on data structures
    function "&"    (L:slv;     R:row)      return row;
    function "&"    (L:row;     R:slv)      return row;
    function "&"    (L:row;     R:row)      return row;
    function "&"    (L:row;     R:matrix)   return matrix;
    function "&"    (L:matrix;  R:row)      return matrix;
    function "&"    (L:matrix;  R:matrix)   return matrix;
    ---------------------------------------------
    function concat (L:row;     R:row)      return row;
    function concat (L:matrix;  R:matrix)   return matrix;



-------------------------------------------------------------------------------------------------

    -- Avalon Streaming Interface
    type ASI is record
        -- fundamental signals
        channel : slv;  -- the channel number for data being transferred
        data    : slv;  -- data source -> sink
        error   : slv;  -- bit mask to mark errors affecting the data
        ready   : sl    -- Asserts high to indicate that the sink can accept data
        valid   : sl;   -- valid signal from Avalon Streaming Protocol
        -- packet transfert signals
        empty   : slv   -- Indicates the number of symbols that are empty
        sop     : sl;   -- start of packet signal from Avalon Streaming Protocol
        eop     : sl;   -- end of packet signal from Avalon Streaming Protocol
    end record ASI;


    -- APB Interface
    type APB is record
        -- CONTROL
        pclk        : sl;               -- APB clock
        preset      : sl;               -- APB reset
        -- INPUTS
        i_paddr     : slv(31 downto 0); -- APB address
        i_pdata     : slv;              -- APB data
        i_pwrite    : sl;               -- APB write (0:read / 1:write)
        i_pstb      : slv(3 downto 0);  -- APB strobe
        i_psel      : sl;               -- APB select
        i_penable   : sl;               -- APB enable
        i_pprot     : slv(2 downto 0);  -- APB protection 
                                            -- (xx0 : non-priviliged, xx1 : priviliged, 
                                            --  x0x : non-secure, x1x : secure, 
                                            --  0xx : data access, 1xx : instruction access)      
        -- OUTPUTS  
        o_pdata     : slv;              -- APB data from slave
        o_pready    : sl;               -- APB ready signal from slave
        o_pslverr   : sl;               -- APB slave error signal
    end record APB;


-------------------------------------------------------------------------------------------------

    -- Buffer handling functions
    function buffer_shift(in_sig : sl; buff : buffer )  return buffer;
    function buffer_shift(in_sig : sl; buff : slv )     return slv;

-------------------------------------------------------------------------------------------------
end n2oblife_pkg;
-------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------
--------------------------------------------- BODY ----------------------------------------------
-------------------------------------------------------------------------------------------------
package body n2oblife_pkg is
-- Definition of previously declared
    -- constants
    -- subprograms
-- Declaration/definition of additional
    -- types and subtypes
    -- subprograms
    -- constants, signals and shared variables

-------------------------------------------------------------------------------------------------

    -- Type conversion functions
    function sgn(vec : slv)     return sgn  is begin return signed(vec);            end function sgn;
    function usgn(vec : slv)    return usgn is begin return unsigned(vec);          end function usgn;
    function slv(usig : usgn)   return slv  is begin return std_logic_vector(usig); end function slv;
    function sgn(usig : usgn)   return sgn  is begin return signed(usig);           end function sgn;
    function to_int(usig:usgn)  return int  is begin return to_integer(usig);       end function to_int;
    function slv(sig : sgn)     return slv  is begin return std_logic_vector(sig);  end function slv;
    function usgn(sig : sgn)    return usgn is begin return unsigned(sig);          end function usgn;
    function to_int(sig : sgn)  return int  is begin return to_integer(sig);        end function to_int;
    function to_sgn(integ : int; length : int)  return sgn  is begin return signed(integ, length);      end function to_sgn;
    function to_usgn(integ : int; length : int) return usgn is begin return unsigned(integ, length);    end function to_usgn


    -- Operator overloading functions
    function "+"(L:slv; R:slv) return slv is variable result : slv(L'left+1 downto L'right);
    begin result = slv(usgn(L) + usgn(R)); return result; end function "+";

    function "-"(L:slv; R:slv) return slv is variable result : slv(L'range);
    begin result = slv(usgn(L) - usgn(R)); return result; end function "-";

    function "*"(L:slv; R:slv) return slv is variable result : slv(2*L'length+L'right-1 downto L'right);
    begin result = slv(usgn(L) * usgn(R)); return result; end function "*";

    function "not"(L:slv; R:slv) return slv is variable result : slv(L'range);
    begin result = slv(usgn(L) not usgn(R)); return result; end function "not";

    function "and"(L:slv; R:slv) return slv is variable result : slv(L'range);
    begin result = slv(usgn(L)and- usgn(R)); return result; end function "and"

    function "or"(L:slv; R:slv) return slv is variable result : slv(L'range);
    begin result = slv(usgn(L) or usgn(R)); return result; end function "or";

    function "nand"(L:slv; R:slv) return slv is variable result : slv(L'range);
    begin result = slv(usgn(L) nand usgn(R)); return result; end function "nand";

    function "nor"(L:slv; R:slv) return slv is variable result : slv(L'range);
    begin result = slv(usgn(L) nor usgn(R)); return result; end function "nor";

    function "xor"(L:slv; R:slv) return slv is variable result : slv(L'left+1 downto L'right);
    begin result = slv(usgn(L) xor usgn(R)); return result; end function "xor";

    function "xnor"(L:slv; R:slv) return slv is variable result : slv(L'range);
    begin result = slv(usgn(L) xnor usgn(R)); return result; end function "xnor";

    ---------------------------------------------

    function "+"(L:row; R:row) return row is 
        variable result : row(L'range)(L(0)'left+1 downto L'right);
    begin
        for i in L'range loop result(i) := L(i) + R(i); end loop;
        return result;
    end function "+";
    
    function "*"(L:row; R:row) return row is
        variable result : row(L'range)(2*L(0)'length+L(0)'right-1 downto L(0)'right);
    begin
        for i in L'range loop result(i) := L(i) * R(i); end loop;
        return result;
    end function "*";

    function "not"(L:row; R:row) return row is
        variable result : row(L'range)(L(0)'range);
    begin
        for i in L'range loop result(i) := L(i) not R(i); end loop;
        return result;
    end function "not";

    function "and"(L:row; R:row) return row is
        variable result : row(L'range)(L(0)'range);
    begin
        for i in L'range loop result(i) := L(i) and R(i); end loop;
        return result;
    end function "and";

    function "or"(L:row; R:row) return row is
        variable result : row(L'range)(L(0)'range);
    begin
        for i in L'range loop result(i) := L(i) or R(i); end loop;
        return result;
    end function "or";

    function "nand"(L:row; R:row) return row is
        variable result : row(L'range)(L(0)'range);
    begin
        for i in L'range loop result(i) := L(i) nand R(i); end loop;
        return result;
    end function "nand";

    function "nor"(L:row; R:row) return row is
        variable result : row(L'range)(L(0)'range);
    begin
        for i in L'range loop result(i) := L(i) nor R(i); end loop;
        return result;
    end function "nor";

    function "xor"(L:row; R:row) return row is
        variable result : row(L'range)(L(0)'range);
    begin
        for i in L'range loop result(i) := L(i) xor R(i); end loop;
        return result;
    end function "xor";

    function "xnor"(L:row; R:row) return row is
        variable result : row(L'range)(L(0)'range);
    begin
        for i in L'range loop result(i) := L(i) xnor R(i); end loop;
        return result;
    end function "xnor";

    ---------------------------------------------

    function "+"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(L(0)(0)'left+1 downto L'right);
    begin
        for i in L'range loop result(i) := L(i) + R(i); end loop;
        return result;
    end function "+";

    function "*"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(2*L(0)(0)'length+L(0)(0)'right-1 downto L(0)(0)'right);
    begin
        for i in L'range loop result(i) := L(i) * R(i); end loop;
        return result;
    end function "*";

    function "not"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(L(0)(0)'range);
    begin
        for i in L'range loop result(i) := L(i) not R(i); end loop;
        return result;
    end function "not";

    function "and"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(L(0)(0)'range);
    begin
        for i in L'range loop result(i) := L(i) and R(i); end loop;
        return result;
    end function "and";

    function "or"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(L(0)(0)'range);
    begin
        for i in L'range loop result(i) := L(i) or R(i); end loop;
        return result;
    end function "or";

    function "nand"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(L(0)(0)'range);
    begin
        for i in L'range loop result(i) := L(i) nand R(i); end loop;
        return result;
    end function "nand";

    function "nor"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(L(0)(0)'range);
    begin
        for i in L'range loop result(i) := L(i) nor R(i); end loop;
        return result;
    end function "nor";

    function "xor"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(L(0)(0)'range);
    begin
        for i in L'range loop result(i) := L(i) xor R(i); end loop;
        return result;
    end function "xor";

    function "xnor"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(L(0)(0)'range);
    begin
        for i in L'range loop result(i) := L(i) xnor R(i); end loop;
        return result;
    end function "xnor";
    

    -- Operations on data structures
    function "&"(L:slv; R:row) return row is
        variable result : row(R'range)(R'left+1 downto R'right);
    begin
        for i in R'range loop result(i) := L & R(i); end loop;
        return result;
    end function "&";

    function "&"(L:row; R:slv) return row is
        variable result : row(L'range)(L'left+1 downto L'right);
    begin
        for i in L'range loop result(i) := L(i) & R; end loop;
        return result;
    end function "&";

    function "&"(L:row; R:row) return row is
        variable result : row(L'range)(L(0)'length+R(0)'length downto 0);
    begin
        for i in L'range loop result(i) := L(i) & R(i); return result;
    end function "&";

    function "&"(L:row; R:matrix) return matrix is
        variable result : matrix(R'range)(R(0)'range)(L(0)'length+R(0)(0)'length downto 0);
    begin
        for i in rowRange loop result(i) := L(i) & R(i); end loop;
        return result;
    end function "&";

    function "&"(L:matrix; R:row) return matrix is
        variable result : matrix(L'range)(L(0)'range)(R(0)'length+L(0)(0)'length downto 0);
    begin
        for i in rowRange loop result(i) := L(i) & R(i); end loop;
        return result;
    end function "&";

    function "&"(L:matrix; R:matrix) return matrix is
        variable result : matrix(L'range)(L(0)'range)(L(0)(0)'length+R(0)(0)'length);
    begin
        for i in L'range loop result(i) := L(i) & R(i); end loop;
        return result;
    end function "&";

    ---------------------------------------------

    function concat(L:row; R:row) return row is
        variable result : row(L'length+R'length downto 0)(L(0)'range);
    begin
        for i in R'range loop result(i) := R(i); end loop;
        for i in L'range loop result(i+R'length) := L(i) end loop;
        return result;
    end function concat;

    function concat(L:matrix; R:matrix) return matrix is
    -- TODO check if this function is correct
        variable result : matrix(L'length+R'length downto 0)(L(0)'range)(L(0)(0)'range);
    begin
        for i in R'range loop result(i) := R(i); end loop;
        for i in L'range loop result(i+R'length) := L(i) end loop;
        return result;
    end function concat;

-------------------------------------------------------------------------------------------------

    -- buffer handling functions
    -- TODO if necessary

-------------------------------------------------------------------------------------------------
end n2oblife_pkg;
-------------------------------------------------------------------------------------------------