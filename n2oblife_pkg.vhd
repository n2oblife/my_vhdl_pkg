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
    ---------------------------------------------
    -- Avalon Streaming Interface
    ---------------------------------------------
    type t_ASI is record
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

    ---------------------------------------------
    -- APB Interface for multiple IPs
    ---------------------------------------------
    -- Parameters
    constant C_APB_BUS_NB           : integer :=  1;
    constant C_APB_ADDR_LENGTH      : integer :=  8;
    constant C_APB_DATA_LENGTH      : integer := 32;

    type t_apb_bus_addr     is array (0 to C_APB_BUS_NB-1) of std_logic_vector(C_APB_ADDR_LENGTH-1 downto 0); 
    type t_apb_bus_ctrl     is array (0 to C_APB_BUS_NB-1) of std_logic; 
    type t_apb_bus_data     is array (0 to C_APB_BUS_NB-1) of std_logic_vector(C_APB_DATA_LENGTH-1 downto 0);
    type t_apb_bus_pstb     is array (0 to C_APB_BUS_NB-1) of slv(3 downto 0);
    type t_apb_bus_pprot    is array (0 to C_APB_BUS_NB-1) of slv(2 downto 0);

    type t_APB is record
        -- CONTROL
        pclk        : sl;               -- APB clock
        preset      : sl;               -- APB reset
        -- INPUTS
        i_paddr     : t_apb_bus_addr;   -- APB address
        i_pdata     : t_apb_bus_data;   -- APB data
        i_pwrite    : t_apb_bus_ctrl;   -- APB write (0:read / 1:write)
        i_pstb      : t_apb_bus_pstb;   -- APB strobe
        i_pselx      : t_apb_bus_ctrl;  -- APB select
        i_penable   : t_apb_bus_ctrl;   -- APB enable
        i_pprot     : t_apb_bus_pprot;  -- APB protection 
                                            -- (xx0 : non-priviliged, xx1 : priviliged, 
                                            --  x0x : non-secure, x1x : secure, 
                                            --  0xx : data access, 1xx : instruction access)      
        -- OUTPUTS  
        o_pdata     : t_apb_bus_data;   -- APB data from slave
        o_pready    : t_apb_bus_ctrl;   -- APB ready signal from slave
        o_pslverr   : t_apb_bus_ctrl;   -- APB slave error signal
    end record APB;

    -- Operating states
    type t_apb_states is (IDLE, SETUP, ACCES);

    -- Testbench status
    type t_apb_status is (
        APB_ST1     ,
        APB_ST2     ,
        APB_ST3     ,
        APB_ST4     ,
        APB_ST5     ,
        APB_ST_RDY  
    );

-------------------------------------------------------------------------------------------------

    ---------------------------------------------
    -- Image loading for testbench(.raw only)
    ---------------------------------------------  
    -- FILE & DUT
    constant TB_C_VIDEO_PIX_PER_CLK     : integer :=    1;
    constant TB_C_VIDEO_PIX_WIDTH       : integer :=   10;
    constant TB_C_VIDEO_SEL             : integer :=    0;
    constant TB_C_VIDEO_X_FP            : integer :=   16;
    constant TB_C_VIDEO_X_SIZE          : integer :=  780;
    constant TB_C_VIDEO_X_BP            : integer :=   16;
    constant TB_C_VIDEO_Y_FP            : integer :=    0;
    constant TB_C_VIDEO_Y_SIZE          : integer :=  640;
    constant TB_C_VIDEO_Y_BP            : integer :=    1;

    -- FILE
    constant TB_SIMU_PATH               : string := "../stimulus/";
    constant TB_IMG_IN_MSB_FIRST        : std_logic := '0';
    constant TB_IMG_IN_0                : string := "img.raw";
    -- constant TB_PATH_IMG_1              : string := "../stimulus/";
    -- constant TB_IMG_OUT_PATH            : string := "";
    constant FILE_PIX_DEPTH_IN          : integer := 16;
    constant FILE_PIX_DEPTH_OUT         : integer := 8;
    constant FILE_VIDEO_RD_WIDTH        : integer := TB_C_VIDEO_PIX_PER_CLK*FILE_PIX_DEPTH_IN;
    constant FILE_VIDEO_WR_WIDTH        : integer := TB_C_VIDEO_PIX_PER_CLK*FILE_PIX_DEPTH_OUT;

-------------------------------------------------------------------------------------------------

    -- Reset functions
    function rst_array(in_array : row)      return row;
    function rst_matrix(in_mat : matrix)    return matrix;

    -- Buffer handling functions (FIFO and LIFO ?)
    -- to handle multiple output, create a type with each output type
    -- function push(elt : sl;     in_row : buffer)    return buffer;
    -- function push(elt : slv;    in_row : row)       return row;
    -- function f_pop(in_row : buffer)
    -- function l_pop(in_row : buffer)

-------------------------------------------------------------------------------------------------

    -- APB Read / Write procedures inside of process
    procedure p_apb_rd_reg_tb(
        apb_inst    : t_APB                                           ;
        apb_id      : integer                                       ;
        reg_addr    : std_logic_vector(C_APB_ADDR_LENGTH-1 downto 0);
        apb_status  : t_apb_status
    );

    procedure p_apb_wr_reg(
        apb_inst    : t_APB;
        apb_id      : int;
        reg_addr    : slv(C_APB_ADDR_LENGTH-1 downto 0);
        reg_data    : slv(C_APB_DATA_LENGTH-1 downto 0);
        apb_status  : t_apb_status
    );

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
        for i in L'range loop result(i+R'length) := L(i); end loop;
        return result;
    end function concat;

-------------------------------------------------------------------------------------------------

    -- Reset functions
    function rst_array(in_array : row) return row is
    begin
        for i in in_array'range loop in_array(i) <= (others => '0'); end loop;
        return in_array;
    end function;

    function rst_matrix(in_mat : matrix) return matrix is
    begin
        for i in in_mat'range loop in_mat(i) <= rst_array(in_mat(i)); end loop;
        return in_mat;
    end function;

-------------------------------------------------------------------------------------------------

    -- buffer handling functions
    -- TODO if necessary

-------------------------------------------------------------------------------------------------

    -- Procedure Read / Write for APB Interface
    
    procedure p_apb_rd_reg_tb(
        apb_inst    : t_APB                                           ;
        apb_id      : integer                                       ;
        reg_addr    : std_logic_vector(C_APB_ADDR_LENGTH-1 downto 0);
        apb_status  : t_apb_status
        ) is
        begin
            -- test bus status
            if (apb_status/=APB_ST_RDY) then
                report "APB Error" severity error;
            end if;
            
        ---------------------------------------------
        -- READ Transfer – Without Wait States
        -- T1 = setup phase
        apb_inst.i_pwrite(apb_id) <= '0';
        apb_inst.i_pselx (apb_id) <= '1';
        apb_inst.i_paddr (apb_id) <= reg_addr;
        wait until rising_edge(apb_inst.pclk);
        
        apb_status <= APB_ST1;
        
        -- T2 = access phase
        apb_inst.i_penable(apb_id) <= '1';
        -- PREADY not used
        wait until rising_edge(apb_inst.pclk);
        
        apb_status <= APB_ST2;
        
        -- T3 = read transfer
        apb_inst.apb_rd_data <= apb_inst.APB_PRDATA(apb_id);
        wait until rising_edge(apb_inst.pclk);
        
        apb_status <= APB_ST3;
        
        -- End transfert
        apb_inst.i_pselx   (apb_id) <= '0';
        apb_inst.i_penable(apb_id)  <= '0';
        
        apb_status <= APB_ST_RDY;
        wait until falling_edge(apb_inst.pclk);
        
    end procedure p_apb_rd_reg_tb;


    procedure p_apb_wr_reg_tb(
        apb_inst    : t_APB                                           ;
        apb_id      : integer                                       ;
        reg_addr    : std_logic_vector(C_APB_ADDR_LENGTH-1 downto 0);
        reg_data    : std_logic_vector(C_APB_DATA_LENGTH downto 0)                 ;
        apb_status  : t_apb_status
    ) is
    begin
        -- test bus status
        if (apb_status/=APB_ST_RDY) then
            report "APB Error" severity error;
        end if;
    
        -- WRITE Transfer – Without Wait States
        ---------------------------------------------
        -- T1 = write transfer setup
        apb_inst.i_pwrite(apb_id) <= '1';
        apb_inst.i_pselx (apb_id) <= '1';
        apb_inst.i_paddr (apb_id) <= reg_addr;
        apb_inst.o_pdata(apb_id) <= reg_data;    
        wait until rising_edge(apb_CLOCK);
        
        apb_status <= APB_ST1;
        
        -- T2 = access phase
        apb_inst.i_penable(apb_id) <= '1';
        -- PREADY not used
        wait until rising_edge(apb_CLOCK);

        apb_status <= APB_ST2;

        -- T3 = write transfer
        -- APB_PWDATA(apb_id) <= reg_data;    
        wait until rising_edge(apb_CLOCK);
        
        apb_status <= APB_ST3;
        
        -- End transfert
        apb_inst.i_pselx   (apb_id) <= '0';
        apb_inst.i_penable(apb_id)  <= '0';
        
        apb_status <= APB_ST_RDY;
        wait until falling_edge(apb_CLOCK);
        
    end p_apb_wr_reg_tb;
-------------------------------------------------------------------------------------------------
end n2oblife_pkg;
-------------------------------------------------------------------------------------------------