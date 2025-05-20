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
-- P_Process_Name: Process and procedures
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
    subtypes str    is string;
    subtypes char   is character;
    subtypes cst    is constant;
    subtypes sig    is signal;

    -- Data strcuture
    type buffer is array (nat <range>) of sl;
    type row    is array (nat <range>) of slv;
    type matrix is array (nat <range>) of row;


-------------------------------------------------------------------------------------------------

    -- Type conversion 
    function sgn        (vec    : slv)                  return sgn;
    function usgn       (vec    : slv)                  return usgn;
    function slv        (usig   : usgn)                 return slv;
    function sgn        (usig   : usgn)                 return sgn;
    function to_int     (usig   : usgn)                 return int;
    function slv        (sig    : sgn)                  return slv;
    function usgn       (sig    : sgn)                  return usgn;
    function to_int     (sig    : sgn)                  return int;
    function to_sgn     (integ  : int; length : int)    return sgn;
    function to_usgn    (integ  : int; length : int)    return usgn;
    
    
    -- Operators overloading (return size handled), element wise operations
    -- TODO, add relations operators (= and /=)
    function "+"    (L:slv;     R:int)      return slv;
    function "-"    (L:slv;     R:int)      return slv;
    ---------------------------------------------
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
    
    -- Returns the bit width of i as a positive integer
    function BitWidth (i:positive) return positive; 

-------------------------------------------------------------------------------------------------
    ---------------------------------------------
    -- Avalon Streaming Interface
    ---------------------------------------------
    type t_ASI is record
        -- fundamental signals
        -- INPUTS
        valid   : sl;   -- valid signal from Avalon Streaming Protocol
        data    : slv;  -- data source -> sink
        channel : slv;  -- the channel number for data being transferred
        -- OUTPUTS
        ready   : sl;   -- Asserts high to indicate that the sink can accept data
        error   : slv;  -- bit mask to mark errors affecting the data
        -- packet transfer signals
        -- INPUTS
        sop     : sl;   -- start of packet signal from Avalon Streaming Protocol
        eop     : sl;   -- end of packet signal from Avalon Streaming Protocol
        empty   : slv;  -- Indicates the number of symbols that are empty
    end record t_ASI;    

    ---------------------------------------------
    -- APB Interface for multiple IPs
    ---------------------------------------------
    -- Parameters
    type t_APB_prop is record
        BUS_NB           : int :=  1;
        ADDR_LENGTH      : int :=  8;
        DATA_LENGTH      : int := 32;
    end record t_APB_prop;

    -- natural range of BUS_NB with slv
    type t_apb_bus_ctrl     is array (nat <range>) of sl; 
    type t_apb_bus_addr     is array (nat <range>) of slv; 
    type t_apb_bus_data     is array (nat <range>) of slv;
    type t_apb_bus_pstb     is array (nat <range>) of slv(3 downto 0);
    type t_apb_bus_pprot    is array (nat <range>) of slv(2 downto 0);

    type t_APB is record
        -- CONTROL
        pclk        : sl;               -- APB clock
        preset_n    : sl;               -- APB reset active low
        -- WRITE
        paddr     : t_apb_bus_addr;   -- APB address
        pprot     : t_apb_bus_pprot;  -- APB protection 
                                            -- (xx0 : non-priviliged, xx1 : priviliged, 
                                            --  x0x : non-secure, x1x : secure, 
                                            --  0xx : data access, 1xx : instruction access)          
        psne        : t_apb_bus_ctrl;   -- APB extension to protection type
        pselx       : t_apb_bus_ctrl;   -- APB select when transfer required
        penable     : t_apb_bus_ctrl;   -- APB enable
        pwrite      : t_apb_bus_ctrl;   -- APB write (0:read / 1:write)
        pwdata      : t_apb_bus_data;   -- APB write data (8, 16 or 32 bits)
        pstrb       : t_apb_bus_pstb;   -- APB strobe which byte lanes to update
        -- READ  
        pready      : t_apb_bus_ctrl;   -- APB ready signal from slave
        prdata      : t_apb_bus_data;   -- APB data from slave
        pslverr     : t_apb_bus_ctrl;   -- APB slave error signal
        -- OTHERS
    end record t_APB;

    -- Operating states
    type t_apb_states is (APB_IDLE, APB_SETUP, APB_ACCES, APB_RDY);

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

    -- -- Read on APB
    -- procedure p_apb_rd(

    -- );
    -- pc_avalon_mm_read : process(all)
	-- begin
	
	-- 	prdata	        <= x"beefcafe";
    --     s_params_bit    <= (others=>'0');
    --     s_reset         <= s_params_ctrl(0);
		
	-- 	case paddr is
    --         -- ctrl
    --         when x"00" =>	prdata	<= x"a4b114e4";
    --         when x"04" =>	prdata	<= x"00010000";
    --         when x"08" =>	prdata	<= s_params_bit;
    --         when x"0c" =>	prdata	<= s_params_ctrl;
            
    --         -- read monitoring
    --         when x"10" =>	prdata	<= std_logic_vector(to_unsigned(s_monito_cnt_rdreq,32));
    --         when x"14" =>	prdata	<= std_logic_vector(to_unsigned(s_monito_cnt_ntvrdreq,32));  
    --         when x"18" =>	prdata	<= std_logic_vector(to_unsigned(s_monito_cnt_rdvalid_last,32));
    --         when x"1c" =>	prdata	<= std_logic_vector(to_unsigned(s_monito_cnt_rdvalid,32));             
    --         -- write monitoring
    --         when x"20" =>	prdata	<= std_logic_vector(to_unsigned(s_monito_cnt_wrreq,32));
    --         when x"24" =>	prdata	<= std_logic_vector(to_unsigned(s_monito_cnt_ntvwrreq,32));
    --         when x"28" =>	prdata	<= std_logic_vector(to_unsigned(s_monito_cnt_dreq_last,32));
    --         when x"2c" =>	prdata	<= std_logic_vector(to_unsigned(s_monito_cnt_dreq,32));


    --         when others => null;
    --     end case;
	
	-- end process pc_avalon_mm_read;

    -- -- write on APB
	-- ps_avalon_mm_write : process(i_rst, apb_clock)
	-- begin
	
	-- 	if (i_rst = '1') then
		
	-- 		s_params_ctrl		<= x"00000000";
			
	-- 	elsif (rising_edge(apb_clock)) then
		
	-- 		s_params_ctrl		<= s_params_ctrl;
			
	-- 		if (penable = '1' and pwrite = '1' and psel = '1') then
	-- 			case paddr is

	-- 				when x"0c" =>	s_params_ctrl	<= pwdata;
                    
	-- 				when others =>	null;
	-- 			end case;
	-- 		end if;
			
	-- 	end if;

	-- end process ps_avalon_mm_write;
-------------------------------------------------------------------------------------------------
-- TESTBENCH
-------------------------------------------------------------------------------------------------
   
    ---------------------------------------------
    -- Image loading for testbench(.raw only)
    ---------------------------------------------  
    -- FILE & DUT
    type t_video_prop is record
        PIX_PER_CLK     : int;  -- number of pixels per clock
        PIX_WIDTH       : int;  -- number of bits per pixel
        SEL             : int;  -- number of select signals
        X_FP            : int;  -- number of front blanking pixels
        X_SIZE          : int;  -- number of pixels in a line
        X_BP            : int;  -- number of back blanking pixels
        Y_FP            : int;  -- number of front blanking lines
        Y_SIZE          : int;  -- number of lines in a frame
        Y_BP            : int;  -- number of back blanking lines
    end record t_video_prop;

    -- FILE
    type t_file_prop is record
        DATA_IN_PER_CLK     : int;      -- number of data per clock
        SIMU_PATH           : str;      -- := "../stimulus/";
        FILE_IN             : str;      -- := "file_in.raw";
        FILE_OUT            : str;      -- := "file_out.raw";
        MSB_FIRST           : sl;       -- tells if msb sent first
        DATA_DEPTH_IN       : int;      -- depth of in data
        DATA_DEPTH_OUT      : int;      -- depth of out data
        -- RD_WIDTH            : int;  -- := PIX_PER_CLK*PIX_DEPTH_IN;
        -- WR_WIDTH            : int;  -- := PIX_PER_CLK*PIX_DEPTH_OUT;
    end record t_file_prop;

    -- -- data in gen
    -- type t_video_sig is record
    --     clk                      : std_logic := '0';
    --     reset_n                  : std_logic;
    --     i_ready              : std_logic;
    --     o_ready           : std_logic;
    --     i_sop                   : std_logic;
    --     i_dval                  : std_logic;
    --     i_data                  : std_logic_vector(DUT_VIDEO_IN_BUS_WIDTH-1 downto 0);
    --     i_eop                   : std_logic;
    -- end record t_video_sig;

    -- File ctrl (for writing)
    type t_video_file_ctrl is record
        data_is_file_data       : sl := '1';
        img_wr_pix_byte_cnt     : int := 0;
        out_type_file           : str(1 to 4) := ".raw"; -- or ".bin"
        img_file_name           : str(1 to 15);
    end record t_video_file_ctrl;

    type TB_FILE_STATUS is (
        ST_INIT             ,
        ST_WAIT_NEW_IMG     ,
        ST_NEW_IMG_RX       ,
        ST_FILE_OPENED_OK   ,
        ST_FILE_OPENED_KO   ,
        ST_FILE_PROCESSING  ,
        ST_FILE_END_OK      ,
        ST_FILE_END_KO      ,
        ST_FILE_CLOSED      
    );

-------------------------------------------------------------------------------------------------
    
    -- generate ctrl signal for video read
    procedure p_sig_ctrl_rd_video_file_tb(
        clk         : sl;       -- reading clk
        clk_period  : time;     -- clk period
        asi_ctrl    : t_ASI;    -- ctr signal generated to read video
        frameHeight : int;      -- Height
        frameWidth  : int;      -- Width
        blanking_x  : int;      -- blanking between rows
        blanking_y  : int       -- blanking between frames
    );

    -- Read video file 
    procedure p_read_video_file_tb(
        clk             : sl;               -- reading clk
        clk_period      : time;             -- clk period
        tb_img_path     : str;              -- path to the image
        s_ctrl_rd_sig   : t_ASI;            -- ctrl signal generated to read video
        s_fread_sig     : t_ASI;            -- file read signals for simulation
        s_video_reset   : sl;               -- a video reset signal
        msb_first       : sl;               -- tells if msb sent first
        tb_frd_status   : TB_FILE_STATUS    -- checks read status
    );

    -- TODO adapt procedure for writing
    -- procedure p_write_video_file_tb(
    -- );

-------------------------------------------------------------------------------------------------

    -- APB Read / Write procedures inside of process
    procedure p_apb_rd_reg_tb(
        apb_inst    : t_APB                                          ;
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

    function "+" (L:slv; R:int) return slv is variable result : slv(L'range);
    begin result = slv(usgn(L)+R); return result; end function "+";

    function "-" (L:slv; R:int) return slv is variable result : slv(L'range);
    begin result = slv(usgn(L)-R); return result; end function "-";

    ---------------------------------------------

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

    function BitWidth(constant i: in positive) return positive is
    begin
        return positive(ceil(log2(real(i) + real(1))));  -- Add one to prevent errors when i = 2**n
    end function;

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
-- TEST BENCH
-------------------------------------------------------------------------------------------------
    
    -- generate ctrl signal for video read
    procedure p_sig_ctrl_rd_video_file_tb(
        clk         : sl;
        clk_period  : time;
        asi_ctrl    : t_ASI;
        frameHeight : int;
        frameWidth  : int;
        blanking_x  : int;
        blanking_y  : int
    ) is
    begin

        wait until rising_edge(clk);

        -- restet row fifos
        asi_ctrl.eop <= '1';
        wait for 1*clk_period;
        asi_ctrl.eop <= '0';
        wait for blanking_x*clk_period;    

        -- start new image
        asi_ctrl.sop <= '1';
        asi_ctrl.valid <= '1';
        wait for 1*clk_period;
        asi_ctrl.sop <= '0';

        -- ctrl video signals
        for i in 0 to frameHeight-1 loop
            for j in 0 to frameWidth-1 loop
                asi_ctrl.valid <= '1';
                wait for 1*clk_period;
            end loop;
                asi_ctrl.valid <= '0';
            wait for blanking_y*clk_period;    
        end loop;
            asi_ctrl.valid <= '0';

  end procedure p_sig_ctrl_rd_video_file_tb;

    -- Read video file 
    procedure p_read_video_file_tb(
        clk             : sl;               -- reading clk
        clk_period      : time;             -- clk period
        tb_img_path     : string;           -- path to the image
        s_ctrl_rd_sig   : t_ASI;            -- ctrl signal generated
        s_fread_sig     : t_ASI;            -- file read signals for simulation
        s_video_reset   : sl;               -- a video reset signal
        msb_first       : std_logic;        -- tells if msb sent first
        tb_frd_status   : TB_FILE_STATUS    -- checks read status
    ) is
        type char_file is file of character;
        file img_file           : char_file;
        variable open_status    : FILE_OPEN_STATUS;
        variable rd_data        : character;
        variable s_img_pix      : std_logic_vector(s_fread_sig.data'length downto 0);
    begin
        
        -- wait tb start is 1st img
        if (s_video_reset ='1') then
            wait until s_video_reset ='0';
        end if;
        
        tb_frd_status <= ST_WAIT_NEW_IMG;
        
        -- wait new img
        if (s_ctrl_rd_sig.sop='0') then
            wait until s_ctrl_rd_sig.sop='1';
        end if;
        
        report "New image READ";
        tb_frd_status <= ST_NEW_IMG_RX;
        
        -- open image
        file_open(open_status, img_file, tb_img_path,  read_mode);
        
        -- check  image status
        if open_status /= open_ok then
            report "IMG_IN file not opened" severity failure;
            tb_frd_status <= ST_FILE_OPENED_KO;
        else
            report "IMG_IN file opened ";
            tb_frd_status <= ST_FILE_OPENED_OK;
        end if;
        
        -- wait sof end
        if (s_ctrl_rd_sig.sop='1') then
            wait until s_ctrl_rd_sig.sop='0';
        end if;    
        
        -- read image 
        -- loop
        while (s_ctrl_rd_sig.sop='0') and (tb_frd_status /= ST_FILE_END_OK) loop
            -- axi stream requires video data
            if (s_ctrl_rd_sig.valid='1') then-- and s_axis_tready='1') then
                tb_frd_status <= ST_FILE_PROCESSING;
                
                -- MSB FIRST FILE READ
                if (msb_first='1') then
                    for pix_bus_index in (s_fread_sig.data'length/8 - 1) downto 0 loop
                        -- read 1 byte
                        if not endfile(img_file) then
                            read(img_file, rd_data);
                            s_img_pix( ((pix_bus_index+1)*8)-1 downto ((pix_bus_index)*8) ) := std_logic_vector(to_unsigned(natural(character'pos(rd_data)), 8));
                        else
                            report "missing pixel msb" severity failure;
                        end if;
                    end loop;
                
                -- LSB FIRST FILE READ
                else
                    for pix_bus_index in 0 to (s_fread_sig.data'length/8 - 1) loop
                        -- read 1 byte
                        if not endfile(img_file) then
                            read(img_file, rd_data);
                            s_img_pix( ((pix_bus_index+1)*8)-1 downto ((pix_bus_index)*8) ) := std_logic_vector(to_unsigned(natural(character'pos(rd_data)), 8));
                        else
                            report "missing pixel msb" severity failure;
                        end if;
                    end loop;
                    
                end if;
                
                -- push every signal for simulation
                s_fread_sig <= s_ctrl_rd_sig;
                s_fread_sig.data <= s_img_pix;
            
            -- no data required, wait next clk
            else
                
            end if;
            
            -- check if file is empty
            if endfile(img_file) then
                report "ALL pixel read OK";
                tb_frd_status <= ST_FILE_END_OK;
            -- else
            --     report "NOK ALL pixel read NOK";
            --     tb_frd_status <= ST_FILE_END_KO;
            end if;
            
            wait until rising_edge(clk);
            
        end loop;
        
        report "Image Read end";
        
        -- fermeture du fichier
        file_close(img_file);
        wait for 0.1 *clk_period;
        
        tb_frd_status <= ST_FILE_CLOSED;
        
        -- loop end
        report "Image closed";
        
    end procedure p_read_video_file_tb;

    -- procedure p_write_video_file_tb(
    --     tb_fwr_status               : TB_FILE_STATUS
    --     tb_img_wr_pix_byte_cnt      : integer;
    -- ) is
    --     type char_file is file of character;
    --     file img_file_BT            : char_file;
    --     variable open_status_BT     : FILE_OPEN_STATUS;
    --     variable wr_data            : character;
    --     variable s_img_pix          : std_logic_vector(FILE_VIDEO_WR_WIDTH-1 downto 0);        
    --     variable img_file_name_BT   : string(1 to 8+1+2+4);   -- img_16b_ + _ + frameNb + ext
    --     variable s_vin_frame_cnt    : integer := 0;
    -- begin
    --     -- attention au blanking, le meme SOF est utilé en in et en ou du DUT
    --     -- il faut au moins 3 lignes de blancking enfin d'image
        
    --     tb_img_wr_pix_byte_cnt <= 0;
    --     wait for 1 ns;
        
    --     tb_fwr_status <= ST_WAIT_NEW_IMG;
        
    --     -- wait new img
    --     if (s_fwrite_sop='0') then
    --         wait until s_fwrite_sop='1';
    --     end if;
        
    --     tb_fwr_status <= ST_NEW_IMG_RX;
        
    --     -- generation nom image a ecrire
    --     if (s_vin_frame_cnt<10) then
    --         img_file_name_BT := "img_BTi_" &  "_" & "0"&integer'image(s_vin_frame_cnt) & tb_out_file_ext_raw;
    --         report "img_file_name_BT = |" & img_file_name_BT & "|";
            
    --     else
    --         img_file_name_BT := "img_BTi_" &  "_" & integer'image(s_vin_frame_cnt) & tb_out_file_ext_raw;
    --         report "img_file_name_BT = |" & img_file_name_BT & "|";
            
    --     end if;
        
    --     tb_dbg_img_file_name <= img_file_name_BT;
        
    --     -- ouverture de l'image
    --     file_open(open_status_BT, img_file_BT, TB_SIMU_PATH&img_file_name_BT, write_mode);
        
    --     -- check  image status
    --     if ( (open_status_BT /= open_ok) ) then
    --         report "IMG_OUT file not opened" severity failure;
    --         tb_fwr_status <= ST_FILE_OPENED_OK;
    --     else
    --         report "IMG_OUT file open ok ";
    --         tb_fwr_status <= ST_FILE_OPENED_KO;
    --     end if;
        
    --     -- wait sof end
    --     if (s_fwrite_sop='1') then
    --         wait until s_fwrite_sop='0';
    --     end if; 
        
    --     -- check DUT output and write data
    --     while (s_fwrite_sop='0') loop
    --         -- wait clock
    --         wait until rising_edge(s_video_clk);
            
    --         -- check data
    --         if (s_fwrite_valid='1') then -- and m_axis_tready='1') then
            
    --             tb_fwr_status <= ST_FILE_PROCESSING;
            
    --             -- for pix_bus_index in 0 to (C_PIX_DEPTH/8 - 1) loop
    --             for pix_bus_index in (FILE_VIDEO_WR_WIDTH/8 - 1) downto 0 loop
    --                 wr_data :=  character'val(to_integer(unsigned(
    --                                 s_fwrite_data( ((pix_bus_index+1)*8)-1 downto ((pix_bus_index)*8) )
    --                             )));
    --                 write(img_file_BT, wr_data);
                    
    --                 tb_img_wr_pix_byte_cnt <= tb_img_wr_pix_byte_cnt + 1;
    --             end loop;
    --         end if;
                    
    --     end loop;
        
    --     report "Image write end";
    --     tb_fwr_status <= ST_FILE_END_OK;
        
    --     -- fermeture du fichier
    --     file_close(img_file_BT);
    --     wait for 1 ns;
        
    --     -- loop end
    --     report "Image closed";
        
    --     tb_fwr_status <= ST_FILE_CLOSED;
        
    --     -- inc img count
    --     s_vin_frame_cnt := s_vin_frame_cnt+1;

    -- end procedure p_write_video_file_tb;

-------------------------------------------------------------------------------------------------

    -- -- Procedure Read / Write for APB Interface
    -- procedure p_apb_rd_reg_tb(
    --     apb_inst    : t_APB                                         ;
    --     apb_id      : integer                                       ;
    --     reg_addr    : std_logic_vector(C_APB_ADDR_LENGTH-1 downto 0);
    --     apb_status  : t_apb_states
    --     ) is
    --     begin
    --         -- test bus status
    --         if (apb_status/=APB_RDY) then
    --             report "APB Error" severity error;
    --         end if;
            
    --     ---------------------------------------------
    --     -- READ Transfer – Without Wait States
    --     -- T1 = setup phase
    --     apb_inst.i_pwrite(apb_id) <= '0';
    --     apb_inst.i_pselx (apb_id) <= '1';
    --     apb_inst.i_paddr (apb_id) <= reg_addr;
    --     wait until rising_edge(apb_inst.pclk);
        
    --     apb_status <= APB_IDLE;
        
    --     -- T2 = access phase
    --     apb_inst.i_penable(apb_id) <= '1';
    --     -- PREADY not used
    --     wait until rising_edge(apb_inst.pclk);
        
    --     apb_status <= APB_SETUP;
        
    --     -- T3 = read transfer
    --     apb_inst.apb_rd_data <= apb_inst.APB_PRDATA(apb_id);
    --     wait until rising_edge(apb_inst.pclk);
        
    --     apb_status <= APB_ACCES;
        
    --     -- End transfert
    --     apb_inst.i_pselx   (apb_id) <= '0';
    --     apb_inst.i_penable(apb_id)  <= '0';
        
    --     apb_status <= APB_RDY;
    --     wait until falling_edge(apb_inst.pclk);
        
    -- end procedure p_apb_rd_reg_tb;


    -- procedure p_apb_wr_reg_tb(
    --     apb_inst    : t_APB                                         ;
    --     apb_id      : integer                                       ;
    --     reg_addr    : std_logic_vector(C_APB_ADDR_LENGTH-1 downto 0);
    --     reg_data    : std_logic_vector(C_APB_DATA_LENGTH downto 0)  ;
    --     apb_status  : t_apb_states
    -- ) is
    -- begin
    --     -- test bus status
    --     if (apb_status/=APB_RDY) then
    --         report "APB Error" severity error;
    --     end if;
    
    --     -- WRITE Transfer – Without Wait States
    --     ---------------------------------------------
    --     -- T1 = write transfer setup
    --     apb_inst.i_pwrite(apb_id) <= '1';
    --     apb_inst.i_pselx (apb_id) <= '1';
    --     apb_inst.i_paddr (apb_id) <= reg_addr;
    --     apb_inst.o_pdata(apb_id) <= reg_data;    
    --     wait until rising_edge(apb_CLOCK);
        
    --     apb_status <= APB_IDLE;
        
    --     -- T2 = access phase
    --     apb_inst.i_penable(apb_id) <= '1';
    --     -- PREADY not used
    --     wait until rising_edge(apb_CLOCK);

    --     apb_status <= APB_SETUP;

    --     -- T3 = write transfer
    --     -- APB_PWDATA(apb_id) <= reg_data;    
    --     wait until rising_edge(apb_CLOCK);
        
    --     apb_status <= APB_ACCES;
        
    --     -- End transfert
    --     apb_inst.i_pselx   (apb_id) <= '0';
    --     apb_inst.i_penable(apb_id)  <= '0';
        
    --     apb_status <= APB_RDY;
    --     wait until falling_edge(apb_CLOCK);
        
    -- end p_apb_wr_reg_tb;

-------------------------------------------------------------------------------------------------
end n2oblife_pkg;
-------------------------------------------------------------------------------------------------