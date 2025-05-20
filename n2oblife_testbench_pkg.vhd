-------------------------------------------------------------------------------------------------
-- Company : ...
-- Author : nooblife
-- Licensing : MIT
-------------------------------------------------------------------------------------------------
-- Version : V1
-- Version history :
-- V1 : 05-04-2024 : nooblife: Creation
-- V2 : 20-05-2025 : split in different files
-------------------------------------------------------------------------------------------------
-- File name : n2oblife_testbench_pkg.vhd
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
use n2oblife_pkg.all;


-------------------------------------------------------------------------------------------------
------------------------------------------ DECLARATION ------------------------------------------
-------------------------------------------------------------------------------------------------
package n2oblife_testbench_pkg is
-- Declaration of
    -- types and subtypes
    -- subprograms
    -- constants, signals etc.

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
end n2oblife_testbench_pkg;
-------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------
--------------------------------------------- BODY ----------------------------------------------
-------------------------------------------------------------------------------------------------
package body n2oblife_testbench_pkg is
    
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
end n2oblife_testbench_pkg;
-------------------------------------------------------------------------------------------------