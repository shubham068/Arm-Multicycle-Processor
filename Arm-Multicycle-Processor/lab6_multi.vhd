Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;

entity Controller is
    port (
		instr: in std_logic_vector(31 downto 0);
        flag_in: in std_logic_vector(3 downto 0);
 
        Wsrc,PW,IorD,MR,MW,IW,DW,M2R,Rsrc,RW,AW,BW,SW,Asrc1,Fset,resW, Fsrc, Bsrc,Rsrc1:out STD_LOGIC;
        clk,reset:in std_logic;
   
        
        --Fsrc=Flag source, 1 for shifter and multiplier, 0 for ALU 
        --Bsrc=B register data source, 1 for multiplier, 0 for RF
        --Rsrc1=Read address on port 1. 1 for Shift amount, multiplier, 0 otherwise.


        --Ssrc:out std_logic_vector(1 downto 0);

        --Ssrc= 00/01/10/11

-- Updated Code after adding proper memory. 
-- New signals-mOff(2 bit)== no of bit to position to start reading/writing within word,
-- last two bits of address,mOpc->type of extension,
-- 000 for normal ldr and instruction load, 101 for normal str. --
        --

        Ssrc,mOff:out STD_LOGIC_VECTOR(1 downto 0); 
        Asrc2,mOpc:out STD_LOGIC_VECTOR(2 downto 0); 

		opcode_out: out std_logic_vector(3 downto 0)
		);
end entity Controller;

architecture behavioural of Controller is


    component Bctrl is
        port (
            ins: in std_logic_vector(3 downto 0);
            flags: in std_logic_vector(3 downto 0);
            p: out std_logic
            );
    end component;

    type state is (fetch,fetch_1,fetch_2,rdAB,arith,addr,brn,wrRF,wrM,wrM1,wrM2,rdM,rdM1,rdM2,M2RF,mul);
    signal n_s,c_s:state;
    signal instr_a: std_logic_vector(6 downto 0):=instr(27 downto 21);
    signal instr_b: std_logic_vector(3 downto 0):=instr(31 downto 28);
    signal p: std_logic;

    begin

        instr_b<=instr(31 downto 28);
        instr_a<=instr(27 downto 21);

        --Acontroller: Actrl port map (instr_a,opcode_out);
        Bcontroller: Bctrl port map (instr_b,flag_in,p);


        state_register : Process(clk)
        begin
            if(Rising_edge(clk)) then
                c_s <= n_s;
            end if;
        end Process;


        signalGen : Process(c_s,instr,reset)

        begin
        if(reset='0') then
                case c_s is
                    when fetch =>
                        n_s  <= fetch_1;
                        
                        PW    <= '0'; 	
                        IW    <= '1';
                        DW    <= '0';
                        AW    <= '0'; 
                        BW    <= '0';
                        Wsrc <= '0';
                        Fset <= '0';
                        RW   <= '0';
                        MR   <= '1';
                        MW   <= '0';
                        IorD <= '0';	
                        Asrc1<= '0';	  
                        M2R  <= '0';	    
                        Rsrc <= '0';		   
                        SW   <= '0';    Bsrc  <= '0';    Rsrc1  <= '0';  Fsrc <= '0';
                        resW <= '0';
                        opcode_out<="0100";
                        Asrc2 <= "001";	Ssrc  <= "00";
                        mOpc  <= "000";
         ---------------------------------------------------------------------------------
                    when fetch_1=>
                        n_s<= fetch_2;
                    when fetch_2=>
                        n_s<= rdAB;
                        PW<='1';    
         
         ----------------------------------------------------------------------------------------------               
                    when rdAB =>
                        if(instr(27 downto 26) = "00") then
                            if(instr(7 downto 4) = "1001") then n_s <= mul;
                            else n_s <= arith;
                            end if;
                        elsif (instr(27 downto 26) = "10") then n_s <= brn;
                        elsif (instr(27 downto 26) = "01") then n_s <= addr;
                        end if;
        
                        PW    <= '0'; 	
                        IW    <= '0';
                        -- DW    <= '0';
                        
                        AW    <= '1'; 
                        BW    <= '1';
        
                        -- Fset <= '0';
                        -- RW   <= '0';
                        MR   <= '0';
                        -- MW   <= '0';
                        -- IorD <= '0';	
                        -- Asrc1<= '0';	  
                        -- M2R  <= '0';	    
                        Rsrc <= '0';		   
                        -- Bsrc  <= '0'; 
                        -- Rsrc1 <= '0';  
                       
                        -- Asrc2 <= "001";	Ssrc  <= "00";
        
        ---------------------------------------------------------------------------                
        
                    when arith =>
                        n_s <= wrRF;
        
                        if(instr(25) = '1') then
                                rsrc <= '0';
                                SW   <= '0';
                                Fsrc <= '0';
                                Asrc2<= "111";
                                -- Immediate
                        else 
                            Asrc2<= "000";
                            if(instr(7)='1' and instr(4)='1') then
                                if(instr(6 downto 5)="00") then
                                    -- -- MUL/MLA 
                                    -- -- Here Rd,Rn are interchanged?
                                    -- rsrc <= '1';
                                    -- SW   <= '1';
                                    -- Fsrc <= '1';
                                    -- Bsrc <= '1';
                                else
                                    --Halfword DT/reg offset.
                                    rsrc <= '1';
                                    SW   <= '1';
                                    Fsrc <= '1';
                                    Bsrc <= '0';
                                end if;
                            elsif(instr(4)='1') then
                                --DP SHftammount reg
                            elsif(instr(4)='0') then
                                -- " immd
                            else
                                --nothing // DP immediate
                            end if;
        
                            
                        end if;
                        
                        -- PW    <= '1'; 	
                        -- IW    <= '1';
                        -- DW    <= '0';
                        AW    <= '0'; 
                        BW    <= '0';
        
                        
                        -- RW   <= '0';
                        -- MR   <= '1';
                        -- MW   <= '0';
                        -- IorD <= '0';	
                        Asrc1<= '1';	  
                        -- M2R  <= '0';	    
                        -- Rsrc <= '0';
                        resW <= '1';	
                        Fset <= p;	
                        
                        --Asrc2 <= "000";	
                        --Ssrc  <= "00";
                        opcode_out <= instr(24 downto 21);
        ---------------------------------------------------------------------------
                    when wrRF =>
                        n_s <= fetch;
                                        
                        -- PW    <= '1'; 	
                        -- IW    <= '1';
                        -- DW    <= '0';
                        -- AW    <= '0'; 
                        -- BW    <= '0';
        
                        -- Fset <= '0';
                        RW   <=  p ; -- output from Bctrl.
                        -- MR   <= '1';
                        -- MW   <= '0';
                        -- IorD <= '0';	
                        -- Asrc1<= '0';	  
                        M2R  <= '0';
                        resW <= '0';
                        Fset <= '0';	    
                        -- Rsrc <= '0';		   
                        -- SW   <= '0';    Bsrc  <= '0';    rsrc  <= '0';
                        
                        -- Asrc2 <= "001";	Ssrc  <= "00";
        
        
         -------------------------------------------------------------------------------           
                    when addr =>
                        if(instr(20)='0') then n_s <= wrM;
                        else 
                            n_s <= rdM;
                            IorD <='1';
                        end if;
                        ResW <=instr(21);
                        Wsrc<='1';               
                        -- PW    <= '1'; 	
                        -- IW    <= '1';
                        -- DW    <= '0';
                        AW    <= '0'; 
                        BW    <= '1';
        
                        -- Fset <= '0';
                        -- RW   <= '0';
                        -- MR   <= '1';
                        -- MW   <= '0';
                        -- IorD <= '0';	
                        Asrc1<= '1';	  
                        -- M2R  <= '0';	    
                        Rsrc <= '1';
                        Rsrc1 <= '0';		   
                        -- Bsrc <= '0';
                        
                        resW <= '1';
                        
                        Asrc2 <= "010";	Ssrc  <= "00";
        
                        if(instr(25) = '1') then
                            --reg offset
                            rsrc <= '1';
                            SW   <= '1';
                            Fsrc <= '1';
                        else
                            --immed offset.
                        end if;
                        
        -------------------------------------------------------------------------------            
                    when wrM1 =>
                                n_s <= wrM2;
                                ResW<='0';
                                RW <= instr(21);
                    when wrM2 =>
                                n_s <= fetch;
                                RW<='0';
                                Wsrc<='0';                                
                    when wrM =>
                        n_s <= wrM1;
                                        
                        -- PW    <= '1'; 	
                        -- IW    <= '1';
                        -- DW    <= '0';
                        -- AW    <= '0'; 
                        -- BW    <= '0';
        
                        -- Fset <= '0';
                        -- RW   <= '0';
                        MR   <= '1';
                        MW   <= p;
                        IorD <= '1';	
                        -- Asrc1<= '0';	  
                        -- M2R  <= '0';	    
                        -- Rsrc <= '0';		   
                        -- SW   <= '0';    Bsrc  <= '0';    rsrc  <= '0';
                        Mopc<="101";
                        Moff<="11";
                        -- Asrc2 <= "001";	Ssrc  <= "00";
                    when rdM1 => 
                        n_s <=  rdM2;
                        ResW<='0';
                        RW <= instr(21);
                    when rdM2 => 
                        n_s <=  M2RF;
                        RW<='0';
                        Wsrc<='0';
                    when rdM => 
                        n_s <=  rdM1;
                                        
                        -- PW    <= '1'; 	
                        -- IW    <= '1';
                        -- DW    <= '0';
                        -- AW    <= '0'; 
                        -- BW    <= '0';
        
                        -- Fset <= '0';
                        -- RW   <= instr(21);
                        ResW <=instr(21);
                        -- MR   <= '1';
                        -- MW   <= '0';
                        -- IorD <= '0';	
                        -- Asrc1<= '0';	  
                        -- M2R  <= '0';	    
                        -- Rsrc <= '0';		   
                        -- SW   <= '0';    Bsrc  <= '0';    rsrc  <= '0';
        
                        DW <= '1';
                        MR <= '1';
                        IorD <= '1';
                        
                        --Asrc2 <= "001";	Ssrc  <= "00";  
                    
                    when M2RF => 
                        n_s <=  fetch; 
                                        
                        -- PW    <= '1'; 	
                        -- IW    <= '1';
                        -- DW    <= '0';
                        -- AW    <= '0'; 
                        -- BW    <= '0';
        
                        -- Fset <= '0';
                        RW   <= p;
                        -- MR   <= '1';
                        -- MW   <= '0';
                        -- IorD <= '0';	
                        -- Asrc1<= '0';	  
                        M2R  <= '1';	    
                        -- Rsrc <= '0';		   
                        -- SW   <= '0';    Bsrc  <= '0';    rsrc  <= '0';
                        
                        -- Asrc2 <= "001";	Ssrc  <= "00";
                                    
                    when brn => 
                        n_s <=  fetch;
                        
                        -- PW    <= '1'; 	
                        -- IW    <= '1';
                        -- DW    <= '0';
                        AW    <= '0'; 
                        BW    <= '0';
        
                        -- Fset <= '0';
                        -- RW   <= '0';
                        -- MR   <= '1';
                        -- MW   <= '0';
                        -- IorD <= '0';	
                        -- Asrc1<= '1';	  
                        -- M2R  <= '0';	    
                        -- Rsrc <= '0';		   
                        -- SW   <= '0';    Bsrc  <= '0';    rsrc  <= '0';
                        Asrc1 <= '0';
                        PW <= p;
                        opcode_out <= "0100"; --ADD
                        
                        Asrc2 <= "011";
                        mOpc <= "000";
        
                        --	Ssrc  <= "00";
        
                    when mul =>
                        --n_s <= ;
                        Rsrc1 <= '1';
        
                end case;
             else
                n_s <=fetch;
             end if;
    end process;
end behavioural;

