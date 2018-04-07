---------------------------------------------------------------------------------- 
-- Company:  
-- Engineer:  
--  
-- Create Date: 02/01/2018 04:58:33 PM 
-- Design Name:  
-- Module Name: ALU - Behavioral 
-- Project Name:  
-- Target Devices:  
-- Tool Versions:  
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
use IEEE.STD_LOGIC_unsigned.ALL; 
entity ALU is 
    Port ( op1 : in STD_LOGIC_VECTOR (31 downto 0); 
           op2 : in STD_LOGIC_VECTOR (31 downto 0); 
           op3 : out STD_LOGIC_VECTOR (31 downto 0); 
           opcode : in STD_LOGIC_VECTOR (3 downto 0); 
           C_in:in STD_LOGIC; 
           C_out : out STD_LOGIC; 
           V : out STD_LOGIC; 
           N : out STD_LOGIC; 
           Z : out STD_LOGIC); 
end ALU; 
 
architecture Behavioral of ALU is 
signal result: STD_LOGIC_VECTOR(31 downto 0); 
signal c_31:STD_LOGIC; 
begin 
    with opcode select result<= 
        op1 and op2 when "0000"|"1000", 
        op1 xor op2 when "0001"|"1001", 
        op1+(not op2)+"1" when "0010"|"1010", 
        (not op1)+op2+"1" when "0011", 
        op1+op2 when "0100"|"1011", 
        op1+op2+C_in when "0101", 
        op1+(not op2)+C_in when "0110", 
        (not op1)+op2+C_in when "0111", 
        op1 or op2 when "1100", 
        op2 when "1101", 
        op1 and (not op2) when "1110",               
        (not op2) when others; 
op3<=result; 
c_31<=op1(31) xor op2(31) xor result(31); 
Z<='1' when result="00000000000000000000000000000000" else '0'; 
N<=result(31); 
V<= c_31 xor ((op1(31) and op2(31)) or (c_31 and (op1(31) or op2(31)))); 
C_out<=c_31; 
end Behavioral; 
library IEEE; 
use IEEE.STD_LOGIC_1164.ALL; 
use ieee.numeric_std.all; 
entity shifter is 
    Port( op1: in STD_LOGIC_VECTOR(31 downto 0); 
          op2: out STD_LOGIC_VECTOR(31 downto 0); 
          s_amt:in STD_LOGIC_VECTOR(4 downto 0); 
          opcode: in STD_LOGIC_VECTOR(1 downto 0); 
          C_out:out STD_LOGIC; 
          C_in:in STD_LOGIC); 
end shifter; 
 
Architecture Behavioral of shifter is 
signal act0,act1,act2,act3,act4,act5: STD_LOGIC_VECTOR(32 downto 0); 
signal fill0: STD_LOGIC_VECTOR(31 downto 0); 
signal fill1: STD_LOGIC_VECTOR(15 downto 0); 
signal fill2: STD_LOGIC_VECTOR(7 downto 0); 
signal fill3: STD_LOGIC_VECTOR(3 downto 0); 
signal fill4: STD_LOGIC_VECTOR(1 downto 0); 
signal carry: STD_LOGIC; 
begin 
    insert:for I in 0 to 31 generate 
       act0(I+1)<=op1(31-I) when opcode="00" else op1(I); 
    end generate; 
    act0(0)<=C_in; 
    with opcode & op1(31) select fill0<= 
        "00000000000000000000000000000000" when "001"|"011"|"000"|"010"|"100", 
        "11111111111111111111111111111111" when "101", 
        op1 when others; 
    act1<=fill0(15 downto 0) & act0(32 downto 16) when s_amt(4)='1' else act0; 
    fill1<=fill0(15 downto 0) when s_amt(4)='0' else fill0(31 downto 16);  
    act2<=fill1(7 downto 0) & act1(32 downto 8) when s_amt(3)='1' else act1; 
    fill2<=fill1(7 downto 0) when s_amt(3)='0' else fill1(15 downto 8); 
    act3<=fill2(3 downto 0) & act2(32 downto 4) when s_amt(2)='1' else act2; 
    fill3<=fill2(3 downto 0) when s_amt(2)='0' else fill2(7 downto 4); 
    act4<=fill3(1 downto 0) & act3(32 downto 2) when s_amt(1)='1' else act3; 
    fill4<=fill3(1 downto 0) when s_amt(1)='0' else fill3(3 downto 2); 
    act5<=fill4(0) & act4(32 downto 1) when s_amt(0)='1' else act4; 
    --op2<=act5(32 downto 1); 
    C_out<=act5(0); 
    extract:for I in 0 to 31 generate 
       op2(I)<=act5(32-I) when opcode="00" else act5(I+1); 
    end generate; 
     
end Behavioral;  
library IEEE; 
use IEEE.STD_LOGIC_1164.ALL; 
use IEEE.STD_LOGIC_unsigned.ALL; 
entity multiplier is 
    Port(op1:in STD_LOGIC_VECTOR(31 downto 0); 
         op2:in STD_LOGIC_VECTOR(31 downto 0); 
         op3:out STD_LOGIC_VECTOR(31 downto 0); 
         N,Z:out STD_LOGIC); 
end multiplier; 
 
 
Architecture Behavioral of multiplier is 
signal act:STD_LOGIC_VECTOR(31 downto 0); 
begin 
    act<=op1+op2; 
    N<=act(31); 
    Z<='1' when act="00000000000000000000000000000000" else '0'; 
    op3<=act;     
end Behavioral; 
 
library IEEE; 
use IEEE.STD_LOGIC_1164.ALL; 
entity reg_file is  
    Port(in1,pc_in:in STD_LOGIC_VECTOR(31 downto 0); 
        out1,out2,pc_out: out STD_LOGIC_VECTOR(31 downto 0); 
        r_ad1,r_ad2,w_ad: in STD_LOGIC_VECTOR(3 downto 0); 
        w_en,clk,reset, pcw:in STD_LOGIC); 
end reg_file; 
Architecture Behavioral of reg_file is 
signal r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15:STD_LOGIC_VECTOR(31 downto 0); 
begin 
    with r_ad1 select out1<= 
        r0 when "0000", 
        r1 when "0001", 
        r2 when "0010", 
        r3 when "0011",  
        r4 when "0100", 
        r5 when "0101", 
        r6 when "0110", 
        r7 when "0111", 
        r8 when "1000", 
        r9 when "1001", 
        r10 when "1010", 
        r11 when "1011",  
        r12 when "1100", 
        r13 when "1101", 
        r14 when "1110", 
        r15 when others; 
    with r_ad2 select out2<= 
            r0 when "0000", 
            r1 when "0001", 
            r2 when "0010", 
            r3 when "0011",  
            r4 when "0100", 
            r5 when "0101", 
            r6 when "0110", 
            r7 when "0111", 
            r8 when "1000", 
            r9 when "1001", 
            r10 when "1010", 
            r11 when "1011",  
            r12 when "1100", 
            r13 when "1101", 
            r14 when "1110", 
            r15 when others; 
    pc_out<=r15;         
    process(clk,reset) 
    begin 
    if(reset='1') then 
            r0<="00000000000000000000000000000000"; 
            r1<="00000000000000000000000000000000"; 
            r2<="00000000000000000000000000000000"; 
            r3<="00000000000000000000000000000000"; 
            r4<="00000000000000000000000000000000"; 
            r5<="00000000000000000000000000000000"; 
            r6<="00000000000000000000000000000000"; 
            r7<="00000000000000000000000000000000"; 
            r8<="00000000000000000000000000000000"; 
            r9<="00000000000000000000000000000000"; 
            r10<="00000000000000000000000000000000"; 
            r11<="00000000000000000000000000000000"; 
            r12<="00000000000000000000000000000000"; 
            r13<="00000000000000000000000000000000"; 
            r14<="00000000000000000000000000000000"; 
            r15<="00000000000000000000000000000000"; 
     else 
if(clk='1' and clk'event) then 
    if (pcw='1') then 
        r15<=pc_in; 
    end if; 
    if(w_en='1') then 
       case w_ad is 
            when "0000"=>r0<=in1; 
            when "0001"=>r1<=in1; 
            when "0010"=>r2<=in1; 
            when "0011"=>r3<=in1; 
            when "0100"=>r4<=in1; 
            when "0101"=>r5<=in1; 
            when "0110"=>r6<=in1;    
            when "0111"=>r7<=in1; 
            when "1000"=>r8<=in1; 
            when "1001"=>r9<=in1; 
            when "1010"=>r10<=in1; 
            when "1011"=>r11<=in1; 
            when "1100"=>r12<=in1; 
            when "1101"=>r13<=in1; 
            when "1110"=>r14<=in1; 
            when others=>r15<=in1; 
    end case; 
end if; 
end if; 
end if; 
    end process; 
end Behavioral; 
library IEEE; 
use IEEE.STD_LOGIC_1164.ALL; 
entity mem_path is 
    Port(in_proc:in STD_LOGIC_VECTOR(31 downto 0); 
        in_mem:in STD_LOGIC_VECTOR(31 downto 0); 
        out_proc,out_mem: out STD_LOGIC_VECTOR(31 downto 0); 
        mem_we : out STD_LOGIC; 
        byte_offset: in STD_LOGIC_VECTOR(1 DOWNTO 0); 
        opcode:in STD_LOGIC_VECTOR(2 downto 0); 
        blwe:out STD_LOGIC_VECTOR(3 downto 0)); 
end mem_path; 
 
Architecture Behavioral of mem_path is 
signal temp1,temp2,temp3,temp4:STD_LOGIC_VECTOR(31 downto 0); 
begin 
    temp1<="0000000000000000" &  in_mem(15 downto 0) when (byte_offset="10") else "0000000000000000" &  in_mem(31 downto 16); 
    temp2<="0000000000000000" &  in_mem(15 downto 0) when (byte_offset="10" and in_mem(15)='0') 
        else "0000000000000000" &  in_mem(31 downto 16) when (byte_offset="00" and in_mem(31)='0') 
        else "1111111111111111" &  in_mem(15 downto 0) when (byte_offset="10" and in_mem(15)='1') 
        else "1111111111111111" &  in_mem(31 downto 16) ; 
    temp3<="000000000000000000000000" & in_mem(7 downto 0) when (byte_offset="11") 
        else "000000000000000000000000" & in_mem(15 downto 8) when (byte_offset="10") 
        else "000000000000000000000000" & in_mem(23 downto 16) when(byte_offset="01") 
        else "000000000000000000000000" & in_mem(31 downto 24); 
    temp4<=  "000000000000000000000000" & in_mem(7 downto 0) when (byte_offset="11" and in_mem(7)='0') 
        else "000000000000000000000000" & in_mem(15 downto 8) when (byte_offset="10"and in_mem(15)='0') 
        else "000000000000000000000000" & in_mem(23 downto 16) when(byte_offset="01"and in_mem(23)='0') 
        else "000000000000000000000000" & in_mem(31 downto 24) when(byte_offset="00"and in_mem(31)='0') 
        else "111111111111111111111111" & in_mem(7 downto 0) when (byte_offset="11" and in_mem(7)='1') 
        else "111111111111111111111111" & in_mem(15 downto 8) when (byte_offset="10"and in_mem(15)='1') 
        else "111111111111111111111111" & in_mem(23 downto 16) when(byte_offset="01"and in_mem(23)='1') 
        else "111111111111111111111111" & in_mem(31 downto 24) ;            
             
    with opcode select out_proc<= 
        in_mem when "000",          --ldr 
        temp1 when "001",           --ldrh 
        temp2 when "010",           --ldrsh 
        temp3 when "011",           --ldrb 
        temp4 when "100",           --ldrsb 
        "11111111111111111111111111111111" when others; 
    blwe<="0001" when (opcode ="111" and byte_offset="11")  
    else "0010" when (opcode="111" and byte_offset="10") 
    else "0100" when (opcode="111" and byte_offset="01") 
    else "1000" when (opcode="111" and byte_offset="00") 
    else "0011" when (opcode="110" and byte_offset="10") 
    else "1100" when (opcode="110" and byte_offset="00") 
    else "1111" when (opcode="101") 
    else "0000"; 
    with byte_offset select out_mem<= 
        in_proc when "11", 
        in_proc(23 downto 0)&"01010101" when "10", 
        in_proc(15 downto 0)&"0011001100110011" when "01", 
        in_proc(7 downto 0)&"000111000111000111000111" when others;     
    end Behavioral; 
library ieee;   
    use ieee.std_logic_1164.all;   
      
    entity flop is   
      port(C, CE, PRE : in std_logic;   
            D : in  std_logic_vector (31 downto 0);   
            Q : out std_logic_vector (31 downto 0));   
    end flop;  
    architecture archi of flop is   
      begin   
        process (C, PRE)   
          begin   
            if (PRE='1') then   
              Q <= "00000000000000000000000000000000";   
            elsif (C'event and C='1')then   
              if (CE='1') then   
                Q <= D;   
              end if;   
            end if;   
        end process;   
    end archi; 
library IEEE; 
use IEEE.STD_LOGIC_1164.ALL; 
entity data_path is 
    Port(clk,reset:in STD_LOGIC 
        ); 
end data_path; 
--Fsrc=Flag source, 1 for shifter and multiplier, 0 for ALU  
--Bsrc=B register data source, 1 for multiplier, 0 for RF 
--Rsrc1=Read address on port 1. 1 for Shift amount, multiplier, 0 otherwise. 
Architecture Behavioural of data_path is 
Component Controller is
    port (
		instr: in std_logic_vector(31 downto 0);
        flag_in: in std_logic_vector(3 downto 0);
 
        PW,IorD,MR,MW,IW,DW,M2R,Rsrc,RW,AW,BW,SW,Asrc1,Fset,resW, Fsrc, Bsrc,Rsrc1:out STD_LOGIC;
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
end Component;
Component ALU is 
    Port ( op1 : in STD_LOGIC_VECTOR (31 downto 0); 
           op2 : in STD_LOGIC_VECTOR (31 downto 0); 
           op3 : out STD_LOGIC_VECTOR (31 downto 0); 
           opcode : in STD_LOGIC_VECTOR (3 downto 0); 
           C_in:in STD_LOGIC; 
           C_out : out STD_LOGIC; 
           V : out STD_LOGIC; 
           N : out STD_LOGIC; 
           Z : out STD_LOGIC); 
end component; 
Component Bram_wrapper is 
  port ( 
    BRAM_PORTA_addr : in STD_LOGIC_VECTOR ( 31 downto 0 ); 
    BRAM_PORTA_clk : in STD_LOGIC; 
    BRAM_PORTA_din : in STD_LOGIC_VECTOR ( 31 downto 0 ); 
    BRAM_PORTA_dout : out STD_LOGIC_VECTOR ( 31 downto 0 ); 
    BRAM_PORTA_en : in STD_LOGIC; 
    BRAM_PORTA_rst : in STD_LOGIC; 
    BRAM_PORTA_we : in STD_LOGIC_VECTOR ( 3 downto 0 ) 
  ); 
  end Component; 
  Component mem_path is 
      Port(in_proc:in STD_LOGIC_VECTOR(31 downto 0); 
          in_mem:in STD_LOGIC_VECTOR(31 downto 0); 
          out_proc,out_mem: out STD_LOGIC_VECTOR(31 downto 0); 
          mem_we : out STD_LOGIC; 
          byte_offset: in STD_LOGIC_VECTOR(1 DOWNTO 0); 
          opcode:in STD_LOGIC_VECTOR(2 downto 0); 
          blwe:out STD_LOGIC_VECTOR(3 downto 0)); 
  end Component;            
Component flop is   
      port(C, CE, PRE : in std_logic;   
            D : in  std_logic_vector (31 downto 0);   
            Q : out std_logic_vector (31 downto 0));   
    end component;  
Component reg_file is  
        Port(in1,pc_in:in STD_LOGIC_VECTOR(31 downto 0); 
            out1,out2,pc_out: out STD_LOGIC_VECTOR(31 downto 0); 
            r_ad1,r_ad2,w_ad: in STD_LOGIC_VECTOR(3 downto 0); 
            w_en,clk,reset, pcw:in STD_LOGIC); 
    end component; 
component shifter is 
        Port( op1: in STD_LOGIC_VECTOR(31 downto 0); 
              op2: out STD_LOGIC_VECTOR(31 downto 0); 
              s_amt:in STD_LOGIC_VECTOR(4 downto 0); 
              opcode: in STD_LOGIC_VECTOR(1 downto 0); 
              C_out:out STD_LOGIC; 
              C_in:in STD_LOGIC); 
    end component; 
component multiplier is 
Port(op1:in STD_LOGIC_VECTOR(31 downto 0); 
         op2:in STD_LOGIC_VECTOR(31 downto 0); 
         op3:out STD_LOGIC_VECTOR(31 downto 0); 
         N,Z:out STD_LOGIC); 
end component; 
signal op:STD_LOGIC_VECTOR(3 downto 0); 
signal Ssrc,mOff: STD_LOGIC_VECTOR(1 downto 0); 
signal Asrc2,mOpc: STD_LOGIC_VECTOR(2 downto 0); 
signal instr:STD_LOGIC_VECTOR(31 downto 0); 
signal Flags:STD_LOGIC_VECTOR(3 downto 0);
signal PW,IorD,MR,MW,IW,DW,M2R,Rsrc,RW,AW,BW,SW,Asrc1,Fset,ReW, Fsrc, Bsrc,rsrc1:STD_LOGIC;
signal Bram_in,Bram_out,Mul_out,B_in,mem_data,inst_data,data_data,rd1_data,rd2_data, A_data, B_data,res_data, ALU_data,RF_in, PC_data, CPSR_in, CPSR_out, ALU_op1, ALU_op2, ALU_op2_preshift, mem_ad,sh_data:STD_LOGIC_VECTOR(31 downto 0); 
signal sh_amt:STD_LOGIC_VECTOR(4 downto 0); 
signal rad1, rad2, flag_sig,wen:STD_LOGIC_VECTOR(3 downto 0); 
signal SH_carry, MU_neg,MU_zero,temp:STD_LOGIC; 
begin 
    ALU_op1<=PC_data when Asrc1='0' else A_data; 
    ALU_op2_preshift<=B_data when Asrc2="000" else "00000000000000000000000000000100" when Asrc2="001" else ("00000000000000000000" & inst_data(11 downto 0)) when Asrc2="010" else "000000" &inst_data(23 downto 0) & "00" when Asrc2="011" else ("000000000000000000000000" & inst_data(7 downto 0)); 
    instr<=inst_data; 
    rad2<=inst_data(3 downto 0) when Rsrc='0' else inst_data(15 downto 12); 
    rad1<=inst_data(19 downto 16) when Rsrc1='0' else inst_data(11 downto 8); 
    RF_in<=data_data when M2R='1' else res_data; 
    CPSR_in<=(flag_sig & "0000000000000000000000000000") when Fsrc='1' else (SH_carry & flag_sig(2 downto 0) & "0000000000000000000000000000"); 
    Flags<=CPSR_out(31 downto 28); 
    sh_amt<=inst_data(11 downto 7) when Ssrc="01" else (inst_data(11 downto 8) & '0') when Ssrc="10" else sh_data(4 downto 0) when Ssrc="11" else "00000"; 
    mem_ad<=PC_data when IorD='0' else res_data; 
    B_in<=rd2_data when  Bsrc='0' else Mul_out; 
    CPSR:flop Port map(clk,Fset,reset,CPSR_in,CPSR_out); 
    IR:flop Port map(clk,IW,reset,mem_data,inst_data); 
    DR:flop Port map(clk,DW,reset,mem_data,data_data); 
    A:flop Port map(clk,AW,reset,rd1_data,A_data); 
    B:flop Port map(clk,BW,reset,B_in,B_data); 
    S:flop Port map(clk,SW,reset,rd1_data,sh_data); 
    RES:flop Port map(clk,ReW,reset,ALU_data,res_data); 
    RF:reg_file Port map(RF_in,ALU_data,rd1_data,rd2_data, PC_data,rad1,rad2,inst_data(15 downto 12),RW,clk,reset, PW); 
    ALU1:ALU Port map(ALU_op1,ALU_op2,ALU_data,op,CPSR_out(31),flag_sig(3),flag_sig(2),flag_sig(1),flag_sig(0)); 
    PM_path: mem_path Port map(B_data,Bram_in,mem_data,Bram_out,temp,mOff,mOpc,wen); 
    Mem:Bram_wrapper Port map(mem_ad,clk,Bram_out,Bram_in,MR,reset,wen); 
    Sft:Shifter Port map(ALU_op2_preshift,ALU_op2,sh_amt,inst_data(6 downto 5), SH_carry, CPSR_out(31)); 
    Mul:Multiplier Port Map(A_data,B_data, Mul_out,MU_neg,MU_zero); 


    Master:Controller Port Map(instr,Flags,PW,IorD,MR,MW,IW,DW,M2R,Rsrc,RW,AW,BW,SW,Asrc1,Fset,ReW, Fsrc, Bsrc,rsrc1,clk,reset,Ssrc,mOff,Asrc2,mOpc,op);
end Behavioural;
