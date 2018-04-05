Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;

entity Bctrl is
port (
		ins: in std_logic_vector(3 downto 0);
		flags: in std_logic_vector(3 downto 0);
		p: out std_logic);
end entity Bctrl;

architecture behavioural of Bctrl is
begin
	process(flags,ins)  --VNCZ is the flag ordering
	begin
		case ins is
			when "0000" => p <= flags(0);
			when "0001" => p <= not flags(0);
			when "0010" => p <= flags(1);
			when "0011" => p <= not flags(1);
			when "0100" => p <= flags(2);
			when "0101" => p <= not flags(2);
			when "0110" => p <= flags(3);
			when "0111" => p <= not flags(3);
			when "1000" => p <= (not (flags(0))) and flags(1);
			when "1001" => p <= not ( (not (flags(0))) and flags(1));
			when "1010" => p <= flags(3) xnor flags(2);
			when "1011" => p <= (flags(3) xor flags(2));
			when "1100" => p <= (flags(3) xnor flags(2)) and (not flags(0));
			when "1101" => p <= not ((flags(3) xnor flags(2)) and (not flags(0)));
			when "1110" => p <= '1';

    end case;
  end process;

end behavioural;
