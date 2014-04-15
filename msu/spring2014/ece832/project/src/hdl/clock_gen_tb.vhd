library IEEE;

use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity TESTBENCH is
  generic
  (
    DEPTH      : natural               := 8;
    TEST_VALUE : unsigned (7 downto 0) := X"A5"
  );
end;

architecture SIMULATION of TESTBENCH is
  signal CLOCK      : std_logic := '0';
  signal RESET      : std_logic := '1';

  signal COMPARISON     : std_logic; 
  signal CLOSE_FEEDBACK : std_logic;
  signal SAMPLE_INPUT   : std_logic;
  signal SELECT_V_IN    : std_logic;
  signal SELECT_V_REF   : std_logic;

  signal CMP            : std_logic;
  signal BITS           : std_logic_vector(DEPTH-1 downto 0);
  signal DIGITAL_BITS   : std_logic_vector(DEPTH-1 downto 0);
  signal VALID          : std_logic;

  component clock_generator is
    generic
    (
      DEPTH : integer range 1 to 24
    );
    port
    (
      CLOCK : in  std_logic;
      RESET : in  std_logic;

      COMPARE        : in  std_logic;
      CLOSE_FEEDBACK : out std_logic := '1';
      SAMPLE_INPUT   : out std_logic := '0';
      SELECT_V_IN    : out std_logic := '1';
      SELECT_V_REF   : out std_logic := '0';

      CMP            : out std_logic := '0';
      BITS           : out std_logic_vector(DEPTH-1 downto 0);
      DIGITAL_BITS   : out std_logic_vector(DEPTH-1 downto 0);
      VALID          : out std_logic
    );
  end component clock_generator;
begin
  RESET <= '0' after 50 ns;
  CLOCK <= not CLOCK after 10 ns;

  COMPARISON <= '1' when (unsigned(BITS) <= TEST_VALUE) else '0';
  DUT : clock_generator
    generic map
    (
      DEPTH => DEPTH
    )
    port    map
    (
      CLOCK   => CLOCK,
      RESET   => RESET,

      COMPARE        => COMPARISON,
      CLOSE_FEEDBACK => CLOSE_FEEDBACK,
      SAMPLE_INPUT   => SAMPLE_INPUT,
      SELECT_V_IN    => SELECT_V_IN,
      SELECT_V_REF   => SELECT_V_REF,

      CMP            => CMP,
      BITS           => BITS,
      DIGITAL_BITS   => DIGITAL_BITS,
      VALID          => VALID
    );

end SIMULATION;

