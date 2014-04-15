library IEEE;

use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.utilities.all;
entity CLOCK_GENERATOR is
  generic
  ( 
    DEPTH         : natural range 1 to 24 := 1
  );
  port
  (
    CLOCK  : in  std_logic;
    RESET  : in  std_logic;

    COMPARE        : in  std_logic;
    CLOSE_FEEDBACK : out std_logic := '1';
    SAMPLE_INPUT   : out std_logic := '0';
    SELECT_V_IN    : out std_logic := '1';
    SELECT_V_REF   : out std_logic := '0';

    CMP            : out std_logic := '0';
    BITS           : out std_logic_vector(DEPTH-1 downto 0) := (others => '0');
    DIGITAL_BITS   : out std_logic_vector(DEPTH-1 downto 0) := (others => '0');
    VALID          : out std_logic := '0'
  );
end CLOCK_GENERATOR;

architecture STRUCTURAL of CLOCK_GENERATOR is
  constant MAX_BIT_COUNT : std_logic_vector(vector_length(DEPTH) - 1 downto 0)
                             := std_logic_vector(to_unsigned(DEPTH-1, vector_length(DEPTH)));

  type   STATE_TYPE is (IDLE,
                        CLEAR, SAMPLE, DISCONNECT_INPUT, BREAK_FEEDBACK, 
                        HOLD, CONNECT_REFERENCE,
                        NEXT_BIT, TEST_BIT, SAMPLE_CMP, BIT_OK,
                        DONE);
  signal STATE            : STATE_TYPE := IDLE;

  signal BITS_I           : std_logic_vector(BITS'range)
                              := (others => '0');
  signal BIT_COUNT        : std_logic_vector(vector_length(DEPTH)-1 downto 0) 
                              := MAX_BIT_COUNT;
  signal DIGITAL_BITS_I   : std_logic_vector(DIGITAL_BITS'range)
                              := (others => '0');
  signal BIT_COUNT_AS_INT : integer range 0 to DEPTH-1;
begin

  BIT_COUNT_AS_INT <= to_integer(unsigned(BIT_COUNT));
  BITS             <= BITS_I;
  DIGITAL_BITS         <= DIGITAL_BITS_I;
  STATE_MACHINE : process (CLOCK) is
  begin
    if (rising_edge(CLOCK))
    then
      if (RESET = '1')
      then
        STATE      <= CLEAR;

        BIT_COUNT  <= MAX_BIT_COUNT;
        BITS_I     <= (others => '0');
        DIGITAL_BITS_I <= (others => '0');
      else
        case (STATE) is
          when IDLE   =>
            STATE     <= IDLE;

            BIT_COUNT <= MAX_BIT_COUNT;
            BITS_I    <= (others => '0');
            
          when CLEAR  =>
            STATE     <= SAMPLE;

            BIT_COUNT <= MAX_BIT_COUNT;
            BITS_I    <= (others => '1');
          
          when SAMPLE =>
            STATE     <= DISCONNECT_INPUT;

            BIT_COUNT <= MAX_BIT_COUNT;
            BITS_I    <= (others => '1');

          when DISCONNECT_INPUT =>
            STATE     <= BREAK_FEEDBACK;

            BIT_COUNT <= MAX_BIT_COUNT;
            BITS_I    <= (others => '1');
            
          when BREAK_FEEDBACK =>
            STATE     <= HOLD;

            BIT_COUNT <= MAX_BIT_COUNT;
            BITS_I    <= (others => '0');

          when HOLD   =>
            STATE     <= CONNECT_REFERENCE;

            BIT_COUNT <= MAX_BIT_COUNT;
            BITS_I    <= (others => '0');

          when CONNECT_REFERENCE =>
            STATE     <= NEXT_BIT;

            BIT_COUNT <= MAX_BIT_COUNT;
            BITS_I    <= (others => '0');
            
          when NEXT_BIT  =>
            STATE     <= TEST_BIT;
            
            BIT_COUNT <= BIT_COUNT;

            BITS_I                   <= (others => '0');
            BITS_I(BIT_COUNT_AS_INT) <= '1';

          when TEST_BIT =>
            STATE     <= SAMPLE_CMP;

            BIT_COUNT <= BIT_COUNT;
            
            BITS_I                   <= (others => '0');
            BITS_I(BIT_COUNT_AS_INT) <= '1';

          when SAMPLE_CMP =>
            STATE     <= BIT_OK;

            BIT_COUNT <= BIT_COUNT;

            BITS_I                   <= (others => '0');
            BITS_I(BIT_COUNT_AS_INT) <= '1';
            
          when BIT_OK =>
            if (BIT_COUNT = (BIT_COUNT'range => '0'))
            then
              STATE     <= DONE;
              BIT_COUNT <= MAX_BIT_COUNT;
            else
              STATE     <= NEXT_BIT;
              BIT_COUNT <= std_logic_vector(unsigned(BIT_COUNT) - 1);
            end if;
            BITS_I                   <= (others => '0');

          when DONE =>
            STATE           <= CLEAR;
            
            BIT_COUNT       <= BIT_COUNT;
            BITS_I          <= (others => '0');

            DIGITAL_BITS_I  <= BITS_I;
            
          when others =>           
            STATE     <= IDLE;

            BIT_COUNT <= MAX_BIT_COUNT;
            BITS_I    <= (others => '0');
        
        end case;
      end if;
    end if;
  end process STATE_MACHINE;

  COMBINATIONAL : process (RESET, STATE) is
  begin
    if (RESET = '1')
    then
      CLOSE_FEEDBACK <= '1';
      SAMPLE_INPUT   <= '0';
      
      SELECT_V_IN    <= '0';
      SELECT_V_REF   <= '0';

      CMP            <= '0';          
      
      VALID  <= '0';
    else
      case (STATE) is
        when IDLE   =>
          CLOSE_FEEDBACK <= '1';
          SAMPLE_INPUT   <= '0';
          
          SELECT_V_IN    <= '0';
          SELECT_V_REF   <= '0';

          CMP            <= '0';          
          
          VALID          <= '0';
          
        when CLEAR  =>
          CLOSE_FEEDBACK <= '1';
          SAMPLE_INPUT   <= '0';
          
          SELECT_V_IN    <= '0';
          SELECT_V_REF   <= '0';

          CMP            <= '0';          
          
          VALID          <= '0';
          
        when SAMPLE =>
          CLOSE_FEEDBACK <= '1';
          SAMPLE_INPUT   <= '1';
          
          SELECT_V_IN    <= '1';
          SELECT_V_REF   <= '0';

          CMP            <= '0';          
          
          VALID          <= '0';

        when DISCONNECT_INPUT =>
          CLOSE_FEEDBACK <= '1';
          SAMPLE_INPUT   <= '1';
          
          SELECT_V_IN    <= '0';
          SELECT_V_REF   <= '0';

          CMP            <= '0';          
          
          VALID          <= '0';
          
        when BREAK_FEEDBACK =>
          CLOSE_FEEDBACK <= '0';
          SAMPLE_INPUT   <= '1';
          
          SELECT_V_IN    <= '0';
          SELECT_V_REF   <= '0';

          CMP            <= '0';          
          
          VALID          <= '0';

        when HOLD           =>
          CLOSE_FEEDBACK <= '0';
          SAMPLE_INPUT   <= '0';

          SELECT_V_IN    <= '0';
          SELECT_V_REF   <= '0';

          CMP            <= '0';                    
          
          VALID          <= '0';
          
        when CONNECT_REFERENCE =>
          CLOSE_FEEDBACK <= '0';
          SAMPLE_INPUT   <= '0';

          SELECT_V_IN    <= '0';
          SELECT_V_REF   <= '1';

          CMP            <= '0';          
          
          VALID          <= '0';
          
        when DONE =>
          CLOSE_FEEDBACK <= '0';
          SAMPLE_INPUT   <= '0';

          SELECT_V_IN    <= '0';
          SELECT_V_REF   <= '0';

          CMP            <= '0';          
          
          VALID  <= '1';

        when SAMPLE_CMP =>
          CLOSE_FEEDBACK <= '0';
          SAMPLE_INPUT   <= '0';

          SELECT_V_IN    <= '0';
          SELECT_V_REF   <= '1';

          CMP            <= '1';

          VALID          <= '0';
          
        when others =>                  -- bit cycling phase
          CLOSE_FEEDBACK <= '0';
          SAMPLE_INPUT   <= '0';

          SELECT_V_IN    <= '0';
          SELECT_V_REF   <= '1';

          CMP            <= '0';          
          
          VALID  <= '0';
      end case;
    end if;
  end process COMBINATIONAL;

end STRUCTURAL;
