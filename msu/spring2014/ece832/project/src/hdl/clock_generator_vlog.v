`define base_two_log(n) ((n) <= (1<<0) ? 0 : (n) <= (1<<1) ? 1 :\
                    (n) <= (1<<2) ? 2 : (n) <= (1<<3) ? 3 :\
                    (n) <= (1<<4) ? 4 : (n) <= (1<<5) ? 5 :\
                    (n) <= (1<<6) ? 6 : (n) <= (1<<7) ? 7 :\
                    (n) <= (1<<8) ? 8 : (n) <= (1<<9) ? 9 :\
                    (n) <= (1<<10) ? 10 : (n) <= (1<<11) ? 11 :\
                    (n) <= (1<<12) ? 12 : (n) <= (1<<13) ? 13 :\
                    (n) <= (1<<14) ? 14 : (n) <= (1<<15) ? 15 :\
                    (n) <= (1<<16) ? 16 : (n) <= (1<<17) ? 17 :\
                    (n) <= (1<<18) ? 18 : (n) <= (1<<19) ? 19 :\
                    (n) <= (1<<20) ? 20 : (n) <= (1<<21) ? 21 :\
                    (n) <= (1<<22) ? 22 : (n) <= (1<<23) ? 23 :\
                    (n) <= (1<<24) ? 24 : (n) <= (1<<25) ? 25 :\
                    (n) <= (1<<26) ? 26 : (n) <= (1<<27) ? 27 :\
                    (n) <= (1<<28) ? 28 : (n) <= (1<<29) ? 29 :\
                    (n) <= (1<<30) ? 30 : (n) <= (1<<31) ? 31 : 32)

module clock_generator_vlog
   #( parameter integer DEPTH = 1 )
    ( input wire clock
    , input wire reset

    , input  wire compare
     
    , output reg  close_feedback
    , output reg  sample_input
    , output reg  select_v_in
    , output reg  select_v_ref

    , output wire [DEPTH-1 : 0] bits
    , output wire [DEPTH-1 : 0] digital_bits
    , output reg                valid
    );

   localparam STATE_IDLE              = 4'd0,
              STATE_CLEAR             = 4'd1,
              STATE_SAMPLE            = 4'd2,
              STATE_DISCONNECT_INPUT  = 4'd3,
              STATE_BREAK_FEEDBACK    = 4'd4,
              STATE_HOLD              = 4'd5,
              STATE_CONNECT_REFERENCE = 4'd6,
              STATE_NEXT_BIT          = 4'd7,
              STATE_TEST_BIT          = 4'd8,
              STATE_DONE              = 4'd9;

   localparam MAX_BIT_COUNT = DEPTH - 1;
   localparam LOG_DEPTH     = `base_two_log(DEPTH);

   reg [3:0] 		     state            = STATE_IDLE;
   reg [DEPTH-1 : 0]         bits_i           = 'd0;
   reg [LOG_DEPTH-1 : 0]     bit_count        = MAX_BIT_COUNT;
   reg [DEPTH-1 : 0] 	     digital_bits_i   = 'd0;
   integer [LOG_DEPTH-1 : 0] bit_count_as_int = MAX_BIT_COUNT;

   /***************************************************/
   
   assign bits         = bits_i;
   assign digital_bits = digital_bits_i;

   always @(reset, bit_count)
     begin
        if (reset == 1'b1)
          begin
             bit_count_as_int = MAX_BIT_COUNT;
          end
        else
          begin
            bit_count_as_int  = bit_count;
          end
     end

   always @(posedge clock)
     begin
       if (reset == 1'b1)
         begin
           state <= STATE_CLEAR;
    
           bit_count      <= MAX_BIT_COUNT;
           bits_i         <= {DEPTH{1'b0}};
           digital_bits_i <= {DEPTH{1'b0}};
	 end
       else
         begin
           case (state)
             STATE_IDLE:
               begin
                 state <= STATE_IDLE;

                 bit_count <= MAX_BIT_COUNT;
                 bits_i    <= {DEPTH{1'b1}};
               end
             STATE_CLEAR:
               begin
                 state <= STATE_SAMPLE;
	  
                 bit_count <= MAX_BIT_COUNT;
                 bits_i    <= {DEPTH{1'b1}};
               end
             STATE_SAMPLE:
               begin
                 state <= STATE_DISCONNECT_INPUT;

                 bit_count <= MAX_BIT_COUNT;
                 bits_i    <= {DEPTH{1'b1}};
               end
             STATE_DISCONNECT_INPUT:
               begin
                  state <= STATE_BREAK_FEEDBACK;

                  bit_count <= MAX_BIT_COUNT;
                  bits_i    <= {DEPTH{1'b0}};
               end
             STATE_BREAK_FEEDBACK:
               begin
                  state <= STATE_HOLD;

                  bit_count <= MAX_BIT_COUNT;
                  bits_i    <= {DEPTH{1'b0}};
               end
             STATE_HOLD:
               begin
                  state <= STATE_CONNECT_REFERENCE;

                  bit_count <= MAX_BIT_COUNT;
                  bits_i    <= {DEPTH{1'b0}};
               end                  
             STATE_CONNECT_REFERENCE:
               begin
                  state <= STATE_NEXT_BIT;
                  
                  bit_count <= MAX_BIT_COUNT;
                  bits_i    <= {DEPTH{1'b0}};
               end
             STATE_NEXT_BIT:
               begin
                  state <= STATE_TEST_BIT;

                  bit_count <= bit_count;

//                  bits_i                   <= {DEPTH{1'b0}};
                  bits_i[bit_count_as_int] <= 1'b1;
               end
             STATE_TEST_BIT:
               begin
                  if (bit_count == {LOG_DEPTH{1'b0}})
                    begin
                       state <= STATE_DONE;
                       bit_count <= MAX_BIT_COUNT;
                    end
                  else
                    begin
                       state <= STATE_NEXT_BIT;
                       bit_count <= bit_count - 1'b1;
                    end
                  bits_i[bit_count_as_int] <= compare;
//{DEPTH{1'b0}};
               end
             STATE_DONE:
               begin
                  state <= STATE_CLEAR;

                  bit_count      <= bit_count;
                  bits_i         <= {DEPTH{1'b0}};
                  digital_bits_i <= bits_i;
               end
             default:
               begin
                  state <= STATE_IDLE;

                  bit_count <= MAX_BIT_COUNT;
                  bits_i    <= {DEPTH{1'b0}};
               end
           endcase
         end
     end
   
     always @(reset, state)
       begin
          if (reset == 1'b1)
            begin
               close_feedback = 1'b1;
               sample_input   = 1'b0;

               select_v_in    = 1'b0;
               select_v_in    = 1'b0;

               valid          = 1'b0;
            end
          else
            begin
               case (state)
                 STATE_IDLE:
                   begin
                      close_feedback = 1'b1;
                      sample_input   = 1'b0;

                      select_v_in    = 1'b0;
                      select_v_ref   = 1'b0;

                      valid          = 1'b0;
                   end
                 STATE_CLEAR:
                   begin
                      close_feedback = 1'b1;
                      sample_input   = 1'b0;

                      select_v_in    = 1'b0;
                      select_v_ref   = 1'b0;

                      valid          = 1'b0;
                   end
                 STATE_SAMPLE:
                   begin
                      close_feedback = 1'b1;
                      sample_input   = 1'b1;

                      select_v_in    = 1'b1;
                      select_v_ref   = 1'b0;

                      valid          = 1'b0;
                   end
                 STATE_DISCONNECT_INPUT:
                   begin
                      close_feedback = 1'b1;
                      sample_input   = 1'b1;

                      select_v_in    = 1'b1;
                      select_v_ref   = 1'b0;

                      valid          = 1'b0;
                   end
                 STATE_BREAK_FEEDBACK:
                   begin
                      close_feedback = 1'b0;
                      sample_input   = 1'b1;

                      select_v_in    = 1'b0;
                      select_v_ref   = 1'b0;

                      valid          = 1'b0;
                   end
                STATE_HOLD:
                  begin
                     close_feedback  = 1'b0;
                     sample_input    = 1'b0;

                     select_v_in     = 1'b0;
                     select_v_ref    = 1'b0;

                     valid           = 1'b0;
                  end
                STATE_CONNECT_REFERENCE:
                  begin
                     close_feedback  = 1'b0;
                     sample_input    = 1'b0;

                     select_v_in     = 1'b0;
                     select_v_ref    = 1'b1;

                     valid           = 1'b0;
                  end
                STATE_DONE:
                  begin
                     close_feedback  = 1'b0;
                     sample_input    = 1'b0;

                     select_v_in     = 1'b0;
                     select_v_ref    = 1'b0;

                     valid           = 1'b1;
                  end
                default:
                  begin
                     close_feedback  = 1'b0;
                     sample_input    = 1'b0;

                     select_v_in     = 1'b0;
                     select_v_ref    = 1'b1;

                     valid           = 1'b0;
                  end
              endcase
            end
       end
   
endmodule // clock_generator_vlog

