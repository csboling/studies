module clock_generator
   #( parameter integer DEPTH = 1 )
    ( input wire clock
    , input wire reset

    , input  wire compare
     
    , output wire close_feedback
    , output wire sample_input
    , output wire select_v_in
    , output wire select_v_ref

    , output wire [DEPTH-1 : 0] bits
    , output wire [DEPTH-1 : 0] digital_bits
    , output wire               valid
    );

   localparam reg [3:0] STATE_IDLE              = 4'd0,
                        STATE_CLEAR             = 4'd1,
                        STATE_SAMPLE            = 4'd2,
                        STATE_DISCONNECT_INPUT  = 4'd3,
                        STATE_BREAK_FEEDBACK    = 4'd4,
		        STATE_HOLD              = 4'd5,
		        STATE_CONNECT_REFERENCE = 4'd6,
		        STATE_NEXT_BIT          = 4'd7,
		        STATE_TEST_BIT          = 4'd8,
		        STATE DONE              = 4'd9;

   reg [3:0] 		     state = STATE_IDLE;
   reg [DEPTH-1 : 0]         bits_i         = 'd0;
   reg [$clog2(DEPTH)-1 : 0] bit_count      = 'd0;
   reg [DEPTH-1 : 0] 	     digital_bits_i = 'd0;
   integer                   bits_as_int;

   /***************************************************/
   
   assign bits         = bits_i;
   assign digital_bits = digital_bits_i;
   assign bits_as_int  = bits_i;

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
             STATE_CONNECT_REFERENCE:
               begin
                  state <= STATE_NEXT_BIT;
                  
                  bit_count <= MAX_BIT_COUNT;
                  bits_i    <= {DEPTH{1'b0}};
               end
             STATE_NEXT_BIT:
               begin
                  state <= STATE_TEST_BIT;

                  bit_count <= BIT_COUNT;

                  bits_i              <= {DEPTH{1'b0}};
                  bits_i[bits_as_int] <= 1'b1;
               end
             STATE_TEST_BIT:
               begin
                  if (BIT_COUNT == {$clog2(DEPTH){1'b0}})
                    begin
                       state <= STATE_DONE;
                       bit_count <= MAX_BIT_COUNT;
                    end
                  else
                    begin
                       state <= STATE_NEXT_BIT;
                       bit_count <= bit_count - 1'b1;
                    end
                  bits_i <= {DEPTH{1'b0}};
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
            end
       end
   
   
