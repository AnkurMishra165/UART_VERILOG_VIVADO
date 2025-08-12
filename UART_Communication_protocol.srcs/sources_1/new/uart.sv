`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 08/12/2025 12:06:17 PM
// Design Name: 
// Module Name: uart
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module uart #(parameter DBIT=8, SB_TICK=16,FIFO_W=2)(
input logic clk,reset,
input logic rd_uart, wr_uart,rx,
input logic[7:0] w_data,
input logic [10:0] dvsr,
output logic tx_full,rx_empty,tx,
output logic [7:0] r_data);

//signal Declaration
logic tick,rx_done_tick,tx_done_tick;
logic tx_empty,tx_fifo_not_empty;
logic [7:0] tx_fifo_out,rx_data_out;

baud_gen baud_gen_unit(.clk(clk), .reset(reset), .dvsr(dvsr), .tick(tick));
uart_rx #( .DBIT(DBIT), .SB_TICK(SB_TICK)) uart_rx_unit(
.clk(clk), .reset(reset), .rx(rx), .s_tick(tick), .rx_done_tick(rx_done_tick),
.dout(rx_data_out)
);

uart_tx #(.DBIT(DBIT), .SB_TICK(SB_TICK)) uart_tx_unit(
.clk(clk), .reset(reset), .tx_start(tx_fifo_not_empty), .s_tick(tick),
.din(tx_fifo_out), .tx(tx), .tx_done_tick(tx_done_tick));

fifo #(.DATA_WIDTH(DBIT), .ADDR_WIDTH(FIFO_W)) fifo_rx_unit(.clk(clk), .reset(reset),
.wr(rx_done_tick), .rd(rd_uart), .w_data(rx_data_out), .empty(rx_empty), .r_data(r_data));

fifo #(.DATA_WIDTH(DBIT), .ADDR_WIDTH(FIFO_W)) fifo_tx_unit (.clk(clk), .reset(reset), 
.wr(wr_uart), .rd(tx_fifo_rd), .w_data(w_data), .empty(tx_empty), .full(tx_full), .r_data(tx_data_out));

endmodule

module baud_gen(input logic clk,reset,
                 input logic[10:0] dvsr,
                 output logic tick);
logic [10:0] r_reg;
logic [10:0] r_next;
always_ff @(posedge clk, posedge reset) begin
if(reset)
r_reg<=0;
else
r_reg<=r_next;

end

assign r_next=(r_reg==dvsr)?0:r_reg+1;
assign tick=(r_reg==1);
                 
                 
 endmodule
 
 module uart_rx #(parameter DBIT=8, SB_TICK=16) (
 input logic clk,reset,
 input logic rx,s_tick,
 output logic rx_done_tick,
 output logic [7:0] dout);
 typedef enum {idle,start,data,stop} state_type;
 state_type state_reg,state_next;
 logic [3:0] s_reg,s_next;
 logic [2:0] n_reg,n_next;
 logic [7:0] b_reg,b_next;
 
 always_ff @(posedge clk,posedge reset) begin
 if(reset) begin
 state_reg<=idle;
 s_reg<=0;
 n_reg<=0;
 b_reg<=0;
 end
 else begin
 state_reg<=state_next;
 s_reg<=s_next;
 n_reg<=n_next;
 b_reg<=b_next;
 end
 end
 always_comb begin
 state_next=state_reg;
 rx_done_tick=1'b0;
 s_next=s_reg;
 n_next=n_reg;
 b_next=b_reg;
 
 case(state_reg)
 idle: begin
 if(~rx) begin
 state_next=start;
 s_next=0;
 end
 end
 start:
 if(s_tick) 
 if(s_reg==7) begin
 state_next=data;
 s_next=0;
 n_next=0;
 end
 else
 s_next=s_reg+1;
 
 data:
 if(s_tick)
 if(s_reg==15) begin
 s_next=0;
 b_next={rx,b_reg[7:1]};
 if(n_reg==(DBIT-1))
 state_next=stop;
 else
 n_next=n_reg+1;
 end
 else 
 s_next=s_reg+1;
 
 stop:
 if(s_tick)
 if(s_reg==(SB_TICK-1)) begin
 state_next=idle;
 rx_done_tick=1;
 end
 else
 s_next=s_reg+1; 
 endcase
 end
 assign dout=b_reg;
 endmodule
 
module uart_tx #(parameter DBIT=8 ,SB_TICK=16) (
  input  logic        clk, reset,
  input  logic        tx_start, s_tick,
  input  logic [7:0]  din,
  output logic        tx_done_tick,
  output logic        tx 
);

  typedef enum {idle,start,data,stop} state_type;
  state_type state_reg,state_next;
  logic [3:0] s_reg,s_next;
  logic [2:0] n_reg,n_next;
  logic [7:0] b_reg,b_next;
  logic tx_reg, tx_next;

  always_ff @(posedge clk,posedge reset) begin
    if(reset) begin
      state_reg<=idle;
      s_reg<=0;
      n_reg<=0;
      b_reg<=0;
      tx_reg<=1'b1;
    end else begin
      state_reg<=state_next;
      s_reg<=s_next;
      n_reg<=n_next;
      b_reg<=b_next;
      tx_reg<=tx_next;
    end
  end

  always_comb begin
    state_next=state_reg;
    tx_done_tick=1'b0;
    s_next=s_reg;
    n_next=n_reg;
    b_next=b_reg;
    tx_next=tx_reg;

    case(state_reg)
      idle: begin
        tx_next=1'b1;
        if(tx_start) begin
          state_next=start;
          s_next=0;
          b_next=din;
        end
      end

      start: begin
        tx_next=1'b0;
        if(s_tick) begin
          if(s_reg==4'd15) begin
            state_next=data;
            s_next=0;
            n_next=0;
          end else begin
            s_next=s_reg+1;
          end
        end
      end

      data: begin
        tx_next=b_reg[0];
        if(s_tick) begin
          if(s_reg==4'd15) begin
            s_next=0;
            b_next=b_reg>>1;
            if(n_reg==(DBIT-1))
              state_next=stop;
            else
              n_next=n_reg+1;
          end else begin
            s_next=s_reg+1;
          end
        end
      end

      stop: begin
        tx_next=1'b1;
        if(s_tick) begin
          if(s_reg==(SB_TICK-1)) begin
            state_next=idle;
            tx_done_tick=1'b1;
            s_next=0;
          end else begin
            s_next=s_reg+1;
          end
        end
      end
    endcase
  end

  assign tx=tx_reg;
endmodule

 
 // -----------------------------------------------------------------------------
// Simple synchronous FIFO (single clock)
// - First-Word Fall-Through (FWFT): r_data reflects mem[r_ptr] continuously
// - No writes when full, no reads when empty (ignored safely)
// - Simultaneous wr & rd allowed (count stays same)
// -----------------------------------------------------------------------------
// Matches your instances:
//   fifo #(.DATA_WIDTH(8), .ADDR_WIDTH(FIFO_W)) rx_fifo (...);
//   fifo #(.DATA_WIDTH(8), .ADDR_WIDTH(FIFO_W)) tx_fifo (...);
// -----------------------------------------------------------------------------
module fifo #(
  parameter int DATA_WIDTH = 8,
  parameter int ADDR_WIDTH = 4   // depth = 2**ADDR_WIDTH
)(
  input  logic                   clk,
  input  logic                   reset,

  // write side
  input  logic                   wr,
  input  logic [DATA_WIDTH-1:0]  w_data,

  // read side
  input  logic                   rd,
  output logic [DATA_WIDTH-1:0]  r_data,

  // status
  output logic                   empty,
  output logic                   full
);

  localparam int DEPTH = 1 << ADDR_WIDTH;

  // storage
  logic [DATA_WIDTH-1:0] mem [0:DEPTH-1];

  // pointers and count
  logic [ADDR_WIDTH-1:0] w_ptr_reg, w_ptr_next;
  logic [ADDR_WIDTH-1:0] r_ptr_reg, r_ptr_next;
  logic [ADDR_WIDTH  :0] count_reg, count_next; // one extra bit for DEPTH

  // convenient enables (block illegal ops)
  wire do_write = wr & ~full;    // only write when not full
  wire do_read  = rd & ~empty;   // only read  when not empty

  // ---------------------
  // sequential state
  // ---------------------
  always_ff @(posedge clk or posedge reset) begin
    if (reset) begin
      w_ptr_reg <= '0;
      r_ptr_reg <= '0;
      count_reg <= '0;
    end else begin
      w_ptr_reg <= w_ptr_next;
      r_ptr_reg <= r_ptr_next;
      count_reg <= count_next;

      if (do_write) begin
        mem[w_ptr_reg] <= w_data;  // write happens on this edge
      end
    end
  end

  // ---------------------
  // next-state logic
  // ---------------------
  // pointer increments wrap naturally due to vector width
  assign w_ptr_next = w_ptr_reg + (do_write ? 1'b1 : 1'b0);
  assign r_ptr_next = r_ptr_reg + (do_read  ? 1'b1 : 1'b0);

  // count update: +1 on write, -1 on read, 0 on both or none
  assign count_next = count_reg + (do_write ? 1 : 0) - (do_read ? 1 : 0);

  // status flags
  assign empty = (count_reg == 0);
  assign full  = (count_reg == DEPTH);

  // ---------------------
  // FWFT read datapath
  // ---------------------
  // r_data always presents the word at the current read pointer.
  // When rd is pulsed and not empty, r_ptr advances on the next clock,
  // so r_data "falls through" to the next word automatically.
  assign r_data = mem[r_ptr_reg];

  // ---------------------
  // (Optional) Simulation-time guards (comment out for pure Verilog)
  // ---------------------
`ifdef ASSERTIONS
  // Underflow/overflow checks
  assert_no_read_empty:  assert property (@(posedge clk) disable iff (reset) rd |-> !empty);
  assert_no_write_full:  assert property (@(posedge clk) disable iff (reset) wr |-> !full);
`endif

endmodule
