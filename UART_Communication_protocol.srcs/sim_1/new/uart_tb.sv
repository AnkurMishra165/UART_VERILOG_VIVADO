`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 08/12/2025 07:00:31 PM
// Design Name: 
// Module Name: uart_tb
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


`timescale 1ns/1ps

module tb_uart;

  // DUT params (feel free to tweak FIFO_W depth)
  localparam int DBIT    = 8;
  localparam int SB_TICK = 16;
  localparam int FIFO_W  = 3;   // depth = 2**3 = 8 bytes

  // Clk/Reset
  logic clk = 0;
  logic reset;

  // Host-side handshake
  logic rd_uart, wr_uart;
  logic [7:0] w_data;
  wire  [7:0] r_data;
  wire        tx_full, rx_empty;

  // Serial
  wire tx;
  wire rx;

  // Baud divisor
  logic [10:0] dvsr;

  // Clock: 100 MHz (10 ns)
  always #5 clk = ~clk;

  // Simple loopback: drive RX from TX
  assign rx = tx;

  // DUT
  uart #(.DBIT(DBIT), .SB_TICK(SB_TICK), .FIFO_W(FIFO_W)) dut (
    .clk(clk),
    .reset(reset),
    .rd_uart(rd_uart),
    .wr_uart(wr_uart),
    .rx(rx),
    .w_data(w_data),
    .dvsr(dvsr),
    .tx_full(tx_full),
    .rx_empty(rx_empty),
    .tx(tx),
    .r_data(r_data)
  );

  // --- Test helpers ---
  task automatic push_tx(input byte b);
    begin
      // wait for space
      @(posedge clk);
      wait (!tx_full);
      w_data <= b;
      wr_uart <= 1'b1;
      @(posedge clk);
      wr_uart <= 1'b0;
      $display("[%0t] Pushed TX byte 0x%02h", $time, b);
    end
  endtask

  task automatic pop_rx(output byte b);
    begin
      // wait until RX has data
      @(posedge clk);
      wait (!rx_empty);
      // FWFT FIFO: r_data already shows the head; rd pops it
      rd_uart <= 1'b1;
      @(posedge clk);
      b = r_data;    // sample after the pop edge
      rd_uart <= 1'b0;
      $display("[%0t] Popped  RX byte 0x%02h", $time, b);
    end
  endtask

  // Simple timeout guard for reads
  task automatic pop_rx_with_timeout(output byte b, input int cycles, output bit ok);
    begin
      ok = 0;
      repeat (cycles) begin
        @(posedge clk);
        if (!rx_empty) begin
          rd_uart <= 1'b1;
          @(posedge clk);
          b = r_data;
          rd_uart <= 1'b0;
          ok = 1;
          return;
        end
      end
    end
  endtask

  // --- Stimulus ---
  initial begin
    // defaults
    rd_uart = 0;
    wr_uart = 0;
    w_data  = '0;

    // Make ticks fast for sim:
    // tick repeats every (dvsr+1) clk cycles; we want it small.
    // With SB_TICK=16, each bit = 16 ticks; so choose small dvsr.
    dvsr = 4;   // tick ~ every 5 clk cycles

    // Reset
    reset = 1;
    repeat (5) @(posedge clk);
    reset = 0;
    $display("[%0t] Deassert reset", $time);

    // Send a few bytes
    push_tx(8'h55);
    push_tx(8'hA3);
    push_tx(8'h00);
    push_tx(8'hFF);

    // Read them back
    byte b; bit ok;
    pop_rx_with_timeout(b, 50000, ok); if (ok) $display("RX=%02h (exp 55)", b); else $fatal(1, "Timeout waiting RX #1");
    pop_rx_with_timeout(b, 50000, ok); if (ok) $display("RX=%02h (exp A3)", b); else $fatal(1, "Timeout waiting RX #2");
    pop_rx_with_timeout(b, 50000, ok); if (ok) $display("RX=%02h (exp 00)", b); else $fatal(1, "Timeout waiting RX #3");
    pop_rx_with_timeout(b, 50000, ok); if (ok) $display("RX=%02h (exp FF)", b); else $fatal(1, "Timeout waiting RX #4");

    $display("✅ Loopback test complete.");
    #2000 $finish;
  end

  // Optional: waveform dump
  initial begin
    $dumpfile("uart_tb.vcd");
    $dumpvars(0, tb_uart);
  end

endmodule

