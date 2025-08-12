`timescale 1ns/1ps

module tb_uart;

  localparam int DBIT    = 8;
  localparam int SB_TICK = 16;
  localparam int FIFO_W  = 3;

  logic clk = 0;
  logic reset;

  logic rd_uart, wr_uart;
  logic [7:0] w_data;
  wire  [7:0] r_data;
  wire        tx_full, rx_empty;

  wire tx;
  wire rx;

  logic [10:0] dvsr;

  logic [7:0] b;
  bit ok;

  always #5 clk = ~clk;

  assign rx = tx;

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

  task automatic push_tx(input logic [7:0] x);
    begin
      @(posedge clk);
      wait (!tx_full);
      w_data <= x;
      wr_uart <= 1'b1;
      @(posedge clk);
      wr_uart <= 1'b0;
      $display("[%0t] Pushed TX byte 0x%02h", $time, x);
    end
  endtask

  task automatic pop_rx_with_timeout(output logic [7:0] x, input int cycles, output bit ok_o);
    begin
      ok_o = 0;
      repeat (cycles) begin
        @(posedge clk);
        if (!rx_empty) begin
          rd_uart <= 1'b1;
          @(posedge clk);
          x = r_data;
          rd_uart <= 1'b0;
          ok_o = 1;
          return;
        end
      end
    end
  endtask

  initial begin
    rd_uart = 0;
    wr_uart = 0;
    w_data  = '0;
    dvsr = 4;

    reset = 1;
    repeat (5) @(posedge clk);
    reset = 0;
    $display("[%0t] Deassert reset", $time);

    push_tx(8'h55);
    push_tx(8'hA3);
    push_tx(8'h00);
    push_tx(8'hFF);

    pop_rx_with_timeout(b, 50000, ok); if (ok) $display("RX=%02h (exp 55)", b); else $fatal(1, "Timeout waiting RX #1");
    pop_rx_with_timeout(b, 50000, ok); if (ok) $display("RX=%02h (exp A3)", b); else $fatal(1, "Timeout waiting RX #2");
    pop_rx_with_timeout(b, 50000, ok); if (ok) $display("RX=%02h (exp 00)", b); else $fatal(1, "Timeout waiting RX #3");
    pop_rx_with_timeout(b, 50000, ok); if (ok) $display("RX=%02h (exp FF)", b); else $fatal(1, "Timeout waiting RX #4");

    #2000 $finish;
  end

  initial begin
    $dumpfile("uart_tb.vcd");
    $dumpvars(0, tb_uart);
  end

endmodule


