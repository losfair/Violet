module io(
    input wire clk,

    output reg sysbus_i_io_ready = 'b0,
    output reg [31:0] sysbus_i_io_data,

    input wire sysbus_o_io_valid,
    input wire sysbus_o_io_write,
    input wire [31:0] sysbus_o_io_addr,
    input wire [31:0] sysbus_o_io_data
);

reg [63:0] cycles = 'd0;
always @ (posedge clk) cycles <= cycles + 'd1;

always @ (posedge clk) begin
    if(!sysbus_o_io_valid) sysbus_i_io_ready <= 'b0;
    else if(sysbus_o_io_valid && !sysbus_i_io_ready) begin
        case (sysbus_o_io_addr)
        'hfe000000: begin
            $fwrite(32'h8000_0002, "%c", sysbus_o_io_data[7:0]);
            sysbus_i_io_data <= 'b0;
            sysbus_i_io_ready <= 'b1;
        end
        'hfe000004: begin
            $display("Termination requested");
            $finish();
        end
        'hfe000010: begin
            sysbus_i_io_data <= cycles[31:0];
            sysbus_i_io_ready <= 'b1;
        end
        'hfe000014: begin
            sysbus_i_io_data <= cycles[63:32];
            sysbus_i_io_ready <= 'b1;
        end
        default: begin
            $display("BAD IO ADDR: 0x%0x", sysbus_o_io_addr);
            $finish();
        end
        endcase
    end
end

endmodule
