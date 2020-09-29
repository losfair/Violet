module top(input wire clk);

wire sysbus_i_io_ready;
wire [31:0] sysbus_i_io_data;

wire sysbus_o_io_valid, sysbus_o_io_write;
wire [31:0] sysbus_o_io_addr, sysbus_o_io_data;

wire pc1_valid;
wire [31:0] pc1;

wire wp1_valid;
wire [4:0] wp1_i;
wire [31:0] wp1_v;

wire pc2_valid;
wire [31:0] pc2;

wire wp2_valid;
wire [4:0] wp2_i;
wire [31:0] wp2_v;

VioletCore core_inst(
    .clk(clk),
    .rst(1'b0),
    .en(1'b1),
    .commit({pc1_valid, pc1, wp1_valid, wp1_i, wp1_v, pc2_valid, pc2, wp2_valid, wp2_i, wp2_v}),
    .sysbus_i({sysbus_i_io_ready, sysbus_i_io_data}),
    .sysbus_o({sysbus_o_io_valid, sysbus_o_io_write, sysbus_o_io_addr, sysbus_o_io_data})
);

io io_inst(
    .clk(clk),
    .sysbus_i_io_ready(sysbus_i_io_ready),
    .sysbus_i_io_data(sysbus_i_io_data),
    .sysbus_o_io_valid(sysbus_o_io_valid),
    .sysbus_o_io_write(sysbus_o_io_write),
    .sysbus_o_io_addr(sysbus_o_io_addr),
    .sysbus_o_io_data(sysbus_o_io_data)
);

always @ (posedge clk) begin
	if(!pc1_valid) $write("(bubble)");
	else begin
		$write("[0x%8x]<", pc1);
		if(!wp1_valid) $write("no_write>");
		else $write("write:%0d=0x%8x>", wp1_i, wp1_v);
	end

	$write(" ");

	if(!pc2_valid) $write("(bubble)");
	else begin
		$write("[0x%8x]<", pc2);
		if(!wp2_valid) $write("no_write>");
		else $write("write:%0d=0x%8x>", wp2_i, wp2_v);
	end
	$write("\n");
end

endmodule
