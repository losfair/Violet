module top(input wire clk);

wire sysbus_i_io_ready;
wire [31:0] sysbus_i_io_data;

reg sysbus_i_fast_ready = 'b0;
wire [31:0] sysbus_i_fast_data;

wire sysbus_o_io_valid, sysbus_o_io_write;
wire [31:0] sysbus_o_io_addr, sysbus_o_io_data;

wire sysbus_o_fast_valid, sysbus_o_fast_write;
wire [31:0] sysbus_o_fast_addr;

// Deferred write
wire sysbus_o_fast_wr_valid;
wire [31:0] sysbus_o_fast_wr_addr;
wire [31:0] sysbus_o_fast_wr_data;
wire [3:0] sysbus_o_fast_wr_mask;

reg sysbus_i_ic_refill_valid = 'b0;
wire [31:0] sysbus_i_ic_refill_addr;
wire [31:0] sysbus_i_ic_refill_data;
reg sysbus_i_ic_refill_ready = 'b0;

wire sysbus_o_ic_refill_valid;
wire [31:0] sysbus_o_ic_refill_addr;

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
    .sysbus_i_io({sysbus_i_io_ready, sysbus_i_io_data}),
    .sysbus_o_io({sysbus_o_io_valid, sysbus_o_io_write, sysbus_o_io_addr, sysbus_o_io_data}),
	.sysbus_i_fast({sysbus_i_fast_ready, sysbus_i_fast_data}),
    .sysbus_o_fast({
		sysbus_o_fast_valid, sysbus_o_fast_write, sysbus_o_fast_addr,
		sysbus_o_fast_wr_valid, sysbus_o_fast_wr_addr, sysbus_o_fast_wr_data, sysbus_o_fast_wr_mask
	}),
	.sysbus_i_ic_refill_valid(sysbus_i_ic_refill_valid),
	.sysbus_i_ic_refill_addr(sysbus_i_ic_refill_addr),
	.sysbus_i_ic_refill_data(sysbus_i_ic_refill_data),
	.sysbus_i_ic_refill_ready(sysbus_i_ic_refill_ready),
	.sysbus_o_ic_refill_valid(sysbus_o_ic_refill_valid),
	.sysbus_o_ic_refill_addr(sysbus_o_ic_refill_addr)
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

// Data memory
reg [31:0] store[65535:0];
reg [31:0] next_addr;
assign sysbus_i_fast_data = store[next_addr[17:2]];

initial begin
	$readmemh("dm.txt", store);
end

always @ (posedge clk) begin
	next_addr <= sysbus_o_fast_addr;

	if(sysbus_o_fast_addr[31:28] == 4'hf) sysbus_i_fast_ready <= 'b0;
	else sysbus_i_fast_ready <= 'b1;

	if(sysbus_o_fast_wr_valid) begin
		if(sysbus_o_fast_wr_mask[0]) store[sysbus_o_fast_wr_addr[17:2]][7:0] <= sysbus_o_fast_wr_data[7:0];
		if(sysbus_o_fast_wr_mask[1]) store[sysbus_o_fast_wr_addr[17:2]][15:8] <= sysbus_o_fast_wr_data[15:8];
		if(sysbus_o_fast_wr_mask[2]) store[sysbus_o_fast_wr_addr[17:2]][23:16] <= sysbus_o_fast_wr_data[23:16];
		if(sysbus_o_fast_wr_mask[3]) store[sysbus_o_fast_wr_addr[17:2]][31:24] <= sysbus_o_fast_wr_data[31:24];
	end
end


// I$ refill
// I$ line is 32 bytes (8 cycles to refill)
reg [31:0] refill_counter = 'b0;
reg [31:0] refill_end = 'b0;
assign sysbus_i_ic_refill_addr = refill_counter;
assign sysbus_i_ic_refill_data = store[refill_counter[17:2]];

always @ (posedge clk) begin
	//$display("time %0d sysbus_o_ic_refill_valid=%b", $time, sysbus_o_ic_refill_valid);
	if(!sysbus_o_ic_refill_valid) begin
		if(sysbus_i_ic_refill_ready) $display("%0d refill ack", $time);
		sysbus_i_ic_refill_ready <= 'b0;
	end else if(sysbus_o_ic_refill_valid && !sysbus_i_ic_refill_ready) begin
		if(!sysbus_i_ic_refill_valid) begin
			$display("%0d refill begin: addr=0x%0x", $time, sysbus_o_ic_refill_addr);
			refill_counter <= sysbus_o_ic_refill_addr;
			refill_end <= sysbus_o_ic_refill_addr + 'd28; // 28 = 32 - 4
			sysbus_i_ic_refill_valid <= 'b1;
		end else begin
			$display("%0d refilled: 0x%0x -> 0x%0x", $time, refill_counter, sysbus_i_ic_refill_data);
			if(refill_counter == refill_end) begin
				sysbus_i_ic_refill_valid <= 'b0;
				sysbus_i_ic_refill_ready <= 'b1;
			end else begin
				refill_counter <= refill_counter + 'd4;
			end
		end
	end
end

/*
always @ (posedge clk) begin
	if(sysbus_o_fast_wr_valid) $display("%0d sysbus_o_fast_wr_valid mask=%b addr=0x%0x data=0x%0x", $time, sysbus_o_fast_wr_mask, sysbus_o_fast_wr_addr, sysbus_o_fast_wr_data);
	if(sysbus_o_fast_valid) $display("%0d sysbus_o_fast_valid addr=0x%0x", $time, sysbus_o_fast_addr);
end
*/
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
