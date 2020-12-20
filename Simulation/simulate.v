module simulate;

reg clk;

top top_0(
    .clk(clk)
);

initial begin
    clk = 'b0;
end

always begin
    #5 clk = !clk;
end

endmodule