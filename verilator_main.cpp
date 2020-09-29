#include "Vtop.h"
#include "verilated.h"

vluint64_t main_time = 0;
double sc_time_stamp() { return main_time; }

int main(int argc, char **argv, char **env)
{
    int clk = 0;
        Verilated::commandArgs(argc, argv);

        Vtop* top = new Vtop;
        while (!Verilated::gotFinish())
        {
                top->clk = clk;
                top->eval();
                clk ^= 1;
                main_time++;
        }

        delete top;
        exit(0);
}
