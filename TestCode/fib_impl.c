int fib(int n) {
    if(n == 1 || n == 2) return 1;
    else return fib(n - 1) + fib(n - 2);
}
