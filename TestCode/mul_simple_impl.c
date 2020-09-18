int do_mul(int a, int b, int c) {
    int result;
    for(int i = 0; i < c; i++) result += a * b + 1;
    return result;
}
