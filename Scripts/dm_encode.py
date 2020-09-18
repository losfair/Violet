import sys
import binascii

def mkBinStr(x):
    s = ""
    for j in range(0, 8):
        i = 8 - j - 1
        if (x >> i) & 1 == 1:
            s += "1"
        else:
            s += "0"
    return s

data = sys.stdin.buffer.read()
out_prefix = sys.argv[1]

outs = [open(out_prefix + "." + str(i) + ".txt", "wb") for i in range(0, 4)]

for i in range(0, 65536):
    if i < len(data):
        x = data[i]
    else:
        x = 0
    outs[i % 4].write((mkBinStr(x) + "\n").encode("utf-8"))
