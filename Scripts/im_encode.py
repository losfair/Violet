import sys
import binascii

def mkBinStr(x):
    s = ""
    for j in range(0, 64):
        i = 64 - j - 1
        if (x >> i) & 1 == 1:
            s += "1"
        else:
            s += "0"
    return s

data = sys.stdin.buffer.read()
while len(data) % 8 != 0:
    data += b"\x00"

for i in range(0, 65536 * 4, 8):
    if i < len(data):
        x = int.from_bytes(data[i:i+8], "little")
    else:
        x = 0
    print(mkBinStr(x))
