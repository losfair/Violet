import sys
import binascii

data = sys.stdin.buffer.read()

# Alignment for I$
while len(data) % 32 != 0:
    data += b"\x00"

for i in range(0, len(data), 4):
    if i < len(data):
        part = bytes([
            data[i+3], data[i+2], data[i+1], data[i]
        ])
    else:
        part = bytes([0, 0, 0, 0])
    print(binascii.hexlify(part).decode("utf-8"))
