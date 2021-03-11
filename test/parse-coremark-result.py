import sys

data = sys.stdin.read().split("\n")
total_ticks = None
iterations = None

for line in data:
  parts = line.split(":", 1)
  if len(parts) == 2:
    key = parts[0].strip()
    value = parts[1].strip()
    if key == "Total ticks":
      total_ticks = int(value)
    elif key == "Iterations":
      iterations = int(value)

print("{} iterations in {} ticks".format(iterations, total_ticks))
print("CoreMark/MHz: {:.2f}".format(iterations * 1000000 / total_ticks))
