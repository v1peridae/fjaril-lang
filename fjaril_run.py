import sys
import fjaril

if len(sys.argv) != 2:
    print("Usage: python cutecore_run.py <filename.owo>")
    sys.exit(1)

filename = sys.argv[1]
if not filename.endswith('.owo'):
    print("Error: File must have .owo extension")
    sys.exit(1)

with open(filename, 'r') as file:
    content = file.read()

result, error = fjaril.run(filename, content)

if error:
    print(error.as_string())
else:
    print(result)
