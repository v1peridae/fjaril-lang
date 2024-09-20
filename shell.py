import fjaril

while True:
    text = input('fjaril >>> ')
    result, error = fjaril.run('<stdin>', text)

    if error:
        print(error.as_string())
    else:
        print(result)
