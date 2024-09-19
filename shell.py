import cutecore

while True:
    text = input('cutecore >>>')
    result, error = cutecore.run('<stdin>', text)

    if error:
        print(error.as_string())
    else:
        print(result)
