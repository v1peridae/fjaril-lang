import cutecore

while True:
    text = input('cutecore')
    result, error = cutecore.run(text)

    if error:
        print(error.as_string())
    else:
        print(result)
