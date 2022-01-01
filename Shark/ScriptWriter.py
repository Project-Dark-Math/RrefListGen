f = open("Script", "w")

while True:
    try:
        line = input()
        f.write(line)
        f.write("\n")
    except:
        f.close()
        break
