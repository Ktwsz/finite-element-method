import matplotlib.pyplot as plt


def load_from_file(file_name):
    f = open(file_name, "r")
    res = [[], []]

    for line in f:
        x, y = [float(s) for s in line.strip("()\n").split(",")]
        res[0].append(x)
        res[1].append(y)

    return res


fig, ax = plt.subplots()

ax.plot(*load_from_file("wynik/wynik.txt"), linewidth=1)

fig.savefig("wynik/wynik.png")
