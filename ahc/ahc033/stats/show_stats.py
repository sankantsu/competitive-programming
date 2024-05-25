import pathlib
import numpy as np
import matplotlib.pyplot as plt


N: int = 5


def load():
    out_dir = pathlib.Path("../out/")
    data = []
    for out in out_dir.iterdir():
        with open(out) as f:
            turn = 10000
            for i in range(N):
                line = f.readline()
                turn = min(turn, len(line))
            data.append((str(out), turn))
    return data


def main():
    data = load()
    files, turns = zip(*data)
    valid_turns = list(filter(lambda t: 0 < t and t < 1000, turns))
    n_success = len(valid_turns)

    best = np.min(valid_turns)
    worst = np.max(valid_turns)
    mean = np.mean(valid_turns)
    gmean = np.exp(np.mean(np.log(valid_turns)))
    print(f"Success: {n_success}")
    print(f"Failed: {len(turns) - n_success}")
    print(f"Best: {best}, Worst: {worst}")
    print(f"Mean: {mean:.2f}, Geometric Mean: {gmean:.2f}")
    plt.hist(valid_turns, bins=50)
    plt.xlabel("Turns")
    plt.ylabel("Occurences")
    plt.show()


if __name__ == "__main__":
    main()
