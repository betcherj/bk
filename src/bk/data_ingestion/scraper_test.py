from bk.utils import data
import pandas as pd

def data_test():
    YEAR = 2020
    data = pd.read_csv('https://github.com/guga31bb/nflfastR-data/blob/master/data/' \
                             'play_by_play_' + str(YEAR) + '.csv.gz?raw=True',
                             compression='gzip', low_memory=False)
    print(data)


if __name__ == "__main__":
    data_test()

