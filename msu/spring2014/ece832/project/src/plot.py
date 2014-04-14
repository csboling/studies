import sys

import numpy as np
import matplotlib.pyplot as plt

def main(fname):
  data = np.genfromtxt(fname, dtype=None, delimiter=' ')
  
  plt.hold(True)
  plt.plot(data[0:,0], np.dot(data[0:,0] - 1.2, 2**4 / (3 - 1.2)))
  plt.plot(data[0:,0], data[0:,1])
  plt.show()

if __name__ == '__main__':
  if len(sys.argv) > 1:
    main(sys.argv[1])
  else:
    main('data.csv')

