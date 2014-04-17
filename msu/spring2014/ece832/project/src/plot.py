import sys

import numpy as np
import matplotlib.pyplot as plt

def main(bits, fname):
  data = np.genfromtxt(fname, dtype=None, delimiter=' ')
  
  plt.hold(True)
  vin = np.dot(data[0:, 0] - 1.2, 2**bits / (3 - 1.2))
  response = data[0:, 1]
  
  plt.plot(vin, vin)
  plt.plot(vin, response)

  plt.show()

if __name__ == '__main__':
  if len(sys.argv) > 2:
    main(int(sys.argv[1]), sys.argv[2])
  else:
    main('data.csv')

