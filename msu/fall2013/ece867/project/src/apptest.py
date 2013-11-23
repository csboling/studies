import numpy as np

a = np.zeros((4,12))
a[0] = np.arange(12)

def test(col):
  return np.dot(col, [4,3,2,1])

print test(a[...,0])
print np.apply_along_axis(test, 0, a)
