import math

def bernoulli_H(p):
  return (-p*math.log(p) - (1-p)*math.log(1-p))

def nmin(n, p, eps):
  denom = math.log((1-p)/p)
  if (p < 1-p):
    num = n*(bernoulli_H(p) - eps + math.log(1-p))
  else:
    num = n*(bernoulli_H(p) + eps + math.log(1-p))
  return math.ceil(num/denom)

def nmax(n, p, eps):
  return nmin(n, p, -eps)
