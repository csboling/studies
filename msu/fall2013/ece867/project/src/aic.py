import sys, logging

import numpy as np
from sklearn import linear_model
import pywt

import bitstring

import coroutine as cr

logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)

timestep_unit = 10000 #us
rate_unit     = 1

maximal_taps = {
  3:   [3,2],
  4:   [4,3],
  5:   [5,3],
  6:   [6,5],
  7:   [7,6],
  8:   [8,6,5,4],
  9:   [9,5],
  10:  [10,7],
  11:  [11,9],
  12:  [12,6,4,1],
  13:  [13,4,3,1],
  14:  [14,5,3,1],
  15:  [15,14],
  16:  [16,15,13,4],
  17:  [17,14],
  18:  [18,11],
  19:  [19,6,2,1],
  20:  [20,17],
  21:  [21,19],
  22:  [22,21],
  23:  [23,18],
  24:  [24,23,22,17],
  25:  [25,22],
  26:  [26,6,2,1],
  27:  [27,5,2,1],
  28:  [28,25],
  29:  [29,27],
  30:  [30,6,4,1],
  31:  [31,28],
  32:  [32,22,2,1],
}

def lfsr(bits, taps, direction=1):
  newbit = bitstring.BitArray('0b0')
  for tap in taps:
    if direction:
      newbit[0] ^= bits[-tap]
    else:
      newbit[0] ^= bits[tap-1]
  if direction:
    bits = bits[1:] + newbit
  else:
    bits = newbit + bits[:-1]
  return bits

def haar_A(N):
  return np.kron(np.eye(N/2), [1,1])

def haar_D(N):
  return np.kron(np.eye(N/2), [1,-1])

def haarmat(N, level=1):
  assert level >= 1
  assert N % 2 == 0
  if level == 1:
    return ((np.sqrt(2)/2)*np.vstack((haar_A(N), haar_D(N))))
  else:
    return np.dot(haarmat(N/2, level-1), haar_A(N))
 
def dwtmat(N, wavelet, level=1):
  def fwd_dwt(x):
    return np.hstack(pywt.wavedec(x, wavelet, mode='per', level=level)[0:2])
  return np.apply_along_axis(fwd_dwt, 1, np.eye(N)).T

def threshold(x, thresh=10.0):
  if (abs(x) > thresh):
    return x
  else:
    return 0

def cycle_random(sets):
  while True:
    yield np.random.randint(0, sets)

def cycle_blocks(levels, sets):
  size = sets
  upstep   = size/2 + 1
  downstep = size - upstep

  m, shift = 0, 0
  while True:
    shift += upstep
    m = (m + shift) % sets
    yield m
    shift -= downstep
    m = (m + shift) % sets
    yield m

class cmux:
  def __init__(self, chiprate, windowsize, wavelet, levels, channels=96, period=32):
    self.wavelet    = wavelet
    self.levels     = levels
    self.windowsize = windowsize

    (self.Psi, self.Psi_inv) = self.get_matrix()

    self.channels   = channels
    self.chiprate   = chiprate #Hz
    self.chipT      = timestep_unit / (self.chiprate * rate_unit)
    self.taps       = maximal_taps[period]

    self.seed       = '0b' + '1'*period
    self.chipseed    = np.array([[1,-1]*(self.channels/2)])

  def get_matrix(self):
    D = []
    for i in xrange(self.levels):
      H = dwtmat(self.windowsize, self.wavelet, i+1)
      newA, newD = np.split(H,2)
      D.insert(0, newD)
    Psi_inv = np.vstack((newA, np.vstack(D)))
    Psi     = np.linalg.inv(Psi_inv)

    return (Psi, Psi_inv)

  def chip(self):
    time = 0
    lfsrval = bitstring.BitArray(self.seed)
    chip    = self.chipseed
    while True:
      if time >= self.chipT:
        time = 0
        lfsrval = lfsr(lfsrval, self.taps)
      else:
        time += 1
      chip      = np.roll(chip, 1)
      chip[0,0] = 2*int(lfsrval.bin[-1])-1
      yield chip

  @cr.coroutine
  def out(self, target):
    in_chip = self.chip()
    while True:
      invals = (yield)
      target.send(np.dot(in_chip.next(), invals))

  @cr.coroutine
  def bypass(self, target, channel):
    while True:
      invals = (yield)
      target.send(invals[channel])

  def snippet(self, thresholds, data, before=15, after=24):
    pos = 0
    q   = []
    post_ct = [0]*self.channels
    for samples in data:
      if pos < before:
        pass
      else:
        for ch in xrange(self.channels):
          if post_ct[ch] == 0:
            if abs(samples[ch]) > thresholds[ch]:
              post_ct[ch] += 1
              new = {'ch'   : ch,
                     'pos'  : pos,
                     'snip' : data[pos-before:pos+after, ch]}
              q.append(new)
          else:
            if post_ct[ch] == after:
              post_ct[ch] = 0
            else:
              post_ct[ch] += 1
      pos += 1
    return q

  @cr.coroutine
  def triv_recon(self, ch, target):
    out_chip = self.chip()
    while True:
      v = (yield)
      new_chip = out_chip.next().T
      z = np.dot(v, new_chip[ch])
      target.send(z)

  @cr.coroutine
  def bcr_recon(self, levels, target, 
                lasso=True, k=0.005, iteration_cap=500):
    numcoefs   = self.Psi.shape[0]

    y = np.zeros(self.windowsize)
    window = cr.circbuf(y)

#    ch_it = cycle_random(self.channels)
#    ch_it = cycle_blocks(levels=levels, sets=self.channels)
    ch_it = xrange(iteration_cap)

    alpha0_ridge = linear_model.Ridge()
    alpha_lasso  = linear_model.Lasso(alpha=k)

    in_chip = self.chip()
    chips = np.zeros((self.channels, self.windowsize))

    I = np.eye(self.windowsize)
    while True:
      # get a vector of windowsize samples
      for i in xrange(self.windowsize):
        window.send((yield))      
        chips[..., i] = in_chip.next()
      w = y
 
      # construct the dictionary from chip sequences
      Phi = np.zeros((self.channels, numcoefs, self.windowsize))
      A   = np.zeros((numcoefs, self.windowsize*self.channels))
      for ch in xrange(self.channels):
        Phi[ch] = np.dot(chips[ch]*I, self.Psi)
        A[...,ch*self.windowsize:(ch+1)*self.windowsize] = Phi[ch]
 
      alpha0_ridge.fit(A, w)    
      alpha = alpha0_ridge.coef_
       
      if lasso:
        iterations = 0
        try:
          for ch in ch_it:
            ch = ch % self.channels
            #print 'iteration',iterations,', channel',ch
            A_before = (A.T[:ch*self.windowsize]).T
            A_after  = (A.T[(ch+1)*self.windowsize:]).T
            alpha_before = alpha[:ch*self.windowsize]
            alpha_after  = alpha[(ch+1)*self.windowsize:]
  
            A_minor = np.hstack((A_before, A_after))
            alpha_m = np.hstack((alpha_before, alpha_after))
  
            v = w - np.dot(A_minor, alpha_m)
            alpha_lasso.fit(Phi[ch], v)
 
            alpha[ch*self.windowsize:(ch+1)*self.windowsize] = alpha_lasso.coef_
    
            if np.max(alpha) == 0:
              print 'All coefficients driven to zero, stopping.'
              break
            if iterations > iteration_cap:
              break
            iterations += 1
        except KeyboardInterrupt:
          pass
      target.send((Phi, alpha))
     
  @cr.coroutine
  def isolate(self, ch, target):
    while True:
      Phi, alpha = (yield)
      x = alpha[ch*self.windowsize:(ch+1)*self.windowsize]
      target.send(x)

  @cr.coroutine
  def undo_dwt(self, target):
    while True:
      reconD = []
      rem = (yield)
      for i in xrange(self.levels):
        rem, newD = np.split(rem, 2)
        reconD.insert(0,newD)
      dwt_coefs = [rem] + reconD
      dwt_recon = pywt.waverec(dwt_coefs, self.wavelet, mode='per')
      target.send(dwt_recon)

  @cr.coroutine
  def endpoint(self, A, X):
    alpha_end = cr.circbuf(A)
    recon_end = self.undo_dwt(cr.circbuf(X))
    while True:
      alpha = (yield)
      alpha_end.send(alpha)
      recon_end.send(alpha)

