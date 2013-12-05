from functools import partial

import numpy as np
import scipy.io, scipy.signal

import matplotlib.pyplot as plt
from   matplotlib import rc

import pywt

import aic
import coroutine as cr
import plots


rc('text', usetex=True)
Fs = 24000.0
channels = 4
windowsize = 128
levels = range(1, int(np.log2(windowsize)))
wavelet = 'sym4'
filter_output = False

PLOT_FFT      = 0
PLOT_WAVELET  = 0
PLOT_RECON    = 1
PLOT_SNIPPETS = 1

def plot_fft(sig):
  z0 = np.squeeze(sig)
  Fz = np.fft.fft(z0)

  freq = np.fft.fftfreq(z0.size)
  plt.plot(freq, np.abs(Fz))

def sine_inputs(Fs, channels, duration):
  sinecount = np.floor(5*np.random.rand(channels)) + 1
  max_sines = np.max(sinecount)
  f = np.zeros((channels, max_sines))
  for i in xrange(f.shape[0]):
    f[i] = Fs/2 * np.hstack((np.random.rand(sinecount[i]), 
                             [0]*(max_sines-sinecount[i])))
  coeffs = 5 * np.random.rand(channels, max_sines)
  w = f/Fs

  t  = np.arange(int(duration))
  ys = np.array([[np.sin(2*np.pi*w0*t) for w0 in ch] for ch in w])
  return np.dot(coeffs, ys)[0,...,...]

def extracel_inputs(Fs, channels, duration):
  '''
  import Quiroga et al's simulated data set 
  (from http://www2.le.ac.uk/departments/engineering/research/
                bioengineering/neuroengineering-lab/software)
  and filter it between 500 and 7500 Hz to examine spike data
  '''
  extracel = scipy.io.loadmat('quiroga_simulation_1.mat')['data'][0,...]
  bw_b, bw_a = scipy.signal.butter(1, (300/(0.5*Fs), 3000/(0.5*Fs)), btype='band')
  ec_filtered = scipy.signal.lfilter(bw_b, bw_a, extracel)

  t  = np.arange(int(duration))
  return np.array([ec_filtered[t.size*i + t] for i in xrange(channels)])

def synthetic_input(windowsize, channels, sparsity, wavelet, levels):
  ret = np.zeros((channels, windowsize))
  nonzeros = int(sparsity*windowsize)
  coefs = np.zeros(windowsize)
  for ch in xrange(channels):
    mag = 50.0
    for i in xrange(nonzeros):
      j = np.random.randint(windowsize)
      coefs[j] = mag
      mag /= 2

    reconD = []
    rem = coefs
    for i in xrange(levels):
      rem, newD = np.split(rem, 2)
      reconD.insert(0,newD)
    dwt_coefs = [rem] + reconD
    ret[ch] = pywt.waverec(dwt_coefs, wavelet, mode='per')
    coefs[...] = 0
  return ret
  
def main():
  #z = sine_inputs(Fs, channels, 5*Fs)
  z = extracel_inputs(Fs, channels, 5*Fs)
  #z = synthetic_input(windowsize, channels, 0.01, wavelet, levels) 

  known_spike = np.asarray(z[0])[26385:26385+windowsize]

  #plots.plot_wavelets(z0, wavelet, levels)

  error = []
  min_mse = 10000
  low_err_level = 0
  for level in levels:
    test_data = z[..., :windowsize]
    test_data[0] = known_spike
    chipped = np.zeros(test_data.shape)
    t_recon = np.zeros(test_data.shape)
  
    converter = aic.cmux(Fs, windowsize, wavelet, level, channels=channels)
    alpha     = np.zeros((channels, 1, converter.Psi.shape[0]))
    signals   = np.zeros((channels, 1, converter.Psi.shape[1]))

    # build a pipeline
    chipped_buffers  = cr.broadcast([cr.circbuf(chipped[i])
                                     for i in xrange(channels)])
    t_reconstructors = cr.broadcast([converter.triv_recon(i, cr.circbuf(t_recon[i]))
                                     for i in xrange(channels)])
    ch_isolators     = cr.broadcast([converter.isolate(i, converter.endpoint(alpha[i], signals[i]))
                                     for i in xrange(channels)])
    bcr_reconstructor = converter.bcr_recon(level,  ch_isolators, lasso=True, 
                                            k=0.0005, iteration_cap=20*channels)

    aic_targets = cr.broadcast([chipped_buffers,
                                t_reconstructors,
                              bcr_reconstructor])
    #aic_out = converter.bypass(aic_targets, 0)
    aic_out     = converter.out(aic_targets)

    alpha_in        = np.dot(converter.Psi_inv, test_data.T).T
    # push the data through
    for x in test_data.T:
      aic_out.send(x)

    bf_b, bf_a = scipy.signal.bessel(1, (3000/(0.5*Fs),), btype='low')
    if filter_output:
      filtered = [scipy.signal.lfilter(bf_b, bf_a, s) for s in signals.squeeze()]
    else:
      filtered = signals.squeeze()

    norm_thresholds = 0.9*np.ones(channels)
    test_data /= np.max(np.abs(test_data))
    t_recon   /= np.max(np.abs(t_recon))
    signals   /= np.max(np.abs(signals))
    filtered  /= np.max(np.abs(filtered))
  
    in_snippets     = converter.snippet(norm_thresholds, test_data.T)
    print '%d input snippets' % len(in_snippets)
    recon_snippets  = converter.snippet(norm_thresholds, filtered.T)
    print '%d BCR recon snippets' % len(recon_snippets)
  
    if PLOT_SNIPPETS:
      for snip in in_snippets:
        maybe_snip = None
        while recon_snippets:
          maybe_snip = recon_snippets.pop(0)
          if maybe_snip['ch'] == snip['ch']:
            break
          else:
            print 'bogus snippet: ch %d pos %d vs ch %d pos %d' \
                  % (snip['ch'], snip['pos'], maybe_snip['ch'], maybe_snip['pos'])
        if maybe_snip == None:
          print 'no reconstruction found for ch %d pos %d' % (snip['ch'], snip['pos'])
        else:
          mse = ((maybe_snip['snip'] - snip['snip'])**2).mean()
          if mse < min_mse:
            min_mse = mse
            low_err_level = level
            best_in_snip  = snip
            best_out_snip = maybe_snip
          error.append(mse)
    else:
      for i in xrange(channels):
        plots.plot_aic(test_data[i], alpha_in[i], 
                       t_recon[i],     
                       alpha[i][0], signals[i][0], filtered[i])
  
  
  print 'Smallest MSE =',min_mse
  plt.figure()
  plt.title('Channel %d of %d\n%d levels\nMSE %f' % (best_in_snip['ch']+1, channels, low_err_level, min_mse))
  plt.plot(best_in_snip['snip'],  label='Original')
  plt.plot(best_out_snip['snip'], label='Reconstruction')
  plt.legend(loc='best')

  plt.figure()
  plt.plot(levels, error,)
  plt.plot(levels, error, 'x')
  plt.title('Mean squared error\n%s\n%s' % (wavelet, 'filtered' if filter_output else 'unfiltered'))
  plt.xlabel('# decomposition levels')
  plt.ylabel('$\epsilon$')
  
  plt.show()

if __name__ == '__main__':
  main()
