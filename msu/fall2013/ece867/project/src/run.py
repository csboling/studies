from functools import partial
from itertools import cycle

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
channels = 1
samples = 3840
windowsize = 128
chunks = samples/windowsize
levels = [4]#range(1, int(np.log2(windowsize)))
wavelet = 'sym4'
filter_output = True

PLOT_FFT       = 0
PLOT_WAVELET   = 0
PLOT_RECON     = 1

PLOT_SNIPPETS    = 0
SHOW_BOGUS_SNIPS = 1
SHOW_ALL_SNIPS   = 1

def normalize(x):
  return (x / (np.max(np.abs(x))))

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
  print extracel.size
  bw_b, bw_a = scipy.signal.butter(1, (300/(0.5*Fs), 3000/(0.5*Fs)), 
                                   btype='band')
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
  
def pipeline(opts={}):
  outs = {}
  default_opts = {
    'Fs'         : 24000,
    'channels'   : 4,
    'windowsize' : 128,
    'chunks'     : 1,
    'wavelet'    : 'sym4',
    'levels'     : 2,
    'domain'     : 'wav',
    'k'          : 0.0005,
    'iterations' : 10,
    'discard'    : 0,
    'threshold'  : 0
  }

  for key in default_opts:
    if key not in opts:
      opts[key] = default_opts[key]
  
  converter = aic.cmux(opts['Fs'], opts['windowsize'], opts['wavelet'], 
                       opts['levels'], 
                       channels=opts['channels'], domain=opts['domain'],
                       discard=opts['discard'], threshold=opts['threshold'])

  outs['chipped']  = np.zeros((channels, opts['windowsize']*opts['chunks']))
  outs['triv']     = np.zeros((channels, opts['windowsize']*opts['chunks']))
  outs['alpha_in'] = np.zeros((channels, opts['chunks'], 
                               converter.Psi.shape[0]))
  outs['alpha']    = np.zeros((channels, opts['chunks'], 
                               converter.Psi.shape[0]))
  outs['bcr']      = np.zeros((channels, opts['chunks'], 
                               converter.Psi.shape[1]))
  outs['forward']  = converter.forward

  chipped_buffers  = cr.broadcast([cr.circbuf(outs['chipped'][i])
                                   for i in xrange(opts['channels'])])
  t_reconstructors = cr.broadcast([converter.triv_recon(i, 
                                     cr.circbuf(outs['triv'][i]))
                                   for i in xrange(opts['channels'])])
  ch_isolators     = cr.broadcast([converter.isolate(i, 
                                     converter.endpoint(outs['alpha'][i], 
                                                        outs['bcr'][i],
                                                        discard=True),
                                                    )
                                   for i in xrange(opts['channels'])])
  bcr_reconstructor = converter.bcr_recon(opts['levels'],  ch_isolators, 
                                          lasso=True, k=opts['k'], 
                                          iteration_cap=opts['iterations'])

  aic_targets = cr.broadcast([chipped_buffers,
                              t_reconstructors,
                              bcr_reconstructor])
  aic_out = converter.out(aic_targets)
  forward = cr.disperse([cr.accrue(opts['windowsize'],
                          converter.endpoint_dwt(outs['alpha_in'][i], None))
                         for i in xrange(opts['channels'])])
  aic_in  = cr.broadcast([aic_out, forward])
  outs['pipeline'] = aic_in
  return (converter, outs)

def main():
  #z = sine_inputs(Fs, channels, 5*Fs)
  z = extracel_inputs(Fs, channels, 5*Fs)
  #z = synthetic_input(windowsize, channels, 0.01, wavelet, levels) 

  bf_b, bf_a = scipy.signal.bessel(1, (3000/(0.5*Fs),), btype='low')
  known_spike0 = np.asarray(z[0])[26385:26385+windowsize*chunks]
  known_spike1 = np.asarray(z[1])[3600:3600 + windowsize*chunks]

  #plots.plot_wavelets(z0, wavelet, levels)
 
  test_data = z[..., :windowsize*chunks]
  test_data[0] = known_spike0
  error = []
  min_mse = 10000
  for level in levels: 
    opts = {'channels'  : channels,
            'windowsize': windowsize,
            'chunks'    : chunks,
            'k'         : 0.0005,
            'threshold' : 0,
            'discard'   : 0,
           }
    converter, outs = pipeline(opts)

    alpha_in = outs['alpha_in']
    chipped  = outs['chipped']
    t_recon  = outs['triv']
    alpha    = outs['alpha']
    signals  = outs['bcr']
    aic_out  = outs['pipeline']

    # push the data through
    for x in test_data.T:
      aic_out.send(x)
    test_data = test_data.reshape((channels, -1))
    t_recon   = t_recon.reshape((channels, -1))
    signals   = signals.reshape((channels, -1))

    if filter_output:
      filt = partial(scipy.signal.lfilter, bf_b, bf_a)
      filtered = np.apply_along_axis(filt, 1, signals)
    else:
      filtered = signals

    def magnitude(v):
      return np.max(np.abs(v))

    norm_thresholds = 0.7*np.ones(channels)
    in_thresholds   = norm_thresholds * magnitude(test_data)
    out_thresholds  = norm_thresholds * magnitude(filtered)

    norm_test_data = np.apply_along_axis(normalize, 1, test_data)
    test_data_scales = np.apply_along_axis(magnitude, 1, test_data)

    norm_t_recon   = np.apply_along_axis(normalize, 1, t_recon)
    t_recon_scales = np.apply_along_axis(magnitude, 1, t_recon)
    
    norm_signals   = np.apply_along_axis(normalize, 1, signals)
    signals_scales = np.apply_along_axis(magnitude, 1, signals)
    
    norm_filtered  = np.apply_along_axis(normalize, 1, filtered)
    filtered_scales = np.apply_along_axis(magnitude, 1, filtered)

    in_snippets     = converter.snippet(in_thresholds, test_data.T)
    print '%d input snippets' % len(in_snippets)
    recon_snippets  = converter.snippet(norm_thresholds, norm_filtered.T)
    print '%d BCR recon snippets' % len(recon_snippets)
 
    print
    print "Filter snippets:"
    for snip in in_snippets:
      for index in xrange(len(recon_snippets)):
        maybe_snip = recon_snippets[index]
        print "looking for ch %d pos %d, saw ch %d pos %d" \
              % (snip['ch'], snip['pos'],
                 maybe_snip['ch'], maybe_snip['pos'])
        if maybe_snip['ch'] == snip['ch']:
          norm_snip  = normalize(snip['snip'])
          mse = ((maybe_snip['snip'] - norm_snip)**2).mean()
          if mse < min_mse:
            min_mse = mse
            low_err_level = level
            best_in_snip  = norm_snip
            best_out_snip = maybe_snip
          error.append(mse)
          if SHOW_ALL_SNIPS:
            plt.figure()
          #  plt.title("channel %d, level %d, pos %d\nMSE %f" \
          #            % (snip['ch'], level, snip['pos'], mse))
            plt.plot(norm_snip, label='Original')
            plt.plot(maybe_snip['snip'], label='Reconstruction (%s)' 
                     % ('filtered' if filter_output else 'unfiltered')) 
            plt.legend(loc='best')
          del recon_snippets[index]
          break
        else:
          print 'bogus snippet: ch %d pos %d vs ch %d pos %d' \
                % (snip['ch'], snip['pos'], 
                   maybe_snip['ch'], maybe_snip['pos'])
          if SHOW_BOGUS_SNIPS:
            plt.figure()
            plt.plot(norm_snip, label='Original')
            plt.plot(maybe_snip['snip'],
                     label='Reconstruction (BOGUS)')
          #  plt.title('BOGUS SNIPPET on ch %d, pos %d' \
          #            % (maybe_snip['ch'], maybe_snip['pos']))

    if PLOT_RECON:
      for i in xrange(channels):
        plots.plot_aic(norm_test_data[i], alpha_in[i], 
                       norm_t_recon[i],     
                       alpha[i], norm_signals[i], norm_filtered[i])

#  if PLOT_SNIPPETS:
#    print 'Smallest MSE = %f (channel %d, level %d)' \
#          % (min_mse, best_out_snip['ch'], low_err_level)
#    if not SHOW_ALL_SNIPS:
#      plt.figure(1)
#      plt.title('Channel %d of %d\n%d levels\nMSE %f' 
#                % (best_in_snip['ch']+1, channels, low_err_level, min_mse))
#      plt.plot(best_in_snip['snip'],  label='Original')
#      plt.plot(best_out_snip['snip'], label='Reconstruction')
#      plt.legend(loc='best')
#  
#      plt.figure(2)
#      plt.plot(discards, error, label='filtered' if filter_output else 'unfiltered')
#      plt.plot(discards, error, 'x')
#      plt.title('Mean squared error\n%s' % wavelet)
#      plt.xlabel('\# detail coefficients discarded')
#      plt.ylabel('$\epsilon$')
#      plt.legend(loc='best')
  
  plt.show()

if __name__ == '__main__':
  main()
