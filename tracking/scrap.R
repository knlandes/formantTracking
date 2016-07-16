

v = vowelsynth (ffs = c(300,1900,2400,3400,4400))
play (v)

spectrogram (v)

ffs = findformants (v)
ffs



formanttrack (v, formants = 2)


coeffs = lpc (v, show = TRUE)

freqresponse (1, coeffs)
polezero (1, coeffs)



freqresponse (1, c(1, -.5, .8))

noise = rnorm (1000)

spectralslice (noise)

noise2 = filter (noise, filter = .5, method = 'recursive')

output = c(0,0)
for (i in 3:length (noise)) output[i] = noise[i] + output[i-1]*.5 - output[i-2]*.8

spectralslice (output)


spectralslice (noise2)


y = noise[1:998]
x1 = noise[2:999]
x2 = noise[3:1000]

lm (x2 ~ 1 + x1 + y)




lpc = function (sound, order = round(fs/1000)+3, fs = 10000, show = FALSE, add = FALSE, 
                preemph = TRUE){
  if (class(sound) == "sound") {
    fs = sound$fs
    sound = sound$sound
  }
  
  if (!is.numeric(sound)) stop ('Input must be numeric.')
  if (preemph == TRUE) sound = preemphasis (sound, fs = fs)
  sound = sound - mean (sound)
  sound = sound * windowfunc(length(sound), type = 'hanning')
  tmp = acf(sound, lag.max = length(sound), plot = FALSE)$acf
  
  n = length (sound)
  y = sound[(order+1):n]
  predictors = sapply (seq(1,length(sound)-order, 1), function (x) sound[(x):(x+order-1)])
  predictors = as.matrix(t(predictors))
  mod = lm (y ~ predictors)
  coefficients = as.numeric(rev(mod$coefficients[-1]))
  coefficients = c(1, -coefficients)
  if (show == TRUE & add == TRUE) freqresponse (1, coefficients, fs = fs, add = add)
  if (show == TRUE & add == FALSE){
    freqresponse (1, coefficients, fs = fs, add = add)
    spectralslice (sound, fs = fs, color = 4, add = TRUE)
  }  
  coefficients
}




tmp = tmp[4000:6000]
tmp = resample (tmp, 10000, 22050)
fs = 10000

plot(tmp)

n=length(tmp)
wn = 8/(1000/fs)
step = 2/(1000/fs)
phasestep = (2*pi)*(2/1000)

spots = seq (1+step,n-wn,step)
i = spots[1]
sp = fft (tmp[span])
startangs = atan2 (Im(sp),Re(sp))
ft = sp
count = 1
for (i in spots){
  span = i:(i+wn-1) 
  sp = fft (tmp[span])
  angs = atan2 (Im(sp),Re(sp))
  
  sp2 = rotate(sp, +(phasestep*count))
  ft = ft * sp2
  count = count + 1
}

par (mfrow = c(2,1),mar=c(4,4,1,1))
spectralslice(tmp[span])
tmpp = log(abs(ft[1:(wn/2)]))
plot (tmpp - max(tmpp), type = 'l',xaxs='i')


sp2 = rotate(sp, angs)

sp2[1]




