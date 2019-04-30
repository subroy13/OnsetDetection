library(tuneR)
library(seewave)
library(sadists)


noise <- readWave('./datasets/primary/Noise_2.wav')
noise <- mono(noise)

acf(noise@left, lag.max = 200)
pacf(noise@left)   # it seems the noise can be modeled as a AR(1) process

arima(noise@left, order = c(1,0,0))

# estimated variance of noise
mse <- 562.2/(1- 0.9719^2)


###############################################
# Power Calculation for Local Energy Detector

# function Gfunc(x - x*) = g(x)
Gfunc <- function(x, Amp, samp.rate = 48000, freq = 200) {
    # here, amp means SNR = Signal to Noise Ratio
    x <- x/samp.rate  # convert to seconds
    const <- samp.rate * Amp^2#/mse
    decay.rate <- 2
    temp1 <- ( exp(-2*decay.rate*x) * ( decay.rate^2*cos(4*pi*freq*x) - decay.rate^2 + 2*pi*decay.rate*freq*sin(4*pi*freq*x) - 4*pi^2*freq^2 ) )/(4*(decay.rate^3 + 4*pi^2*freq^2))
    temp2 <- (-4*pi^2*freq^2)/(4*(decay.rate^3 + 4*pi^2*freq^2))
    return(const*(temp1 - temp2))
}


# find non centrality of Noncentrality(x-x*) = T_w(x)
Noncentrality <- function(x, window.len, Amp, samp.rate = 48000, freq = 200) {
    if (x < (-window.len)) {
        return(0)
    }
    else if (x <= 0 & x >= (-window.len)) {
        return(Gfunc(x+window.len, Amp, samp.rate, freq))
    }
    else {
        # here, x > 0, i.e. true onset was before current
        return(Gfunc(x+window.len, Amp, samp.rate, freq) - Gfunc(x, Amp, samp.rate, freq))
    }
}



# Function for finding the probability that T_w(x-x*) < T_w(x-x*+k) 
# here, argument x = (x-x*)
NotPeakProb <- function(x, k, window.len, Amp, samp.rate = 48000, freq = 200) {
    if (abs(k) >= window.len) {
        ncp1 <- Noncentrality(x, window.len, Amp, samp.rate, freq)
        ncp2 <- Noncentrality(x+k, window.len, Amp, samp.rate, freq)
        
        temp <- max(c(ncp1, ncp2))
        ncp1 <- ncp1*1000/temp
        ncp2 <- ncp2*1000/temp
        
        prob <- psumchisqpow(0, wts = c(1, -1), df = c(window.len, window.len), ncp = c(ncp1, ncp2))
        
    }
    else if (k > 0) {
        ncp1 <- Noncentrality(x, k, Amp, samp.rate, freq)
        ncp2 <- Noncentrality(x+window.len+1, k, Amp, samp.rate, freq)
        
        temp <- max(c(ncp1, ncp2))
        ncp1 <- ncp1*1000/temp
        ncp2 <- ncp2*1000/temp
        
        prob <- psumchisqpow(0, wts = c(1, -1), df = c(k, k), ncp = c(ncp1, ncp2))
        
    }
    else {
        k <- abs(k)
        ncp1 <- Noncentrality(x-k+window.len+1, k, Amp, samp.rate, freq)
        ncp2 <- Noncentrality(x-k, k, Amp, samp.rate, freq)
        
        temp <- max(c(ncp1, ncp2))
        ncp1 <- ncp1*1000/temp
        ncp2 <- ncp2*1000/temp
        
        prob <- psumchisqpow(0, wts = c(1, -1), df = c(k,k), ncp = c(ncp1, ncp2))
    }
    return(prob)
}


x = c(-20:-1, 1:20)*128
y = sapply(x, FUN = function(i) { NotPeakProb(0, i, 4096, 9387) })
plot(x,y)



# Note that, Probability that T(x*) is a peak is same as;
# P(T(x*) > T(x* + k) for all k given by the algorithm)
# This prob is > sum(P(T(x*) > T(x* + k))) - (2r-1)   # by Booles inequality

LowerBoundPeakProb <- function(x, window.len, move.len, Amp = 1000, samp.rate = 48000, freq = 200, flag = 4, blag = 4) {
    # n <- floor(samp.rate/(10*move.len))
    temp <- c((-blag):(-1), 1:flag) * move.len
    y <- sapply(temp, FUN = function(i) { 1- NotPeakProb(x, i, window.len, Amp, samp.rate, freq) })
    return(max(0, sum(y) - (flag + blag - 1)))
}


# Note that, this lower bound does not change much for amplitude and frequency


x = (-1024):1024
y <- numeric(2049)

for (i in 1:length(x)) {
    y[i] <- LowerBoundPeakProb(x[i], window.len = 4096, move.len = 1024, Amp = 5000, flag = 8, blag = 8)
    if (i%%50 == 0){
        message(paste(i,'Done...'))
    }
}

plot(x, y, type = "l", xlab = "Shift from Actual Onset", ylab = "Lower Bound on the Peak Probability")
abline(h = 0.9, col = "blue")




###############################################
# Power Calculation for Spectral Dissimilarity and Dominant SD

FindProbSpectralFlux <- function(seed = 1234, Resample = 1000, noisev = 500, maxflat = 0.1) {
    probs <- numeric(48000*4)
    set.seed(seed)
    for (b in 1:Resample) {
        x <- rnorm((48000*4), mean = 0, sd = noisev)
        y <- seq(2, 4, length.out = (48000*2))
        x1 <- runif(maxflat*48000, min = 9000, max = 10000)
        x1 <- x1*sin(2*pi*150*(y[1:length(x1)] - 2))
        
        x2 <- exp(-5*(y[1:(length(y) - length(x1))]-2))*sin(2*pi*150*(y[1:(length(y) - length(x1))]-2))
        
        x3 <- x + c(rep(0, 48000*2), x1, x2)
        x <- Wave(x3, samp.rate = 48000, bit = 16)
        a <- Spectral_Flux(x, 4096, 2048)
        onsets <- PickPeaks(a[,2], a[,1], checkpoints = seq(-4, 4, by = 1), hopsize = 1)
        index <- round(onsets*48000)
        probs[index] <- probs[index] + rep(1, length(index))
        if (b%%100 == 0) {
            message(paste('Resample', b, 'completed...'))
        }
    }
    probs <- probs/Resample
    return(probs)
}


PowerProb <- function(seed = NA, Resample = 1000, noisev = 500, lag = 4, amp = 10000) {
    probs <- numeric(48000*2)
    if (!is.na(seed)) {
        set.seed(seed)
    }
    for (b in 1:Resample) {
        x <- rnorm((48000*2), mean = 0, sd = noisev)
        t_onset <- 48001
        y <- (t_onset:(2*48000))
        x2 <- amp*exp(-5*(y - t_onset)/48000)*cos(2*pi*150*(y-t_onset)/48000)
        x[y] <- x[y] + x2
        x <- Wave(x, samp.rate = 48000, bit = 16)
        #a <- Spectral_Flux(x, 4096, 2048)
        a <- Dominant_SD(x, 4096, 2048)
        onsets <- PickPeaks(a[,2], a[,1], 
                            checkpoints = seq(-lag, lag, by = 1), 
                            hopsize = 1)
        index <- round(onsets*48000)
        for (i in index){
            probs[(i-2048):(i+2048)] <- probs[(i-2048):(i+2048)] + rep(1, 4097)
        }
        if (b%%100 == 0) {
            message(paste('Resample', b, 'completed...'))
        }
    }
    probs <- probs/Resample
    return(probs)
}

x <- PowerProb(Resample = 2500, amp = 5000, lag = 2)
x <- x[1:96000]
plot((-47999:48000)/48000, x, type = "l", 
     ylab = "Peak Detection Probability", 
     xlab = "Deviance from True onset (s)")









