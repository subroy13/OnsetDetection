library(tuneR)
library(seewave)
library(fftw)
#library(rpanel)

# Reading the Data
song.stereo = readMP3('Arpan o je mane na mana_reduced.mp3')
song.mono = mono(song.stereo, which = "both")  #converts to mono by averaging out the channels
length(song.mono@left)/song.mono@samp.rate   # number of seconds the song remains
rm(song.stereo)

# Visualizing the data
oscillo(song.mono)
oscillo(song.mono, from = 1.25, to = 5)
oscillo(song.mono, from = 1.25, to = 2)
oscillo(song.mono, from = 1.25, to = 1.35)


# For simplicity, we work with 0.25 to 3.25 seconds area
song.ext = extractWave(song.mono, from = 1.25*song.mono@samp.rate, to = 5*song.mono@samp.rate)
play(song.ext)


###########################################
song.ext = readWave('Subhrajyoty_scotland_theme.wav')
song.ext = mono(song.ext)
str(song.ext)
play(song.ext)


# Obtaining Absolute Amplitude Envelope 
# Note that, for a mono file, it is same as energy profile,
# where energy profile = sqrt(song@left^2 + song@right^2) = abs(song@left)
env(song.ext, envt = "abs", msmooth = c(4096, 75))
abline(v = c(1.092, 1.982, 2.196, 2.735, 3.237, 4.303, 4.843, 5.34, 6.499,
             7.126,7.653), col = "red")


# Obtaining Hillbert transform as energy profiler
env(song.ext)

# A more practical / statistical way of visualizing the energy profile
hist(abs(song.ext@left))
# This shows, there are lots of silent sections, 
# i.e. where the energy profile is very low


####################################################
# Intro to STFT
# Note that, the shortest detectable tone perceived by humans stays for 100ms on average

window.len <- 2^floor(log2(song.ext@samp.rate/10))
#dynspec(song.ext, wl = window.len, ovlp = 50, osc = TRUE, fftw = TRUE)
#dynspectro(song.ext, wl = window.len, ovlp = 50, osc = TRUE, fftw = TRUE)


# Let's track the dominant frequency using STFT
df <- dfreq(song.ext, wl = window.len, ovlp = 50, plot = FALSE) # tracks the dominant frequency in kilohertz
head(df)  # x-column is the time and y-col is the dominant frequency in KHz
df[,2] <- 1000*df[,2]   #convert it to Hz
df <- as.data.frame(df)
colnames(df) <- c("Time", "Freq(Hz)")
df$Spec.tuneR <- NA
df$Energy <- NA


head(df)


specs <- periodogram(song.ext, width = window.len, overlap = 0.75*window.len)
# we need to remove the last one as the last window is padded with zeroes

for (i in 1:nrow(df)) {
    if (!is.na(df[i,2])) {
        index <- which(specs@freq==df[i,2])   # check with frequency matches
        df$Spec.tuneR[i] <- specs@spec[[i]][index]
        df$Energy[i] <- specs@energy[i]
    }
}

head(df)

plot(df$Time, df$`Freq(Hz)`, type = "l")
plot(df$Time, df$Energy, type = "l")
plot(df$Time, df$Spec.tuneR, type = "l")

abline(v = c(1.092, 1.982, 2.196, 2.735, 3.237, 4.303, 4.843, 5.34, 6.499,
             7.126,7.653) - 0.05 , col = "red", lwd = 1)
plot(df$Time, specs@variance, type = "l")










