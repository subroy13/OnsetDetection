# Load packages
library(tuneR)
library(seewave)
library(zoo)

###################################################################
# Main function to Compare the detected results
generate_song <- function(notes, duration = NULL, onsets = NULL, base = 440, samp.rate = 44100, shape = "sine") {
    if (is.null(duration)) {
        # then compute the durations
        duration <- numeric(length(onsets))
        temp <- c(diff(onsets), 1)
        duration[1:length(onsets)] <- ifelse(temp > 1, 1, temp) 
    }
    if (length(duration) < length(notes)) {
        notes <- notes[1:length(duration)]
    }
    else {
        duration <- duration[1:length(notes)]
    }
    
    song <- data.frame(pitch = notes, duration = duration)
    notes <- c(A = 0, B = 2, C = -10, D = -7, E = -5, F = -4, G = -2)
    song$note = notes[substr(song$pitch, 1, 1)]
    song$note = song$note + ifelse(substr(song$pitch, 2,2) == "#", 1, 0)
    song$note = song$note + ifelse(substr(song$pitch, 2,2) == "b", -1, 0)
    song$note = song$note + ifelse(substr(song$pitch, 2,2) == "5", 13, 0)
    song$note = song$note + ifelse(substr(song$pitch, 2,2) == "3", -13, 0)
    song$freq = base * 2^(song$note/12)
    
    
    x = as.list(numeric(nrow(song)))
    for (i in 1:nrow(song)) {
        x[[i]] = synth(samp.rate, d = song$duration[i], cf = song$freq[i], 
                       output = "Wave", a = 2^15-1, shape = shape, harmonics = c(1,0,0.5))
    }    
    
    mysong = do.call(bind, x)
    mysong@left <- mysong@left/max(max(mysong@left), -min(mysong@left))*(2^15 - 1)
    return(mysong)
}


####################################################################
# Song 1 (Sa Re Jahan Se Accha)

notes = "D# D C D B3 C C G3 A3 C D E F E D F E D C D# D C D B3 C C"
notes = strsplit(notes, " ")[[1]]
# the actual onset of the song
song_onsets <- c(1, 1.5, 2, 2.25, 2.75, 3, 3.5, 5, 5.25, 5.5, 6, 6.25, 6.75, 7, 7.75, 8, 8.25, 8.5, 8.75, 9, 9.5, 10, 10.25, 10.75, 11, 11.5) 
# the onset I hummed
true_onsets <- c(0.809, 1.417, 2.099, 2.344, 2.944, 3.271, 3.843, 5.792, 6.215, 6.528, 7.332, 7.523, 8.123, 8.477, 9.157, 9.513, 9.704, 9.935, 10.317, 11.026, 11.693, 12.334, 12.702, 13.343, 13.779, 14.297)

cor(song_onsets, true_onsets)   # pretty high 0.999


song <- readWave('./sa re jahan se accha.wav')
song <- mono(song)

####################################################################
# Song 2 (Ekla Cholo Re)

notes = "B3 B3 B3 C C D D G F E D C D E E D C"
notes = strsplit(notes, " ")[[1]]
song_onsets <- c(0.67, 1, 1.33, 2, 2.67, 3, 3.33, 4, 5, 6, 7, 7.33, 8, 8.67, 9, 9.33, 10)   # the actual onset of the song
true_onsets <- c(0.496, 0.589, 0.856, 1.315, 1.719, 1.967, 2.196, 2.643, 3.319, 4.014, 4.647, 4.938, 5.317, 5.726, 6.012, 6.241, 6.663)   # the onset of the hummed song
cor(true_onsets, song_onsets)   # the correlation is 0.999

song <- readWave('./ekla cholo re.wav')
song <- mono(song)

##########################################################
# Song 3 (Jingle Bells)

notes = ""
notes = strsplit(notes, " ")[[1]]
song_onsets <- c(0.25, 0.5, 0.75, 1.25, 1.5, 1.75, 
                 2.25, 2.5, 2.75, 3, 3.25, 
                 4.25, 4.5, 4.75, 
                 5.125, 5.25, 5.5, 5.75, 
                 6.25, 6.5, 6.75, 7, 7.5, 8)   

true_onsets <- c(0.495, 0.718, 1.011, 1.448, 1.611, 1.872, 2.627, 2.879, 3.198, 3.391, 3.676, 4.725, 5.011, 5.262, 5.489, 5.741, 6.085, 6.37, 6.681, 7.142, 7.394, 7.663, 7.948, 8.485)  

cor(true_onsets, song_onsets)   # the correlation is 0.999

song <- readWave('./jingle bells.wav')
song <- mono(song)


####################################################################
# Local Energy Detector
# Resolution , d for which T(x), T(x+d), T(x+2d), these are calculated

EnergyDetector <- function(song, window.len, resolution) {
    temp <- 1:length(song@left)
    Energymat <- rollapply(temp, width = window.len, by = resolution, partial = T, align = 'left', FUN = function(x){ c(mean(x)/song@samp.rate, sum(song@left[x]^2)/window.len) })
    colnames(Energymat) <- c("Time(s)","Energy")
    return(Energymat)
}


#####################################################################
# Frequency Based Method (Spectral Dissimilarity)

Spectral_Flux <- function(song, window.len, resolution) {
    ovlp <- (1-resolution/window.len)*100
    spec <- spectro(song, wl = window.len, ovlp = ovlp, complex = T, fftw = T, flim = c(0,3))
    spec_mags <- abs(spec$amp)
    Spectral_Flux <- numeric(ncol(spec$amp))
    for (i in 2:ncol(spec$amp)) {
        temp <- spec_mags[, i] - spec_mags[, (i-1)]
        temp <- ifelse(temp > 0, temp, 0)
        Spectral_Flux[i] <- sum(temp)
    }
    Spectral_mat <- cbind(spec$time, Spectral_Flux)
    colnames(Spectral_mat) <- c("Time(s)","Flux")
    
    return(Spectral_mat)
}

###############################################################

Dominant_SD <- function(song, window.len, resolution) {
    ovlp <- (1-resolution/window.len)*100
    spec <- spectro(song, wl = window.len, ovlp = ovlp, complex = T, fftw = T, flim = c(0,3))
    #spec_mags <- 1 - exp(-(abs(spec$amp)^2)*2)
    spec_mags <- abs(spec$amp)
    KStat <- numeric(ncol(spec$amp))
    #temp <- seq(0, 1, length.out = length(spec$freq))
    for (i in 2:ncol(spec$amp)) {
        dom1 <- which.max(spec_mags[, i])
        dom2 <- which.max(spec_mags[, (i-1)])
        temp <- max(spec_mags[, (i-1)])
        KStat[i] <- max(spec_mags[, i]) - temp #+ log(abs(dom1- dom2))
        #temp <- spec_mags[,i]/spec_mags[,(i-1)]
        
        #KStat[i] <- log(max())
        #KStat[i] <- mean((sort(spec_mags[, i]) - temp)^2)
    }
    KStat <- ifelse(KStat < 0, 0, KStat)
    #KStat <- ifelse(is.nan(KStat), 1, KStat)
    #KStat <- ifelse(KStat == Inf, 10, KStat)
    KMat <- cbind(spec$time, KStat)
    colnames(KMat) <- c("Time(s)","KStat")
    
    return(KMat)
}



###################################################################
# Peak Picking Algorithm
# hopsize, the hops on which the algorithm tries to predict whether it has an onset or not

# Relation with Resolution
# resolution = gcd(hopsize, checkpoints[2] - checkpoints[1])

# here, the argument checkpoints and hopsize is in resolution units

PickPeaks <- function(detection, times, checkpoints, hopsize, threshold = 1, thres = NULL) {
    onset <- c()
    if (is.null(thres)) {
        thres <- threshold * mean(detection)
    }
    #thres <- pchisq(q = 0.99, df = 4096)
    index <- seq(1, length(times), by = hopsize)
    for (i in index) {
        if (detection[i] > thres) {
            id <- (checkpoints + i)
            maxindex <- which.max(detection[id[id > 0]])
            if (checkpoints[maxindex] == 0) {
                l <- length(onset)
                if (l == 0) {
                    onset <- c(onset, times[i])
                }
                else {
                    if (times[i] > onset[l] + 0.1) {
                        # the next peak is after sufficient time 
                        onset <- c(onset, times[i])
                    }
                }
            }
        } 
    }
    return(onset)
}



#######################################################################
# Subset Matching Algorithm

# find nearest target in same scale
Find_Nearest_Target <- function(df, i) {
    if (i==1) {
        # the first one
        if (df$isTarget[i] != df$isTarget[(i+1)]) {
            return(i+1)
        }
        else {
            a = ifelse(df$isTarget[i], 'FALSE_NEGATIVE', 'FALSE_POSITIVE')
            return(a)
        }
    }
    else if (i==nrow(df)) {
        # the last one
        if (df$isTarget[i] != df$isTarget[(i-1)]) {
            return(i-1)
        }
        else {
            a = ifelse(df$isTarget[i], 'FALSE_NEGATIVE', 'FALSE_POSITIVE')
            return(a)        
        }
    }
    else {
        # now consider a middle value
        if ((df$isTarget[i] != df$isTarget[(i+1)]) | (df$isTarget[i] != df$isTarget[(i-1)])) {
            # we now have atleast one side being different colour
            if (df$isTarget[i] == df$isTarget[(i+1)]) {
                # so the prev one is of different color
                return(i-1)
            }
            else if (df$isTarget[i] == df$isTarget[(i-1)]) {
                # so the next one is of different color
                return(i+1)
            }
            else {
                # both side are of different colors
                a = df$Time[(i+1)] - df$Time[i]
                b = df$Time[i] - df$Time[(i-1)]
                return(ifelse(a < b, (i+1), (i-1)))
            }
        }
        else {
            a = ifelse(df$isTarget[i], 'FALSE_NEGATIVE', 'FALSE_POSITIVE')
            return(a)
        }
    }
}

# find nearest matching in same scale
Subset_Matching <- function(output, target) {
    
    time <- c(output, target)
    isTarget <- c(rep(FALSE, length(output)), rep(TRUE, length(target)))
    df <- data.frame(Time = time, isTarget = isTarget)
    df <- df[order(df$Time),]
    
    matched <- matrix(NA, nrow = min(length(output), length(target)), ncol = 2)
    false_positive <- 0
    false_negative <- 0
    
    i <- 1
    countMatch <- 0
    
    while (i < (1+nrow(df))) {
        j <- Find_Nearest_Target(df, i)
        if (j == "FALSE_POSITIVE") {
            false_positive <- false_positive + 1
            i <- (i+1)
        }
        else if (j == "FALSE_NEGATIVE") {
            false_negative <- false_negative + 1
            i <- (i+1)
        }
        else {
            k <- Find_Nearest_Target(df, j)  
            # clearly, since i's nearest neighbor is j, 
            # j cannot have both side as its same color
            if (i == k) {
                # that means it is a match
                countMatch <- countMatch + 1
                k <- ifelse(df$isTarget[i], 1, 2)  # the left column contains target values
                matched[countMatch,k] <- df$Time[i]
                matched[countMatch,(3-k)] <- df$Time[j]
                
                i <- (i+2)
            }
            else {
                # that means it is not a match
                j <- ifelse(df$isTarget[i], "FALSE_NEGATIVE", "FALSE_POSITIVE")
                if (j == "FALSE_POSITIVE") {
                    false_positive <- false_positive + 1
                    i <- (i+1)
                }
                else if (j == "FALSE_NEGATIVE") {
                    false_negative <- false_negative + 1
                    i <- (i+1)
                }
            }
        }
        
        
    }
    
    matched <- matched[1:countMatch,]
    
    return(list("False Pos" = false_positive, "False Neg" = false_negative, "Matched Entry" = matched))
}

# find nearest linear transformation
Correlative_Matching <- function(output, target, verbose = F) {
    n <- length(output)
    m <- length(target)
    if(n < m) {
        beta = (output[n]-output[1])/(target[m]-target[1])
        alpha = output[1]-beta*target[1]
        scaled_target = alpha + beta*target
        a <- Subset_Matching(output, scaled_target)
        
        correction <- (1 - a[[1]]/n) * (1 - a[[2]]/m)
        if (length(a$`Matched Entry`) > 4) {
            score <- cor(a$`Matched Entry`[,1], a$`Matched Entry`[,2])*correction
        }
        return(score)
    }
    
    else {
    cor_mat <- matrix(0, nrow=(n-m+1),ncol=(n-m+1))
    for(i in 1:(n-m+1)){
        for(j in m:n){
            #message(paste(i, '++', j))
            beta = (output[j]-output[i])/(target[m]-target[1])
            alpha = output[i]-beta*target[1]
            scaled_target = alpha + beta*target
            a <- Subset_Matching(output, scaled_target)
                
            correction <- (1 - a[[1]]/n) * (1 - a[[2]]/m)
                
            if (length(a$`Matched Entry`) > 4) {
                    cor_mat[i,j-m+1]<- cor(a$`Matched Entry`[,1], a$`Matched Entry`[,2])*correction 
                }
        }
    }
        if (verbose) {
            k = which.max(cor_mat)
            j = floor(k/(n-m))+1
            i = k-(j-1)*(n-m)
            beta = (output[m-1+j]-output[i])/(target[m]-target[1])
            alpha = output[i]-beta*target[1]
            scaled_target = alpha + beta*target
            a <- Subset_Matching(output, scaled_target)
            a$`Matched Entry`[,1] <- (a$`Matched Entry`[,1] - alpha)/beta
            return(a)
        }
        else {
            return(max(cor_mat))
        }
    }
}



##################################################################
# Example 


op <- par(mfrow = c(2, 1),     # 2x1 layout
          oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
          mar = c(2, 0, 2, 2), # space for one row of text at ticks and to separate plots
          mgp = c(2, 1, 0)   # axis label at 2 rows distance, tick labels at 1 row
)

a <- EnergyDetector(song, 4096, 512)

a <- Spectral_Flux(song, 4096, 2048)

a <- PowerProb(song, 4096, 2048)


plot(a[,1], a[,2], type = "l", xlab = "Time(s)", 
     ylab = "Values", 
     main = "Dominant Spectral Dissimilarity Detection Function for Song 1")
abline(v = true_onsets, col = "red")

plot(a[,1], a[,2], type = "l", ylim = c(0, 10))

onsets <- PickPeaks(a[,2], a[,1], checkpoints = seq(-2, 2, by = 1), 
                    hopsize = 1, threshold = 1)

quantile(a[,2], probs = seq(0.1, 0.9, by = 0.1))
onsets <- PickPeaks(a[,2], a[,1], checkpoints = seq(-4, 4, by = 1), hopsize = 1, thres = quantile(a[,2], probs = 0.5))


plot(onsets, rep(1, length(onsets)), type = "h",xlab = NA, ylab = NA, 
     main = "Detected Onsets", yaxt = "n", xlim = c(min(a[,1]), max(a[,1])))

points(true_onsets, rep(1, length(true_onsets)), col = "red")



#################################################
# Database reading

database = readLines(con = file('./database.txt'))
database = strsplit(database, ", ")
names(database) <- lapply(database, function(x){x[1]})
database <- lapply(database, function(x){ as.numeric(x[-1]) })


scores <- sapply(database, function(x){
    message('Song searching... Computing score...')
    return(Correlative_Matching(onsets, x)) 
    })

sort(scores, decreasing = T)







