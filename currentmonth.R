#new dataset with consideration of maturity effect
#source("LoadingDataorigin.R")
library(xts)
options(warn = -1)
#for different period 
#20150416 to 20150709
#time period
# 20121128~20150416
yf1 <- 137761
# 20121128~20150709
yf2 <- 151681
#length of IF, hs
yf3 <- length(if00c)
#20121128~20150902
yf4 <- 161281


#20150416~20150709
yc2 <- 13920
#length
yc3 <- length(ic00c)
#20150416~20150902
yc4 <- 23521


#construct date 
iftm <- paste(hs300t$V4, hs300t$V5, sep = ":")
ifymd <- paste(hs300t$V1, hs300t$V2, hs300t$V3, sep = "-")
ifymdhm <- paste(ifymd, iftm)

#for IC,IH
ictm <- paste(ict$V4, ict$V5, sep = ":")
icymd <- paste(ict$V1, ict$V2, ict$V3, sep = "-")
icymdhm <- paste(icymd , ictm)




#use strptime() to transform the character
ifdate <- strptime(ifymdhm, format = "%Y-%m-%d")
icdate <- strptime(icymdhm, format = "%Y-%m-%d")
ihdate <- icdate

#change the dataframe into xts object
if0ts <- as.xts(if00cl, ifdate)
if1ts <- as.xts(if01cl, ifdate)

ic0ts <- as.xts(ic00cl, icdate)
ic1ts <- as.xts(ic01cl, icdate)

ih0ts <- as.xts(ih00cl, ihdate)
ih1ts <- as.xts(ih01cl, ihdate)

Aif0ts <- if0ts[yf1:length(if00cl)]
Aif1ts <- if1ts[yf1:length(if00cl)]

hsts <- as.xts(hs300c, ifdate)
zzts <- as.xts(zz500c, icdate)
szts <- as.xts(sz50c,ihdate)




#construct new series with delivery date
aD <- c("2015-04-16","2015-05-15", "2015-06-19", "2015-07-17","2015-08-21", "2015-09-18","2015-10-16","2015-11-20", "2015-12-18")
ad <- as.Date(aD)
nad <- ad - 4
nad <- as.character(nad[2:length(nad)])

#compute the days between two delivery date
if0plugin <- sapply(1:(length(aD)-1), function(i) if0ts[ifdate >= aD[i] & ifdate < aD[i+1]] ) 
#convert min data into daily
ifMDn <- sapply(1:length(if0plugin), function(i) length(if0plugin[[i]])/240)
ifMDN <- ifMDn -1

#obtain the time stamp
Aftid <- sapply(1:length(ifMDN), function(i) c(rep(ifMDN[i]:1, rep(240,ifMDN[i])), rep(0,240) ))
Maftid <- unlist(Aftid)
Taftid <- rep(20:11, rep(240, 10))
Aft <- c(Maftid,Taftid)
length(Aft)

Aggdata1 <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                   Aic00, Aic01, Aic02, Aic03, 
                   Aih00, Aih01, Aih02, Aih03, tid = Aft
)

# only slightly different result



for(i in 1:8){
  
  if0ts[ifdate >= nad[i] & ifdate <= aD[(i+1)]] <- if1ts[ifdate >= nad[i] & ifdate <= aD[(i+1)]]
  
  #print("a")
}
for(i in 1:8){
  
  ic0ts[icdate >= nad[i] & icdate <= aD[(i+1)]] <- ic1ts[icdate >= nad[i] & icdate <= aD[(i+1)]]
  
  #print("a")
}
for(i in 1:8){
  
  ih0ts[ihdate >= nad[i] & ihdate <= aD[(i+1)]] <- ih1ts[ihdate >= nad[i] & ihdate <= aD[(i+1)]]
  
  #print("a")
}

Aif0ts <- if0ts[yf1:length(if00cl)]
Aif00 <- as.vector(Aif0ts[,1])
Bif00 <- Aif00[1:yc2]
Bft <- Aft[1:yc2]
Cif00 <- Aif00[yc4:length(Aif00)]
Cft <- Aft[yc4:length(Aif00)]


Aic0ts <- ic0ts[1:length(ic00cl)]
Aic00 <- as.vector(Aic0ts[,1])
Bic00 <- Aic00[1:yc2]
Cic00 <- Aic00[yc4:length(Aif00)]


Aih0ts <- ih0ts[1:length(ih00cl)]
Aih00 <- as.vector(Aih0ts[,1])
Bih00 <- Aih00[1:yc2]
Cih00 <- Aih00[yc4:length(Aif00)]

#GG
Aggdata <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                  Aic00, Aic01, Aic02, Aic03, 
                  Aih00, Aih01, Aih02, Aih03, tid = Aft
)

Ahadata <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                  Aic00, Aic01, Aic02, Aic03, 
                  Aih00, Aih01, Aih02, Aih03, tid = Aft, K="H"
)

