#loading data
library("zoo")
##loading hs300 index data 20100416~20151127
hs300 <- read.csv('E:\\Rtrial\\winddata\\hs300.csv',header = FALSE)
zz500 <- read.csv('E:\\Rtrial\\winddata\\zz500.csv',header = FALSE)
sz50 <- read.csv('E:\\Rtrial\\winddata\\sz50.csv',header = FALSE)
hs300t <- read.csv('E:\\Rtrial\\winddata\\hs300t.csv',header = FALSE)
ict <- read.csv('E:\\Rtrial\\winddata\\ict.csv',header = FALSE)

##loading index futures data
if00 <- read.csv('E:\\Rtrial\\winddata\\if00.csv',header = FALSE)
ic00 <- read.csv('E:\\Rtrial\\winddata\\ic00.csv',header = FALSE)
ih00 <- read.csv('E:\\Rtrial\\winddata\\ih00.csv',header = FALSE)

if01 <- read.csv('E:\\Rtrial\\winddata\\if01.csv',header = FALSE)
ic01 <- read.csv('E:\\Rtrial\\winddata\\ic01.csv',header = FALSE)
ih01 <- read.csv('E:\\Rtrial\\winddata\\ih01.csv',header = FALSE)

if02 <- read.csv('E:\\Rtrial\\winddata\\if02.csv',header = FALSE)
ic02 <- read.csv('E:\\Rtrial\\winddata\\ic02.csv',header = FALSE)
ih02 <- read.csv('E:\\Rtrial\\winddata\\ih02.csv',header = FALSE)

if03 <- read.csv('E:\\Rtrial\\winddata\\if03.csv',header = FALSE)
ic03 <- read.csv('E:\\Rtrial\\winddata\\ic03.csv',header = FALSE)
ih03 <- read.csv('E:\\Rtrial\\winddata\\ih03.csv',header = FALSE)

load(file = "Ndata1231.RData")
load(file = "Nindex1231.RData")


#winsorizing
#---------------------------------
winsor1 <- function (x, fraction=.01)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}

if00c<- na.approx(if00$V1)
ic00c<- na.approx(ic00$V1)
ih00c<- na.approx(ih00$V1)

if01c<- na.approx(if01$V1)
ic01c<- na.approx(ic01$V1)
ih01c<- na.approx(ih01$V1)

if02c<- na.approx(if02$V1)
ic02c<- na.approx(ic02$V1)
ih02c<- na.approx(ih02$V1)

if03c<- na.approx(if03$V1)
ic03c<- na.approx(ic03$V1)
ih03c<- na.approx(ih03$V1)
ih03c <- c(ih03c, ih03c[length(ih03c)-2])


#add new period
if00c<- c(if00c, na.approx(nif00$Data$close))
ic00c<- c(ic00c, na.approx(nic00$Data$close))
ih00c<- c(ih00c, na.approx(nih00$Data$close))

if01c<- c(if01c, na.approx(nif01$Data$close))
ic01c<- c(ic01c, na.approx(nic01$Data$close))
ih01c<- c(ih01c, na.approx(nih01$Data$close))

if02c<- c(if02c, na.approx(nif02$Data$close))
ic02c<- c(ic02c, na.approx(nic02$Data$close))
ih02c<- c(ih02c, na.approx(nih02$Data$close))

if03c<- c(if03c, na.approx(nif03$Data$close))
ic03c<- c(ic03c, na.approx(nic03$Data$close))
ih03c<- c(ih03c, na.approx(nih03$Data$close))



names(if00) <- c("close", "volume", "change", "position")
names(if01) <- c("close", "volume", "change", "position")
names(if02) <- c("close", "volume", "change", "position")
names(if03) <- c("close", "volume", "change", "position")

names(ic00) <- c("close", "volume", "change", "position")
names(ic01) <- c("close", "volume", "change", "position")
names(ic02) <- c("close", "volume", "change", "position")
names(ic03) <- c("close", "volume", "change", "position")

names(ih00) <- c("close", "volume", "change", "position")
names(ih01) <- c("close", "volume", "change", "position")
names(ih02) <- c("close", "volume", "change", "position")
names(ih03) <- c("close", "volume", "change", "position")

#volume
#--------------------------------
hs300vol <- na.approx(c(hs300$V2, as.double(nhs300$Data$volume)))
zz500vol <- na.approx(c(zz500$V2, as.double(nzz500$Data$volume)))
sz50vol <- na.approx(c(sz50$V2, as.double(nsz50$Data$volume)))

if00vol <- na.approx(c(if00$volume, as.double(nif00$Data$volume)))
ic00vol <- na.approx(c(ic00$volume, as.double(nic00$Data$volume)))
ih00vol <- na.approx(c(ih00$volume, as.double(nih00$Data$volume)))

if01vol <- na.approx(c(if01$volume, as.double(nif01$Data$volume)))
ic01vol <- na.approx(c(ic01$volume, as.double(nic01$Data$volume)))
ih01vol <- na.approx(c(ih01$volume, as.double(nih01$Data$volume)))

if02vol <- na.approx(c(if02$volume, as.double(nif02$Data$volume)))
ic02vol <- na.approx(c(ic02$volume, as.double(nic02$Data$volume)))
ih02vol <- na.approx(c(ih02$volume, as.double(nih02$Data$volume)))

if03vol <- na.approx(c(if03$volume, as.double(nif03$Data$volume)))
ic03vol <- na.approx(c(ic03$volume, as.double(nic03$Data$volume)))
ih03vol <- na.approx(c(ih03$volume, as.double(nih03$Data$volume)))

#position
#------------------------------------------------------------

if00pos <- na.approx(c(if00$position, as.double(nif00$Data$position)))
ic00pos <- na.approx(c(ic00$position, as.double(nic00$Data$position)))
ih00pos <- na.approx(c(ih00$position, as.double(nih00$Data$position)))

if01pos <- na.approx(c(if01$position, as.double(nif01$Data$position)))
ic01pos <- na.approx(c(ic01$position, as.double(nic01$Data$position)))
ih01pos <- na.approx(c(ih01$position, as.double(nih01$Data$position)))

if02pos <- na.approx(c(if02$position, as.double(nif02$Data$position)))
ic02pos <- na.approx(c(ic02$position, as.double(nic02$Data$position)))
ih02pos <- na.approx(c(ih02$position, as.double(nih02$Data$position)))

if03pos <- na.approx(c(if03$position, as.double(nif03$Data$position)))
ic03pos <- na.approx(c(ic03$position, as.double(nic03$Data$position)))
ih03pos <- na.approx(c(ih03$position, as.double(nih03$Data$position)))



#use zoo package na.approx() function to generate time series without NA
#close price of Index Futures



#winsorizing
#if00c <- winsor1(if00c)
#ic00c <- winsor1(ic00c)
#ih00c <- winsor1(ih00c)

#if01c <- winsor1(if01c)
#ic01c <- winsor1(ic01c)
#ih01c <- winsor1(ih01c)

#if02c <- winsor1(if02c)
#ic02c <- winsor1(ic02c)
#ih02c <- winsor1(ih02c)

#if03c <- winsor1(if03c)
#ic03c <- winsor1(ic03c)
#ih03c <- winsor1(ih03c)


#log of close
if00cl<- log(if00c)
ic00cl<- log(ic00c)
ih00cl<- log(ih00c)

if01cl<- log(if01c)
ic01cl<- log(ic01c)
ih01cl<- log(ih01c)

if02cl<- log(if02c)
ic02cl<- log(ic02c)
ih02cl<- log(ih02c)

if03cl<- log(if03c)
ic03cl<- log(ic03c)
ih03cl<- log(ih03c)

#log return of IF/IC/IH
if00r <- diff(if00cl)
ic00r <- diff(ic00cl)
ih00r <- diff(ih00cl)

if01r <- diff(if01cl)
ic01r <- diff(ic01cl)
ih01r <- diff(ih01cl)

if02r <- diff(if02cl)
ic02r <- diff(ic02cl)
ih02r <- diff(ih02cl)

if03r <- diff(if03cl)
ic03r <- diff(ic03cl)
ih03r <- diff(ih03cl)


#take logritham of index
#hs300c <- c(na.approx(hs300$V1))
hs300c <- c(na.approx(hs300$V1),na.approx(nhs300$Data$close))
#zz500c <- c(na.approx(zz500$V1))
zz500c <- c(na.approx(zz500$V1),na.approx(nzz500$Data$close))
sz50c <- c(na.approx(sz50$V1),na.approx(nsz50$Data$close))

hs300cl <- log(hs300c)
zz500cl <- log(zz500c)
sz50cl <- log(sz50c)

hs300r <- diff(hs300cl)
zz500r <- diff(zz500cl)
sz50r　<- diff(sz50cl)

#get the basis 
basif <- if00c - hs300c
basic <- ic00c - zz500c
basih <- ih00c - sz50c

#save.image("E:/Rtrial/MyFirstPaper/Alldata.RData")

#temp <- data.frame(if00, if01, if02, if03, hs300)
#write.csv(temp, file = "IF.csv")

#tempc <- data.frame(ic00, ic01, ic02, ic03, zz500)
#write.csv(tempc, file = "IC.csv")

#temph <- data.frame(ih00, ih01, ih02, ih03, sz50)
#write.csv(temph, file = "IH.csv")





