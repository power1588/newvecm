
#=================================

#use paste to obtain the date characters
#for IF HS300
library(xts)

#library(ggplot2)
#library(xtsExtra)

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
ifts <- as.xts(if00c[yf1:yf3], ifdate[yf1:yf3])
icts <- as.xts(ic00c, icdate)
ihts <- as.xts(ih00c, ihdate)
hsts <- as.xts(hs300c[yf1:yf3], ifdate[yf1:yf3])
zzts <- as.xts(zz500c, icdate)
szts <- as.xts(sz50c,ihdate)

#change the frequency to days
infts <- endpoints(ifts, 'days')
incts <- endpoints(icts, 'days')

inftsm <- endpoints(ifts, on = 'mins', k=5)

#================================================
#plot

#region.interval <- c('2015-04-16', '2015-07-09')

par(xpd = T, mar = c(2,2,2,2), mfrow = c(2,1))

plot(ifts[infts], type = "l", ylab = "price", xaxt = "n", main = "The price history of IF00")
axis.POSIXct(1, at = seq(ifdate[1], max(ifdate), "months"),ifdate, format = "%Y-%m")
plot(basif[infts], xaxt = "n", ylab = "basis", type = "l",main = "The basis between IF00 and CSI300 spot ", lty = 1, pch = 3)
abline( h = 0, lty = 2)
#lines(c(600,600), c(0, 5000), lwd = 2, lty = 3)


plot(icts[incts], xaxt = "n", main="The Price history of IC00" , lty=1)
axis.POSIXct(1, icdate, format = "%Y-%m")
plot(basic[incts], xaxt = "n", type = "l",  main="The basis between IC00 and CSI500 spot " , lty=1)
abline( h = 0, lty = 2)


plot(ihts[incts], xaxt = "n", main="The price history of IH00" )
axis.POSIXct(1, ihdate, format = "%Y-%m")
plot(basih[incts], xaxt = "n", type = "l",  main="The basis between IH00 and SSE50 spot" , lty=1)
abline( h = 0, lty = 2)





