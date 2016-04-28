#Library pachage
library("vars")
library("fUnitRoots")
library("zoo")


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

#for VECM models among IF, IC and IH
#==================================================================
#perpare the data for log price with VCEM

#whole data with close volumn change (position:futures have spot don't)
#------------------------------------------------------------------
#INPUT

#hs300
#zz500
#sz50
#if00
#if01
#if02
#if03
#ic00
#ic01
#ic02
#ic03
#ih00
#ih01
#ih02
#ih03

#log price 
#hs300cl
#zz500cl
#sz50cl

#if00cl
#if01cl
#if02cl
#if03cl

#ic00cl
#ic01cl
#ic02cl
#ic03cl

#ih00cl
#ih01cl
#ih02cl
#ih03cl

#------------------------------------------------------------------
#OUTPUT
#ggmeasure for Period A  B  C
#Aggdata
#Bggdata
#Cggdata
#hasbrouck 
#Ahadata
#Bhadata
#Chadata



#for Period A Apr.16th 2015 to Dec. 31st 2015
#---------------------------------------------------------------------
Ahs300 <- hs300cl[yf1:length(hs300cl)]
Azz500 <- zz500cl[1:length(zz500cl)]
Asz50 <- sz50cl[1:length(sz50cl)]

Aif00 <- if00cl[yf1:length(if00cl)]
Aif01 <- if01cl[yf1:length(if00cl)]
Aif02 <- if02cl[yf1:length(if00cl)]
Aif03 <- if03cl[yf1:length(if00cl)]

Aic00 <- ic00cl[1:length(ic00cl)]
Aic01 <- ic01cl[1:length(ic00cl)]
Aic02 <- ic02cl[1:length(ic00cl)]
Aic03 <- ic03cl[1:length(ic00cl)]

Aih00 <- ih00cl[1:length(ih00cl)]
Aih01 <- ih01cl[1:length(ih00cl)]
Aih02 <- ih02cl[1:length(ih00cl)]
Aih03 <- ih03cl[1:length(ih00cl)]



#close price
Afdataclo <- data.frame(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, Aic00, Aic01, Aic02, Aic03,Aih00, Aih01, Aih02, Aih03 )
#return
AfdataDcl <- sapply(Afdataclo, diff)


#volume of spot and futures

Afdatavol <- data.frame(hs300vol[yf1:yf3],zz500vol[1:yc3],sz50vol[1:yc3],if00vol[yf1:yf3],if01vol[yf1:yf3],if02vol[yf1:yf3],if03vol[yf1:yf3],
                        ic00vol[1:yc3],ic01vol[1:yc3],ic02vol[1:yc3], ic03vol[1:yc3],
                        ih00vol[1:yc3],ih01vol[1:yc3], ih02vol[1:yc3], ih03vol[1:yc3])


names(Afdatavol) <- c("hs300_vol","zz500vol","sz50_vol","if00_vol", "if01_vol","if02_vol","if03_vol",
                      "ic00_vol","ic01_vol","ic02_vol","ic03_vol",
                      "ih00_vol","ih01_vol","ih02_vol","ih03_vol")

Afdatapos <- data.frame(if00pos[yf1:yf3],if01pos[yf1:yf3],if02pos[yf1:yf3],if03pos[yf1:yf3],
                        ic00pos[1:yc3],ic01pos[1:yc3],ic02pos[1:yc3], ic03pos[1:yc3],
                        ih00pos[1:yc3],ih01pos[1:yc3], ih02pos[1:yc3], ih03pos[1:yc3])

names(Afdatapos) <- c("if00_pos", "if01_pos","if02_pos","if03_pos",
                      "ic00_pos","ic01_pos","ic02_pos","ic03_pos",
                      "ih00_pos","ih01_pos","ih02_pos","ih03_pos")

#416 to 1231 mean of volume and position
mean_Aret <- sapply(1:15, function(i) mean(AfdataDcl[,i]))

#stand error of return
std_Aret <- sapply(1:15, function(i) sd(AfdataDcl[,i]))
#annulize
sd_Aret <- std_Aret*sqrt(240*252)

#acf
acf_Aret <- sapply(1:15, function(i) acf(AfdataDcl[,i], lag.max = 1)$acf)

#volume position
mean_Avol <- sapply(Afdatavol, mean)
mean_Apos <- sapply(Afdatapos, mean)


#416 to 709
BfdataDcl <- AfdataDcl[1:yc2,]
Bfdatavol <- Afdatavol[1:yc2,]
Bfdatapos <- Afdatapos[1:yc2,]

#mean of return
mean_Bret <- sapply(1:15, function(i) mean(BfdataDcl[,i]))
mean_Bret <- as.data.frame(mean_Bret)

#stand error of return
std_Bret <- sapply(1:15, function(i) sd(BfdataDcl[,i]))
#annulize
sd_Bret <- std_Bret*sqrt(240*252)

#acf
acf_Bret <- sapply(1:15, function(i) acf(BfdataDcl[,i], lag.max = 1)$acf)

#volume position 
mean_Bvol <- sapply(Bfdatavol, mean)
mean_Bpos <- sapply(Bfdatapos, mean)


#902 to 1231
CfdataDcl <- AfdataDcl[yc4:(yc3-1),]
Cfdatavol <- Afdatavol[yc4:yc3,]
Cfdatapos <- Afdatapos[yc4:yc3,]

#mean of return
mean_Cret <- sapply(1:15, function(i) mean(CfdataDcl[,i]))

#stand error of return
std_Cret <- sapply(1:15, function(i) sd(CfdataDcl[,i]))
#annulize
#some question
sd_Cret <- std_Cret*sqrt(240*252)

#acf
acf_Cret <- sapply(1:15, function(i) acf(CfdataDcl[,i], lag.max = 1)$acf)

#volume position
mean_Cvol <- sapply(Cfdatavol, mean)
mean_Cpos <- sapply(Cfdatapos, mean)

#time stamp for maturity
AtidIni <- rep(20:1, rep(240,20))
AN <- ((length(Aif00)-length(AtidIni))%/%240)
AM <- AN%/%20
AtidMod <- rep(20:1, rep(240,20))
AtidMid <- rep(AtidMod, AM)
AW <-  AN - AM*20
AtidTail <- rep(AW:1, rep(240, AW))
Atidtemp <- c(AtidIni, AtidMid)
Atid <- c(Atidtemp, AtidTail )

#Test 
#=============================================================
#ADF test for price and returns

#Price
Aadfcl <- sapply(1:dim(Afdataclo)[2], function(i) summary(ur.df(Afdataclo[,i], type = "none", lags = 2)) )

#return
AadfDc <- sapply(1:dim(AfdataDcl)[2], function(i) summary(ur.df(AfdataDcl[,i], type = "none", lags = 2)) )



#select the lag level of VECM
#--------------------------------------------------------------------
Varvecmf <- data.frame( diff(Ahs300), diff(Aif00))
infocritf <- VARselect(Varvecmf, lag.max = 40, type = "const")
infocritf$selection


#for Johansen test
result.jo <- ca.jo(x = Varvecmf, type = "eigen", ecdet ="const")
summary(result.jo)@teststat
summary(result.jo)@cval




Varvecmc <- data.frame(diff(Azz500), diff(Aic00 ))
infocritc <- VARselect(Varvecmc, lag.max = 40, type = "const")
infocritc$selection

Varvecmh <- data.frame(diff(Asz50), diff(Aih00))
infocrith <- VARselect(Varvecmh, lag.max = 40, type = "const")
infocrith$selection

#the selection is 6 for VAR, then for VECM the lag level we use 5.
lagv <- 5

#function for cotest

#use a function to initial the order of variables
#lenc function generate of variables

lenc <- function(r, lag = lagv){
  #generate the retrun of spot with lag
  r <- diff(r)
  b <- length(r)-1
  s <- sapply(1:(lag+1), function(i) r[i:(b - lag + i)] )
  return(s)
}

#cointegration test for spot-futures pair 
cotestA <- function(s, f, M = Atid, n=1, lag=lagv){
  #1.test cointegrate between spot and futures and get the residuals as
  #the error correction term
  # time stamp
  Atid <- M
  Atid1 <- Atid + 20
  Atid2 <- Atid + 40
  Atid3 <- Atid + 60
  
  if (f == Aif00| s == Aif00 | f == Aic00| s == Aic00 | f == Aih00| s == Aih00 | 
      f == Bif00| s == Bif00 | f == Bic00| s == Bic00 | f == Bih00| s == Bih00 |
      f == Cif00| s == Cif00 | f == Cic00| s == Cic00 | f == Cih00| s == Bih00 )
  {lmresult <- lm(f ~ s + Atid )}
  else if(f == Aif01| s == Aif01 | f == Aic01| s == Aic01 | f == Aih01| s == Aih01 | 
          f == Bif01| s == Bif01 | f == Bic01| s == Bic01 | f == Bih01| s == Bih01 |
          f == Cif01| s == Cif01 | f == Cic01| s == Cic01 | f == Cih01| s == Bih01 )
  {lmresult <- lm(f ~ s + Atid1 )}
  else if(f == Aif02| s == Aif02 | f == Aic02| s == Aic02 | f == Aih02| s == Aih02 | 
          f == Bif02| s == Bif02 | f == Bic02| s == Bic02 | f == Bih02| s == Bih02 |
          f == Cif02| s == Cif02 | f == Cic02| s == Cic02 | f == Cih02| s == Bih02  )
  {lmresult <- lm(f ~ s + Atid2)}
  else
  {lmresult <- lm(f ~ s + Atid3)}
  
  #lmresult <- lm(f ~ s)
  ect <- lmresult$residuals
  #2. use the ect(erro correction term) and lag term of spot and futures
  # to run regression about the diff(spot or futures)
  ##2.1 generate the dummy variable to indicator the launch of new futures
  ##and the shorting ban 
  # regresssion on diff(log(s)) ~ ect + diff(lag(s)) + diff(lag(f))
  s <- lenc(s, lag = lagv)
  f <- lenc(f, lag = lagv)
  
  
  cotest1 <- lm(s[,1] ~  ect[-c(1:(lag+1))]    
                +  s[,2] + s[,3] + s[,4]+ s[,5]   + f[,2] + f[,3] + f[,4] + f[,5]   )
  
  
  cotest2 <- lm(f[,1] ~  ect[-c(1:(lag+1))] 
                +  s[,2] + s[,3] + s[,4]+ s[,5]  + f[,2] + f[,3] + f[,4] + f[,5]  )
  
  
  e1 <- cotest1$residuals
  e2 <- cotest2$residuals
  v1 <- sd(e1)
  v2 <- sd(e2)
  rho <- cor(e1,e2)
  #for theta
  ##gamma1 for stock
  d11 <- cotest1$coefficients[2]
  names(d11) <- NULL
  
  ##gamma2 for futures
  d21 <- cotest2$coefficients[2]
  names(d21) <- NULL
  
  
  #GG measure
  
  g1 <- abs(d11)/(abs(d11)+abs(d21))
  
  
  #Hasbrouck measure
  H11 <- (-d21*v1 + d11*rho*v2)^2/(((-d21*v1 + d11*rho*v2)^2)+(d11*v2*sqrt(1-rho^2))^2)
  H12 <- (d11*v2*sqrt(1-rho^2))^2/(((-d21*v1 + d11*rho*v2)^2)+(d11*v2*sqrt(1-rho^2))^2)
  
  
  #test co <- cotest1
  if (n ==1){return((cotest1))}
  if (n ==2){return((cotest2))}
  if (n ==3){return(c(H11,H12,rho))}
  if (n ==4){return(c(g1))}
  if( n ==5 ){return(lmresult)}
  
}

#lmr <- cotestA(Ahs300,Aif00, n = 5)
#--------------------------------------------------------------------
#Af0co <- cotestA(Ahs300,Aif00, Atid,n=2, lag=5)
#for whole period Apr. 16th 2015 to Dec. 31st 2015
#----------------------------------------------------------------------
# function that calculate the GG and Hasboruck , N for time interval, K for GG or Hasbrouck
GHdata <- function(HS300, ZZ500, SZ50, IF00, IF01, IF02, IF03,
                   IC00, IC01, IC02, IC03, IH00, IH01, IH02, IH03, tid = Atid, N = 1, K="G"){
  inT <- seq(from = 0, to = length(HS300), by = N)
  HS300 <- HS300[inT]
  ZZ500 <- ZZ500[inT]
  SZ50 <- SZ50[inT]
  IF00 <- IF00[inT]
  IF01 <- IF01[inT]
  IF02 <- IF02[inT]
  IF03 <- IF03[inT]
  IC00 <- IC00[inT]
  IC01 <- IC01[inT]
  IC02 <- IC02[inT]
  IC03 <- IC03[inT]
  IH00 <- IH00[inT]
  IH01 <- IH01[inT]
  IH02 <- IH02[inT]
  IH03 <- IH03[inT]
  tid <- tid[inT]
  
  f0gg <- cotestA(HS300,IF00, tid,n=4, lag=5)
  f1gg <- cotestA(HS300,IF01, tid,n=4, lag=5)
  f2gg <- cotestA(HS300,IF02, tid,n=4, lag=5)
  f3gg <- cotestA(HS300,IF03, tid,n=4, lag=5)
  
  c0gg<- cotestA(ZZ500,IC00, tid, n=4, lag=5)
  c1gg<- cotestA(ZZ500,IC01, tid, n=4, lag=5)
  c2gg<- cotestA(ZZ500,IC02, tid, n=4, lag=5)
  c3gg<- cotestA(ZZ500,IC03, tid, n=4, lag=5)
  
  h0gg <- cotestA(SZ50,IH00,tid,n=4, lag=5)
  h1gg <- cotestA(SZ50,IH01,tid,n=4, lag=5)
  h2gg <- cotestA(SZ50,IH02,tid,n=4, lag=5)
  h3gg <- cotestA(SZ50,IH03,tid,n=4, lag=5)
  
  ggdata <- data.frame(f0gg, f1gg, f2gg, f3gg,
                       c0gg, c1gg, c2gg, c3gg,
                       h0gg, h1gg, h2gg, h3gg)
  #Hasbrouck
  Af0ha1 <- cotestA(HS300, IF00, tid,n=3, lag=lagv)
  Af0ha2 <- cotestA(IF00, HS300,tid,n=3, lag=lagv)
  Af0haM <- (Af0ha1[2] + Af0ha2[1])/2
  
  Af1ha1 <- cotestA(HS300, IF01,tid,n=3, lag=lagv)
  Af1ha2 <- cotestA(IF01, HS300,tid,n=3, lag=lagv)
  Af1haM <- (Af1ha1[2] + Af1ha2[1])/2
  
  Af2ha1 <- cotestA(HS300, IF02,tid,n=3, lag=lagv)
  Af2ha2 <- cotestA(IF02, HS300,tid,n=3, lag=lagv)
  Af2haM <- (Af2ha1[2] + Af2ha2[1])/2
  
  Af3ha1 <- cotestA(HS300, IF03,tid,n=3, lag=lagv)
  Af3ha2 <- cotestA(IF03, HS300,tid,n=3, lag=lagv)
  Af3haM <- (Af3ha1[2] + Af3ha2[1])/2
  
  #IC
  Ac0ha1 <- cotestA(ZZ500, IC00,tid, n=3, lag = lagv)
  Ac0ha2 <- cotestA(IC00, ZZ500,tid,  n=3, lag = lagv)
  Ac0haM <- (Ac0ha1[2] + Ac0ha2[1])/2
  
  Ac1ha1 <- cotestA(ZZ500, IC01,tid, n=3, lag = lagv)
  Ac1ha2 <- cotestA(IC01, ZZ500,tid,  n=3, lag = lagv)
  Ac1haM <- (Ac1ha1[2] + Ac1ha2[1])/2
  
  Ac2ha1 <- cotestA(ZZ500, IC02,tid, n=3, lag = lagv)
  Ac2ha2 <- cotestA(IC02, ZZ500,tid,  n=3, lag = lagv)
  Ac2haM <- (Ac2ha1[2] + Ac2ha2[1])/2
  
  Ac3ha1 <- cotestA(ZZ500, IC03,tid, n=3, lag = lagv)
  Ac3ha2 <- cotestA(IC03, ZZ500,tid,  n=3, lag = lagv)
  Ac3haM <- (Ac3ha1[2] + Ac3ha2[1])/2
  
  #IH
  Ah0ha1 <- cotestA(SZ50, IH00, tid, n = 3, lag = lagv)
  Ah0ha2 <- cotestA(IH00, SZ50, tid, n = 3, lag = lagv)
  Ah0haM <- (Ah0ha1[2] + Ah0ha2[1])/2
  
  Ah1ha1 <- cotestA(SZ50, IH01, tid, n = 3, lag = lagv)
  Ah1ha2 <- cotestA(IH01, SZ50, tid, n = 3, lag = lagv)
  Ah1haM <- (Ah1ha1[2] + Ah1ha2[1])/2
  
  Ah2ha1 <- cotestA(SZ50, IH02, tid, n = 3, lag = lagv)
  Ah2ha2 <- cotestA(IH02, SZ50, tid, n = 3, lag = lagv)
  Ah2haM <- (Ah2ha1[2] + Ah2ha2[1])/2
  
  Ah3ha1 <- cotestA(SZ50, IH03, tid, n = 3, lag = lagv)
  Ah3ha2 <- cotestA(IH03, SZ50, tid, n = 3, lag = lagv)
  Ah3haM <- (Ah3ha1[2] + Ah3ha2[1])/2
  
  hadata <- data.frame(Af0haM, Af1haM, Af2haM, Af3haM,
                       Ac0haM, Ac1haM, Ac2haM, Ac3haM,
                       Ah0haM, Ah1haM, Ah2haM, Ah3haM)
  
  
  if(K=="G"){ return(ggdata)}
  if(K == "H"){return(hadata)}
}


#GG measure for 12 spot-futures pairs in Period A
Aggdata <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                  Aic00, Aic01, Aic02, Aic03, 
                  Aih00, Aih01, Aih02, Aih03, tid = Atid
)

#Hasbrouck for 12 spot-futures pairs in Period A
Ahadata <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                  Aic00, Aic01, Aic02, Aic03, 
                  Aih00, Aih01, Aih02, Aih03, tid = Atid, K="H"
)

#FOR 5-MIN
#GG measure for 12 spot-futures pairs in Period A with 5-min interval
Agg5data <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                   Aic00, Aic01, Aic02, Aic03, 
                   Aih00, Aih01, Aih02, Aih03, tid = Atid, N = 5
)
#Hasbrouck for 12 spot-futures pairs in Period A with 5-min interval
Aha5data <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                   Aic00, Aic01, Aic02, Aic03, 
                   Aih00, Aih01, Aih02, Aih03, tid = Atid, K="H", N = 5
)

# 10 min
Agg10data <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                    Aic00, Aic01, Aic02, Aic03, 
                    Aih00, Aih01, Aih02, Aih03, tid = Atid, N = 10
)
#Hasbrouck for 12 spot-futures pairs in Period A with 5-min interval
Aha10data <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                    Aic00, Aic01, Aic02, Aic03, 
                    Aih00, Aih01, Aih02, Aih03, tid = Atid, K="H", N = 10
)

# 30 min
Agg30data <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                    Aic00, Aic01, Aic02, Aic03, 
                    Aih00, Aih01, Aih02, Aih03, tid = Atid, N = 30
)
#Hasbrouck for 12 spot-futures pairs in Period A with 5-min interval
Aha30data <- GHdata(Ahs300, Azz500, Asz50, Aif00, Aif01, Aif02, Aif03, 
                    Aic00, Aic01, Aic02, Aic03, 
                    Aih00, Aih01, Aih02, Aih03, tid = Atid, K="H", N = 30
)

#------------------------------------------------------------

# cross-futures/spot analysis IF00/IC00, IF00/IH00, IC00/IH00
#------------------------------------------------------
# n for choice of output 
cotestAf <- function(s, f, n=1, lag=lagv){
  
  lmresult <- lm(f ~ s)
  ect <- lmresult$residuals
  #2. use the ect(erro correction term) and lag term of spot and futures
  # to run regression about the diff(spot or futures)
  ##2.1 generate the dummy variable to indicator the launch of new futures
  ##and the shorting ban 
  # regresssion on log(spot) ~ ect + dum1*ect + dum2*ect +diff(lag(s)) + diff(lag(f))
  s <- lenc(s, lag = lagv)
  f <- lenc(f, lag = lagv)
  
  
  cotest1 <- lm(s[,1] ~  ect[-c(1:(lag+1))]    
                +  s[,2] + s[,3] + s[,4]+ s[,5]   + f[,2] + f[,3] + f[,4] + f[,5]   )
  
  
  cotest2 <- lm(f[,1] ~  ect[-c(1:(lag+1))] 
                +  s[,2] + s[,3] + s[,4]+ s[,5]  + f[,2] + f[,3] + f[,4] + f[,5]  )
  
  
  e1 <- cotest1$residuals
  e2 <- cotest2$residuals
  v1 <- sd(e1)
  v2 <- sd(e2)
  rho <- cor(e1,e2)
  #for theta
  ##gamma1 for stock
  d11 <- cotest1$coefficients[2]
  names(d11) <- NULL
  
  ##gamma2 for futures
  d21 <- cotest2$coefficients[2]
  names(d21) <- NULL
  
  
  #GG measure
  
  g1 <- abs(d11)/(abs(d11)+abs(d21))
  
  
  #Hasbrouck measure
  H11 <- (-d21*v1 + d11*rho*v2)^2/(((-d21*v1 + d11*rho*v2)^2)+(d11*v2*sqrt(1-rho^2))^2)
  H12 <- (d11*v2*sqrt(1-rho^2))^2/(((-d21*v1 + d11*rho*v2)^2)+(d11*v2*sqrt(1-rho^2))^2)
  
  
  #test co <- cotest1
  if (n ==1){return((cotest1))}
  if (n ==2){return((cotest2))}
  if (n ==3){return(c(H11,H12,rho))}
  if (n ==4){return(c(g1))}
  
}

# cross-futures Price discovery by GG and Hasbrouck Measures
# N for Time interval 
GGHadata <- function(IF,IC,IH,N = 1,K="G"){
  inT <- seq(from = 0, to = length(IF), by = N)
  TF <- IF[inT]
  TC <- IC[inT]
  TH <- IH[inT]
  #GG
  Tfcgg <- cotestAf(TF,TC, n=4, lag=5)
  Tcfgg <- 1- Tfcgg
  
  Tfhgg <- cotestAf(TF,TH,  n=4, lag=5)
  Thfgg <- 1- Tfhgg
  
  Tchgg <- cotestAf(TC,TH,  n=4, lag=5)
  Thcgg <- 1- Tchgg
  
  Tfchggdata <- data.frame( Tcfgg,Tfcgg,  Thfgg,Tfhgg,  Thcgg,Tchgg)
  
  Tfcha1 <- cotestAf(TF,TC, n=3, lag=5)
  Tfcha2 <- cotestAf(TC,TF, n=3, lag=5)
  TfchafM <- (Tfcha1[1]+Tfcha2[2])/2
  TfchacM <- (Tfcha1[2]+Tfcha2[1])/2
  
  Tfhha1 <- cotestAf(TF,TH, n=3, lag=5)
  Tfhha2 <- cotestAf(TH,TF, n=3, lag=5)
  TfhhafM <- (Tfhha1[1] + Tfhha2[2])/2
  TfhhahM <- (Tfhha1[2] + Tfhha2[1])/2
  
  Tchha1 <- cotestAf(TC,TH, n=3, lag=5)
  Tchha2 <- cotestAf(TH,TC, n=3, lag=5)
  TchhacM <- (Tchha1[1] + Tchha2[2])/2
  TchhahM <- (Tchha1[2] + Tchha2[1])/2
  
  Tfchhadata <- data.frame(TfchafM, TfchacM, TfhhafM, TfhhahM, TchhacM, TchhahM)
  
  if(K == "G") {return(Tfchggdata)}
  if(K == "H") {return(Tfchhadata)}
}

#------------------------------------------------------------
# cross futures
#for Johansen test
crossfu <- data.frame(Aif00,Aic00, Aih00)
result.jo <- ca.jo(x = crossfu, type = "trace", ecdet ="const")
summary(result.jo)@teststat
summary(result.jo)@cval


#for 1 minutes
#GG  
AGGfhcdata <- GGHadata(Aif00,Aic00, Aih00, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
AHafhcdata <- GGHadata(Aif00,Aic00, Aih00, K = "H")

#for 5 min
#GG
AGG5fhcdata <- GGHadata(Aif00,Aic00, Aih00,N = 5, K = "G")

#Hasbrouck measure H_1(u) H_2(l)
AHa5fhcdata <- GGHadata(Aif00,Aic00, Aih00,N = 5, K = "H")


#for 10 minutes
#GG
AGG10fhcdata <- GGHadata(Aif00,Aic00, Aih00,N = 10, K = "G")

#Hasbrouck measure H_1(u) H_2(l)
AHa10fhcdata <- GGHadata(Aif00,Aic00, Aih00,N = 10, K = "H")

# FOR 30 min
#GG
AGG30fhcdata <- GGHadata(Aif00,Aic00, Aih00,N = 30, K = "G")

#Hasbrouck measure H_1(u) H_2(l)
AHa30fhcdata <- GGHadata(Aif00,Aic00, Aih00,N = 30, K = "H")



#--------------------------------------------------------------------
#cross spot
#for Johansen test
crosssp <- data.frame(Ahs300,Azz500, Asz50)
result.jo <- ca.jo(x = crosssp, type = "eigen", ecdet ="const")
summary(result.jo)
summary(result.jo)@cval


#GG  
AGGsdata <- GGHadata(Ahs300,Azz500, Asz50, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
AHasdata <- GGHadata(Ahs300,Azz500, Asz50, K = "H")

#for 5 min
#GG 
AGG5sdata <- GGHadata(Ahs300,Azz500, Asz50,N = 5, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
AHa5sdata <- GGHadata(Ahs300,Azz500, Asz50, N= 5, K = "H")


#for 10 minutes
#GG 
AGG10sdata <- GGHadata(Ahs300,Azz500, Asz50,N = 10, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
AHa10sdata <- GGHadata(Ahs300,Azz500, Asz50, N= 10, K = "H")

#FOR 30 MIN

#GG 
AGG30sdata <- GGHadata(Ahs300,Azz500, Asz50,N = 30, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
AHa30sdata <- GGHadata(Ahs300,Azz500, Asz50, N= 30, K = "H")


#--------------------------------------------------------------------

#for period B Apr.16 2015 to Jul. 9th 2015
#--------------------------------------------------------------------
Bif00 <- if00cl[yf1:(yf2-1)]
Bif01 <- if01cl[yf1:(yf2-1)]
Bif02 <- if02cl[yf1:(yf2-1)]
Bif03 <- if03cl[yf1:(yf2-1)]

Bic00 <- ic00cl[1:yc2]
Bic01 <- ic01cl[1:yc2]
Bic02 <- ic02cl[1:yc2]
Bic03 <- ic03cl[1:yc2]

Bih00 <- ih00cl[1:yc2]
Bih01 <- ih01cl[1:yc2]
Bih02 <- ih02cl[1:yc2]
Bih03 <- ih03cl[1:yc2]

Bhs300 <- hs300cl[yf1:(yf2-1)]
Bzz500 <- zz500cl[1:yc2]
Bsz50 <- sz50cl[1:yc2]

#time stamp for maturity
BtidIni <- rep(20:1, rep(240,20))
BN <- ((length(Bif00)-length(BtidIni))%/%240)
BM <- BN%/%20
BtidMod <- rep(20:1, rep(240,20))
BtidMid <- rep(BtidMod, BM)
BW <-  BN - BM*20
BtidTail <- rep(BW:1, rep(240, BW))
Btidtemp <- c(BtidIni, BtidMid)
Btid <- c(Btidtemp, BtidTail )

#---------------------------------------------------------------------
#for spot-future pairs
# 1 min
#GG measure for 12 spot-futures pairs in Period A
Bggdata <- GHdata(Bhs300, Bzz500, Bsz50, Bif00, Bif01, Bif02, Bif03, 
                  Bic00, Bic01, Bic02, Bic03, 
                  Bih00, Bih01, Bih02, Bih03, tid = Btid)

#Hasbrouck measure
Bhadata <- GHdata(Bhs300, Bzz500, Bsz50, Bif00, Bif01, Bif02, Bif03, 
                  Bic00, Bic01, Bic02, Bic03, 
                  Bih00, Bih01, Bih02, Bih03, tid = Btid, K = "H")
# 5 min
#GG measure for 12 spot-futures pairs in Period A
Bgg5data <- GHdata(Bhs300, Bzz500, Bsz50, Bif00, Bif01, Bif02, Bif03, 
                   Bic00, Bic01, Bic02, Bic03, 
                   Bih00, Bih01, Bih02, Bih03, tid = Btid, N = 5)

#Hasbrouck measure
Bha5data <- GHdata(Bhs300, Bzz500, Bsz50, Bif00, Bif01, Bif02, Bif03, 
                   Bic00, Bic01, Bic02, Bic03, 
                   Bih00, Bih01, Bih02, Bih03, tid = Btid,N =5, K = "H")

# 10 min
#GG measure for 12 spot-futures pairs in Period A
Bgg10data <- GHdata(Bhs300, Bzz500, Bsz50, Bif00, Bif01, Bif02, Bif03, 
                    Bic00, Bic01, Bic02, Bic03, 
                    Bih00, Bih01, Bih02, Bih03, tid = Btid, N = 10)

#Hasbrouck measure
Bha10data <- GHdata(Bhs300, Bzz500, Bsz50, Bif00, Bif01, Bif02, Bif03, 
                    Bic00, Bic01, Bic02, Bic03, 
                    Bih00, Bih01, Bih02, Bih03, tid = Btid,N = 10, K = "H")

# 30 MIN
#GG measure for 12 spot-futures pairs in Period A
Bgg30data <- GHdata(Bhs300, Bzz500, Bsz50, Bif00, Bif01, Bif02, Bif03, 
                    Bic00, Bic01, Bic02, Bic03, 
                    Bih00, Bih01, Bih02, Bih03, tid = Btid, N = 30)

#Hasbrouck measure
Bha30data <- GHdata(Bhs300, Bzz500, Bsz50, Bif00, Bif01, Bif02, Bif03, 
                    Bic00, Bic01, Bic02, Bic03, 
                    Bih00, Bih01, Bih02, Bih03, tid = Btid,N = 30, K = "H")

#----------------------------------------------------------------------

#price discovery ability across futures
#--------------------------------------------------------------------
#GG
BGGfhcdata <- GGHadata(Bif00,Bic00, Bih00, K = "G")

#Hasbrouck measure H_1(u) H_2(l) for 1-min
BHafhcdata <- GGHadata(Bif00,Bic00, Bih00, K = "H")

#for 5 min
#GG
BGG5fhcdata <- GGHadata(Bif00,Bic00, Bih00,N = 5, K = "G")

#Hasbrouck measure H_1(u) H_2(l)
BHa5fhcdata <- GGHadata(Bif00,Bic00, Bih00,N = 5, K = "H")

#for 10 minutes
#GG
BGG10fhcdata <- GGHadata(Bif00,Bic00, Bih00,N = 10, K = "G")

#Hasbrouck measure H_1(u) H_2(l)
BHa10fhcdata <- GGHadata(Bif00,Bic00, Bih00,N = 10, K = "H")

#FOR 30-min
#GG
BGG30fhcdata <- GGHadata(Bif00,Bic00, Bih00,N = 30, K = "G")

#Hasbrouck measure H_1(u) H_2(l)
BHa30fhcdata <- GGHadata(Bif00,Bic00, Bih00,N = 30, K = "H")

#cross spot
#--------------------------------------------------------------------

#GG  
BGGsdata <- GGHadata(Bhs300,Bzz500, Bsz50, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
BHasdata <- GGHadata(Bhs300,Bzz500, Bsz50, K = "H")

#for 5 min
#GG  
BGG5sdata <- GGHadata(Bhs300,Bzz500, Bsz50,N = 5, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
BHa5sdata <- GGHadata(Bhs300,Bzz500, Bsz50,N = 5, K = "H")


#FOR 10 min
#GG  
BGG10sdata <- GGHadata(Bhs300,Bzz500, Bsz50,N = 10, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
BHa10sdata <- GGHadata(Bhs300,Bzz500, Bsz50,N = 10, K = "H")

#FOR 30 min
#GG  
BGG30sdata <- GGHadata(Bhs300,Bzz500, Bsz50,N = 30, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
BHa30sdata <- GGHadata(Bhs300,Bzz500, Bsz50,N = 30, K = "H")

#-------------------------------------------------------------------


#for Period C Sep. 2nd 2015 to Dec. 31st 2015
#-------------------------------------------------------------------
Cif00 <- if00cl[yf4:yf3]
Cif01 <- if01cl[yf4:yf3]
Cif02 <- if02cl[yf4:yf3]
Cif03 <- if03cl[yf4:yf3]

Cic00 <- ic00cl[yc4:yc3]
Cic01 <- ic01cl[yc4:yc3]
Cic02 <- ic02cl[yc4:yc3]
Cic03 <- ic03cl[yc4:yc3]

Cih00 <- ih00cl[yc4:yc3]
Cih01 <- ih01cl[yc4:yc3]
Cih02 <- ih02cl[yc4:yc3]
Cih03 <- ih03cl[yc4:yc3]

Chs300 <- hs300cl[yf4:yf3]
Czz500 <- zz500cl[yc4:yc3]
Csz50 <- sz50cl[yc4:yc3]


#time stamp for maturity
CtidIni <- rep(20:1, rep(240,20))
CN <- ((length(Cif00)-length(CtidIni))%/%240)
CM <- CN%/%20
CtidMod <- rep(20:1, rep(240,20))
CtidMid <- rep(CtidMod, CM)
CW <-  CN - CM*20
CtidTail <- rep(CW:1, rep(240, CW))
Ctidtemp <- c(CtidIni, CtidMid)
Ctid <- c(Ctidtemp, CtidTail )

#for spot-futures pair

# spot-futures pair
#---------------------------------------------------------------------
# 1 min
#GG measure for futures
Cggdata <- GHdata(Chs300, Czz500, Csz50, Cif00, Cif01, Cif02, Cif03, 
                  Cic00, Cic01, Cic02, Cic03, 
                  Cih00, Cih01, Cih02, Cih03, tid = Ctid)

#Hasbrouck measure
Chadata <- GHdata(Chs300, Czz500, Csz50, Cif00, Cif01, Cif02, Cif03, 
                  Cic00, Cic01, Cic02, Cic03, 
                  Cih00, Cih01, Cih02, Cih03, tid = Ctid, K = "H")

# 5 min
#GG measure for futures
Cgg5data <- GHdata(Chs300, Czz500, Csz50, Cif00, Cif01, Cif02, Cif03, 
                   Cic00, Cic01, Cic02, Cic03, 
                   Cih00, Cih01, Cih02, Cih03, tid = Ctid, N = 5)

#Hasbrouck measure
Cha5data <- GHdata(Chs300, Czz500, Csz50, Cif00, Cif01, Cif02, Cif03, 
                   Cic00, Cic01, Cic02, Cic03, 
                   Cih00, Cih01, Cih02, Cih03, tid = Ctid,N = 5, K = "H")

# 10 min
#GG measure for futures
Cgg10data <- GHdata(Chs300, Czz500, Csz50, Cif00, Cif01, Cif02, Cif03, 
                    Cic00, Cic01, Cic02, Cic03, 
                    Cih00, Cih01, Cih02, Cih03, tid = Ctid, N = 10)

#Hasbrouck measure
Cha10data <- GHdata(Chs300, Czz500, Csz50, Cif00, Cif01, Cif02, Cif03, 
                    Cic00, Cic01, Cic02, Cic03, 
                    Cih00, Cih01, Cih02, Cih03, tid = Ctid,N = 10, K = "H")

# 30 min
#GG 
Cgg30data <- GHdata(Chs300, Czz500, Csz50, Cif00, Cif01, Cif02, Cif03, 
                    Cic00, Cic01, Cic02, Cic03, 
                    Cih00, Cih01, Cih02, Cih03, tid = Ctid, N = 30)

#Hasbrouck measure
Cha30data <- GHdata(Chs300, Czz500, Csz50, Cif00, Cif01, Cif02, Cif03, 
                    Cic00, Cic01, Cic02, Cic03, 
                    Cih00, Cih01, Cih02, Cih03, tid = Ctid,N = 30, K = "H")


#-----------------------------------------------------------------------
# cross futures
#---------------------------------------------------------------------
#for 1min data
#GG
CGGfhcdata <- GGHadata(Cif00,Cic00, Cih00, K = "G")

#Hasbrouck measure H_1(u) H_2(l) for 1-min
CHafhcdata <- GGHadata(Cif00,Cic00, Cih00, K = "H")

#FOR 5-MIN
#GG
CGG5fhcdata <- GGHadata(Cif00,Cic00, Cih00, N =5, K = "G")

#Hasbrouck measure H_1(u) H_2(l) for 1-min
CHa5fhcdata <- GGHadata(Cif00,Cic00, Cih00, N =5, K = "H")

#FOR 10-MIN
#GG
CGG10fhcdata <- GGHadata(Cif00,Cic00, Cih00, N =10, K = "G")

#Hasbrouck measure H_1(u) H_2(l) for 1-min
CHa10fhcdata <- GGHadata(Cif00,Cic00, Cih00, N =10, K = "H")

#FOR 30-MIN
#GG
CGG30fhcdata <- GGHadata(Cif00,Cic00, Cih00, N =30, K = "G")

#Hasbrouck measure H_1(u) H_2(l) for 1-min
CHa30fhcdata <- GGHadata(Cif00,Cic00, Cih00, N =30, K = "H")
#---------------------------------------------------------------

# cross spot
#--------------------------------------------------------------

#GG  
CGGsdata <- GGHadata(Chs300,Czz500, Csz50, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
CHasdata <- GGHadata(Chs300,Czz500, Csz50, K = "H")

#FOR 5-MIN
#GG  
CGG5sdata <- GGHadata(Chs300,Czz500, Csz50,N = 5, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
CHa5sdata <- GGHadata(Chs300,Czz500, Csz50,N =5,  K = "H")

#FOR 10 min
#GG  
CGG10sdata <- GGHadata(Chs300,Czz500, Csz50,N = 10, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
CHa10sdata <- GGHadata(Chs300,Czz500, Csz50,N =10,  K = "H")

#FOR 30 min
#GG  
CGG30sdata <- GGHadata(Chs300,Czz500, Csz50,N = 30, K = "G")

#Hasbrouck measure H_1(u) H_2(l) 
CHa30sdata <- GGHadata(Chs300,Czz500, Csz50,N =30,  K = "H")

#=====================================================================
#collect results for table

#for spot-future pair
#--------------------------------------------------------------------
#for 1 min
Tsfgh1data <- data.frame(t(Aggdata), t(Ahadata),t(Bggdata), t(Bhadata),t(Cggdata), t(Chadata))

#FOR 5 min
Tsfgh5data <- data.frame(t(Agg5data), t(Aha5data),t(Bgg5data), t(Bha5data),t(Cgg5data), t(Cha5data))

#FOR 10 min
Tsfgh10data <- data.frame(t(Agg10data), t(Aha10data),t(Bgg10data), t(Bha10data),t(Cgg10data), t(Cha10data))

#for 30 min
Tsfgh30data <- data.frame(t(Agg30data), t(Aha30data),t(Bgg30data), t(Bha30data),t(Cgg30data), t(Cha30data))



#for among spot
#--------------------------------------------------------------------
#for 1-min
Tsgh1data <- data.frame(t(AGGsdata), t(AHasdata), t(BGGsdata), t(BHasdata), t(CGGsdata), t(CHasdata))

#for 5-min
Tsgh5data <- data.frame(t(AGG5sdata), t(AHa5sdata), t(BGG5sdata), t(BHa5sdata), t(CGG5sdata), t(CHa5sdata) )

#for 10-min
Tsgh10data <- data.frame(t(AGG10sdata), t(AHa10sdata), t(BGG10sdata), t(BHa10sdata), t(CGG10sdata), t(CHa10sdata) )

#for 30-min
Tsgh30data <- data.frame(t(AGG30sdata), t(AHa30sdata), t(BGG30sdata), t(BHa30sdata), t(CGG30sdata), t(CHa30sdata) )



#for among futures
#--------------------------------------------------------------------
#for 1-min
Tfgh1data <- data.frame(t(AGGfhcdata), t(AHafhcdata), t(BGGfhcdata), t(BHafhcdata),t(CGGfhcdata), t(CHafhcdata))

#for 5-min
Tfgh5data <- data.frame(t(AGG5fhcdata), t(AHa5fhcdata), t(BGG5fhcdata), t(BHa5fhcdata),t(CGG5fhcdata), t(CHa5fhcdata))

#for 10-min
Tfgh10data <- data.frame(t(AGG10fhcdata), t(AHa10fhcdata), t(BGG10fhcdata), t(BHa10fhcdata),t(CGG10fhcdata), t(CHa10fhcdata))

#for 30-min
Tfgh30data <- data.frame(t(AGG30fhcdata), t(AHa30fhcdata), t(BGG30fhcdata), t(BHa30fhcdata),t(CGG30fhcdata), t(CHa30fhcdata))



































