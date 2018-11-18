RFactor <- function(Data,initialmonth,months,registration,nyear){


  data<-Data$date
  Rainfall<-Data$rainfall

  data<-as.Date(data,"%d/%m/%Y")

  year<-format(data,"%Y")
  month<-format(data,"%m")
  day<-format(data,"%d")

  o<-initialmonth

  sizesum <- numeric(length(data))
  erosivitytestsum <- numeric(length(data))

  pannual <- numeric(length(data))
  eannual <- numeric(length(data))
  nceannual <- numeric(length(data))
  ncneannual <- numeric(length(data))
  ceannual <- numeric(length(data))
  cneannual <- numeric(length(data))

  monthly<-c("01","02","03","04","05","06","07","08","09","10","11","12")
  account<-rep(monthly,times=50)

  print(c("Year","Month","Rain","EI30","N. erosive","N. non erosive","Erosive rain", "Non erosive rain")) 


  for (m in initialmonth:(initialmonth+months-1)){

    r<-rep(0,length(data))

    finalrainfallsum2<-rep(0,length(data))
    g<-o

    for (g in g:length(data)){
      if ((month[g]==account[m])|(month[g]==account[m])) finalrainfallsum2[g]<-Rainfall[g] else break
    }

    finalrainfallsum1<-sum(finalrainfallsum2)

    p<-o+1
    o<-p

    for (o in o:length(data)){
      if ((month[o]==account[m])|(month[o]==account[m])) r[o]<-Rainfall[o] else break
    }
    r<-r[p:o]
    sizesum[m] <- sum(r)
    rainfall1<-(1:length(data))
    y<-c(0,rainfall1)
    z<-r
    x<-r
    i<-1
    k<-2

    size<-length(z)
    z<-c(z,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    x<-c(x,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

    for (i in i:size){
      if (x[i]==0) z[i]<-y[1] else break}

    if (registration==5) {
      for (j in 1:size){
        for (i in (i-1):size){
          z[i]<-y[k]
          if ((x[i+1]+x[i+2]+x[i+3]+x[i+4]+x[i+5]+x[i+6]+x[i+7]+x[i+8]+x[i+9]+
               x[i+10]+x[i+11]+x[i+12]+x[i+13]+x[i+14]+x[i+15]+x[i+16]+x[i+17]+x[i+18]+
               x[i+19]+x[i+20]+x[i+21]+x[i+22]+x[i+23]+x[i+24]+x[i+25]+x[i+26]+x[i+27]+
               x[i+28]+x[i+29]+x[i+30]+x[i+31]+x[i+32]+x[i+33]+x[i+34]+x[i+35]+x[i+36]+
               x[i+37]+x[i+38]+x[i+39]+x[i+40]+x[i+41]+x[i+42]+x[i+43]+x[i+44]+x[i+45]+
               x[i+46]+x[i+47]+x[i+48]+x[i+49]+x[i+50]+x[i+51]+x[i+52]+x[i+53]+x[i+54]+
               x[i+55]+x[i+56]+x[i+57]+x[i+58]+x[i+59]+x[i+60]+x[i+61]+x[i+62]+x[i+63]+
               x[i+64]+x[i+65]+x[i+66]+x[i+67]+x[i+68]+x[i+69]+x[i+70]+x[i+71]+x[i+72])==0) break
        }
        k<-k+1
        i<-i+1

        for (i in i:size){
          if (z[i]==0) z[i]<-y[1] else break
        }
        if (i==size) break
      }
    }

    if (registration==10) {
      for (j in 1:size){
        for (i in (i-1):size){
          z[i]<-y[k]
          if ((x[i+1]+x[i+2]+x[i+3]+x[i+4]+x[i+5]+x[i+6]+x[i+7]+x[i+8]+x[i+9]+
               x[i+10]+x[i+11]+x[i+12]+x[i+13]+x[i+14]+x[i+15]+x[i+16]+x[i+17]+x[i+18]+
               x[i+19]+x[i+20]+x[i+21]+x[i+22]+x[i+23]+x[i+24]+x[i+25]+x[i+26]+x[i+27]+
               x[i+28]+x[i+29]+x[i+30]+x[i+31]+x[i+32]+x[i+33]+x[i+34]+x[i+35]+x[i+36])==0) break
        }
        k<-k+1
        i<-i+1

        for (i in i:size){
          if (z[i]==0) z[i]<-y[1] else break
        }
        if (i==size) break
      }
    }

    if (registration==15) {
      for (j in 1:size){
        for (i in (i-1):size){
          z[i]<-y[k]
          if ((x[i+1]+x[i+2]+x[i+3]+x[i+4]+x[i+5]+x[i+6]+x[i+7]+x[i+8]+x[i+9]+
               x[i+10]+x[i+11]+x[i+12]+x[i+13]+x[i+14]+x[i+15]+x[i+16]+x[i+17]+x[i+18]+
               x[i+19]+x[i+20]+x[i+21]+x[i+22]+x[i+23]+x[i+24])==0) break
        }
        k<-k+1
        i<-i+1

        for (i in i:size){
          if (z[i]==0) z[i]<-y[1] else break
        }
        if (i==size) break
      }
    }

    z<-z[1:(length(z)-37)]
    x<-x[1:(length(x)-37)]

    znew<-z

    l<-3:length(z)
    I30<-c(x[1],x[1]+x[2],x[l-2]+x[l-1]+x[l])*2

    w<-x*6 #time interval   #w = I10

    for (m in 1:length(z)){
      if (w[m]==0) w[m]<-w[m] else w[m]<-0.119+0.0873*log10(w[m])
    }
    KE<-w
    Eunitary<-KE*x
    Einterval<-Eunitary
    KEfinal<-Eunitary

    more<-0
    for (i in 1:length(z)){
      if (znew[i]>more) more<-znew[i]
    }

    sum<-rep(0,more)

    for (n in 1:more){
      currentsum<-rep(0,length(z))
      for (p in 1:length(z)){
        if (znew[p]==n) currentsum[p]<-KEfinal[p]
      }
      sum[n]<-sum(currentsum)
    }

    intervalsum<-sum

    #condition3

    maximum<-rep(0,more)
    finalmaximum<-rep(0,more)

    for (m in 1:more){
      for (q in 1:length(z)){
        if (znew[q]==m) maximum[m]<-I30[q]
        if (maximum[m]>finalmaximum[m]) finalmaximum[m]<-maximum[m]
      }
    }

    #condition2

    Eintervalsum<-rep(0,more)
    for (n in 1:more){
      currentEinterval<-rep(0,length(z))
      for (p in 1:length(z)){
        if (znew[p]==n) currentEinterval[p]<-Einterval[p]
      }
      Eintervalsum[n]<-sum(currentEinterval)
    }

    rainfallsum<-rep(0,more)

    for (n in 1:more){
      currentrainfallsum<-rep(0,length(z))
      for (p in 1:length(z)){
        if (znew[p]==n) currentrainfallsum[p]<-x[p]
      }

      rainfallsum[n]<-sum(currentrainfallsum)

    }

    #condition1

    rain<-c("erosive","no erosive")

    finalrainfallsum<- finalrainfallsum1

    EI30<-rep(0,length(sum))
    for (u in 1:length(sum)){
      if (rainfallsum[u]>10) EI30[u]<-Eintervalsum[u]*finalmaximum[u] else EI30[u]<-0
    }

    totalerosivity<-round(sum(EI30),1)

    #############################################################################
    ####################### Condition 1 #####################
    #############################################################################

    newrain<-rep(0,more)

    for (i in 1:more){
      if (rainfallsum[i]>10) newrain[i]<-rain[1] else newrain[i]<-rain[2]

    }

    um<-rep(1,times=more)
    if ((t(um)%*%rainfallsum)==0) nonerosivenumber<-0 else {
      nonerosivenumber<-0
      for (i in 1:more){
        if (newrain[i]==rain[2]) nonerosivenumber<-nonerosivenumber+1
      }
    }

    erosivenumber<-0

    for (i in 1:more){

      if (newrain[i]==rain[1]) erosivenumber<-erosivenumber+1
    }

    erosivesum<-rep(0,more)
    for (i in 1:more){
      if (newrain[i]==rain[1]) erosivesum[i]<-rainfallsum[i]
    }

    totalerosivesum<-sum(erosivesum)

    nonerosivesum<-rep(0,more)
    for (i in 1:more){
      if (newrain[i]==rain[2]) nonerosivesum[i]<-rainfallsum[i]
    }
    totalnonerosivesum<-sum(nonerosivesum)

    print(c(year[o-1],month[o-1],finalrainfallsum,totalerosivity,erosivenumber,nonerosivenumber,totalerosivesum, totalnonerosivesum))

    erosivitytestsum[o] <- totalerosivity

    pannual[o] <- finalrainfallsum
    eannual[o] <- totalerosivity
    nceannual[o] <- erosivenumber
    ncneannual[o] <- nonerosivenumber
    ceannual[o] <- totalerosivesum
    cneannual[o] <- totalnonerosivesum

    if (month[o]=="01"){

      print(c("TOTAL","YEAR",sum(pannual),sum(eannual),sum(nceannual),sum(ncneannual),sum(ceannual),sum(cneannual)))
      pannual <- numeric(length(data))
      eannual <- numeric(length(data))
      nceannual <- numeric(length(data))
      ncneannual <- numeric(length(data))
      ceannual <- numeric(length(data))
      cneannual <- numeric(length(data))
    }
  }
  print(c("TOTAL","YEAR",sum(pannual),sum(eannual),sum(nceannual),sum(ncneannual),sum(ceannual),sum(cneannual)))
  print(c("FATOR R",sum(erosivitytestsum)/nyear))
}


