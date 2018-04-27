#####################################################(Random and Noise)

MyFunction <- function(x) {
  MyFunction <- 2*x^4 + 4*x^3 + 8*x^2 + 16*x
}

MyFunction2 <- function(x) {
  MyFunction2 <- 0.05*(x-1)^2 + (3 - 2.9*exp(-2.77527*x^2))*(1-cos(x*(4-50*exp(-2.77527*x^2))))
}

NumberOfDots <- 200 #Размер выборки
NoiseRatio <- 0.2 #Коэффициэнт уровня помех
RandomValue <- runif(NumberOfDots, min=-1, max=1) #Кси

TrueX <- seq(from=-3, to=3, by=6/(NumberOfDots-1))
TrueY <- MyFunction2(TrueX)

NoiseX <- runif(NumberOfDots, min=-3, max=3) #Random
NoiseX <- sort(NoiseX)
Y <- MyFunction2(NoiseX)
Interference <- NoiseRatio*RandomValue*Y #Помехи
NoiseY <- Y + Interference

plot(TrueX, TrueY, type="l", xlim=c(-3,3), ylim=c(0,10), lwd=2, axes=FALSE , ann=FALSE)
par(new=TRUE)
plot(NoiseX, NoiseY, xlab="X", ylab="Y", type="l", xlim=c(-3,3), ylim=c(0,10), lwd=2, col="#FF4500")
mtext("Func2", side=3, line=0.7)

BlurCoef <- 0.0425 #Коэффициэнт размытия
Bell <- matrix(nrow=NumberOfDots, ncol=NumberOfDots) #Колоколообразная функция
for(i in 1:NumberOfDots)
{
  for(j in 1:NumberOfDots)
  {
    buffer <- abs((NoiseX[i]-NoiseX[j])/BlurCoef)
    if (buffer <= 1) {
      Bell[i,j] <- 1 - buffer
    } else {
      Bell[i,j]<-0
    }
  }
}
Bell1 <- vector(mode="numeric", NumberOfDots)
Chislitel <- vector(mode="numeric", NumberOfDots)
for (i in 1:NumberOfDots)
{
  Bell1[i] <- sum(Bell[i,])
  for (j in 1:NumberOfDots)
  {
    Chislitel[i] <- Chislitel[i] + NoiseY[j]*Bell[i,j]
  }
}

Rozenblatt_Parzen <- function(I) {
  Rozenblatt_Parzen <- Chislitel[I] / Bell1[I]
}

NonParamY <- vector(mode="numeric", NumberOfDots)
for(i in 1:NumberOfDots) {
  NonParamY[i] <- Rozenblatt_Parzen(i)
}

par(new=TRUE)
plot(NoiseX, NonParamY, type="l", xlim=c(-3,3), ylim=c(0,10), col='blue',axes=FALSE , ann=FALSE)

######################(Функция коэффициэнта размытия для непараметрики)
BlurCoef <- 0.0025
sch <- 1
BlurPlot <- round(seq(from=0, to=1, length=2*NumberOfDots), 4) #Ось X
BlurSum <- vector(mode="numeric", NumberOfDots*2) #Массив сумм
Blur <- vector(mode="numeric", NumberOfDots) #Сумма для одного значения коэф-та
while (sch <= (NumberOfDots*2+1)) {
  for (i in 1:NumberOfDots) {
    STD <- (Y[i] - NonParamY[i])^2
    if (i == 1) { 
      Blur[i]<-STD 
    } else {
      STDD <- Blur[i-1]
      Blur[i] <- STDD + STD
    }
  }
  k <- BlurCoef*NumberOfDots*2
  BlurSum[k] <- Blur[i]
  BlurCoef <- 0.0025 + 0.0025*k
  sch <- sch + 1
  for(i in 1:NumberOfDots) {
    for(j in 1:NumberOfDots) {
      buffer <- abs((NoiseX[i] - NoiseX[j])/BlurCoef)
      if (buffer <= 1) {
        Bell[i,j] <- 1 - buffer
      } else {
        Bell[i,j] <- 0
      }
    }
  }
  Chisl <- 0
  for (i in 1:NumberOfDots) {
    Bell1[i] <- sum(Bell[i,])
    for (j in 1:NumberOfDots) {
      Chisl <- Chisl + NoiseY[j]*Bell[i,j]
    }
    Chislitel[i] <- Chisl
    Chisl <- 0
  }
  for(i in 1:NumberOfDots) {
    NonParamY[i] <- Rozenblatt_Parzen(i)
  }
  print(BlurCoef)
}
Min <- 1000000
NumberOfMin <- 0
for(i in 1:NumberOfDots) {
  if (BlurSum[i] != 0 & BlurSum[i] < Min) {
    Min <- BlurSum[i]
    NumberOfMin <- i
  }
}
plot(BlurPlot, BlurSum, xlab="Cs", ylab="W(Cs)", type="l", col="red")
title("Function of Blur Coefficient")
mtext(Min, side=3, line=0.7)
mtext(NumberOfMin/2/NumberOfDots, side=3, line=0)
abline(v=NumberOfMin/2/NumberOfDots, col="black")
abline(h=Min, col="black")
