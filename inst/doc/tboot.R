## ----sim, cache = FALSE-------------------------------------------------------
library(tboot)
set.seed(2018)
color   <- sample(c("brown", "green", "blue"), 300, replace = TRUE)
quant1  <- rnorm(300) + ifelse(color=="red", 1, 0)
quant2  <- rnorm(300) + quant1*.5
bin1    <- ifelse(quant1+rnorm(300) > 1, 1, 0)
bin2    <- ifelse(quant2+rnorm(300) > 1, 1, 0)
simData <- data.frame(color, quant1, quant2, bin1, bin2)
head(simData)

## ----dataset, cache = FALSE---------------------------------------------------
dataset=as.matrix(cbind(
    colorBlue=ifelse(simData$color=="blue",1,0),
    colorBrown=ifelse(simData$color=="brown",1,0),
    simData[,-1]))
colMeans(dataset)

## ----target, cache = FALSE----------------------------------------------------
target <-  c(colorBlue=0.4,
             colorBrown=0.4,
             quant1=0.4,
             quant2=0.4,
             bin1=0.4,
             bin2=0.4) 

## ----weights, cache = FALSE---------------------------------------------------
weights <- tweights(dataset = dataset, target = target)

## ----boot, cache = FALSE------------------------------------------------------
boot <- tboot(weights = weights, nrow = 1e5)

## ----compare, cache = FALSE---------------------------------------------------
colMeans(boot)

## ----hist, cache = FALSE------------------------------------------------------
hist(weights$weights, breaks=25)
abline(v=1/300,col="red")

## ----targetweight, cache = FALSE----------------------------------------------
weights <- tweights(dataset = dataset, 
                    target = c(quant1=0.5, quant2=0.5))

## ----boot2, cache = FALSE-----------------------------------------------------
boot <- tboot(weights, nrow = 1e5)
rbind("dataset mean"    = colMeans(dataset),
      "tbootstrap mean" = colMeans(boot))

## ----boot21, cache = FALSE----------------------------------------------------
pltdta=data.frame(dataset[,c("quant1", "quant2")], weights=weights$weights)
library(ggplot2)
ggplot(pltdta, aes(x=quant1, y=quant2, color=weights)) + geom_point() + 
  geom_point(aes(x=.6, y=.6), shape = 3, colour = "red", size = 2, stroke = 3)+
  geom_point(aes(x=mean(quant1), y=mean(quant2)),
             shape = 3, colour = "black",  size = 2, stroke = 3)


## ----var, cache = FALSE-------------------------------------------------------
dataset=cbind(dataset, quant1_2= (dataset[,"quant1"]-0.5)^2)
weights <- tweights(dataset = dataset, 
                    target = c(quant1=0.5, quant1_2=0.75))
boot <- tboot(weights, nrow = 1e5)
var(dataset[,"quant1"])
var(boot[,"quant1"])
rbind("dataset mean"    = colMeans(dataset),
      "tbootstrap mean" = colMeans(boot))

## ----boot3, cache = FALSE-----------------------------------------------------
x1=rnorm(1000)
x2=rnorm(1000)*sqrt(.1) + x1*sqrt(.9)
dataset2=data.frame(x1=x1, x2=x2)

weights_no_augmentation <- tweights(dataset = dataset2, 
                    target = c(x1=0.2, x2=-0.2))
weights_augmentation <- tweights(dataset = dataset2, 
                    target = c(x1=0.2, x2=-0.2), Nindependent=1)
boot_no_augmentation <- tboot(weights_no_augmentation, nrow = 1e5)
boot_augmentation <- tboot(weights_augmentation, nrow = 1e5)
cor(boot_no_augmentation)
cor(boot_augmentation)


