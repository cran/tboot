## ----sim, cache = FALSE-------------------------------------------------------
library(tboot)
set.seed(2020)
quant1  <- rnorm(200) + 1
bin1    <- ifelse( (.5*quant1 + .5*rnorm(200)) > .5, 1, 0)
bin2    <- ifelse( (.5*quant1 + .5*rnorm(200)) > .5, 1, 0)
simData <- data.frame(quant1, bin1, bin2)
head(simData)

## ----target, cache = FALSE----------------------------------------------------
marginal_active <-  list(quant1=rnorm(5000, mean=.5, sd=.2),
                         bin1=rbeta(5000, shape1 = 50,shape2=50),
                         bin2=rbeta(5000, shape1 = 60,shape2=40))
marginal_pbo <-  list(quant1=rnorm(5000, mean=.2, sd=.2),
                      bin1=rbeta(5000, shape1 = 20,shape2=80),
                      bin2=rbeta(5000, shape1 = 30,shape2=70))

## ----weights, cache = FALSE---------------------------------------------------
bmr_active <- tweights_bmr(dataset = simData, marginal = marginal_active)
bmr_pbo <- tweights_bmr(dataset = simData, marginal = marginal_pbo)

## ----post_bmr, cache = FALSE, fig.height = 6, fig.width = 6, fig.align = "center"----
samples <- rbind(data.frame(trt="active", post_bmr(nsims=1e3, bmr_active)),
                 data.frame(trt="pbo", post_bmr(nsims=1e3, bmr_pbo)))
head(samples)

## ----pairsgraph, cache = FALSE, fig.height = 6, fig.width = 6, fig.align = "center"----
pairs(samples[,-1], col=ifelse(samples=="active","red","blue"), pch='.', cex=.5)

## ----marginal dist, cache = FALSE, fig.height = 6, fig.width = 6, fig.align = "center"----
library(ggplot2)
pltdta=do.call(rbind, lapply(c("quant1","bin1", "bin2"),
                             function(nm) {
                               rbind(data.frame(type="BMR", var=nm, trt=samples$trt,
                                                val=samples[[nm]]), 
                                     data.frame(type="marginal", var=nm,
                                                trt="active", 
                                                val=marginal_active[[nm]]),
                                     data.frame(type="marginal", var=nm, 
                                                trt="pbo", 
                                                val=marginal_pbo[[nm]]))
                             }))

  ggplot(pltdta, aes(fill=type, x=val)) + 
  geom_density(alpha=.3) + facet_grid(var~trt, scales = "free")



## ----tboot_bmr, cache = FALSE-------------------------------------------------
active_sample=tboot_bmr(nrow=100, weights_bmr=bmr_active)
head(active_sample)
                   

## ----attr, cache = FALSE------------------------------------------------------
attr(active_sample, "post_bmr")
                   

## ----sim_trial, cache = FALSE-------------------------------------------------
#Manage any errors by assuming the pvalue failed to reach statistical
#significance (i.e. pvalue is 1) but keep track of such errors. 
errorTrackGlobal=list()
manageError=function(expr)  {
  tryCatch(eval(quote(expr)), error=function(e){
    errorTrackGlobal[[length(errorTrackGlobal)+1]] <<- e$message
    return(1)
  }) 
}


#create function to simulate and analyze one virtual trial
sim_and_analyze=function() {
  active_sample=tboot_bmr(100, bmr_active)
  pbo_sample=tboot_bmr(100, bmr_pbo)
  data.frame(
    p_quant1=manageError(t.test(active_sample$quant1,pbo_sample$quant1)$p.value),
    p_bin1=manageError(fisher.test(active_sample$bin1,pbo_sample$bin1)$p.value),
    p_bin2=manageError(fisher.test(active_sample$bin2,pbo_sample$bin2)$p.value)
  )
}
#Simulate Pvalues
p_sim=do.call(rbind, replicate(100, sim_and_analyze(), simplify = FALSE))
head(errorTrackGlobal)
head(p_sim)
                   

