#' @title Function post_bmr
#' @description Simulates the joint posterior based upon a dataset and specified marginal posterior distribution of the mean of selected variables.
#' @seealso \code{\link{tweights_bmr}}
#' @export
#' @param nsims The number of posterior simulations to draw.
#' @param weights_bmr An object of class 'tweights_bmr' created using the 'tweights_bmr' function.
#' @return 
#' A matrix of simulations from the posterior.
#' @examples
#' #Use winsorized marginal to keep marginal simulation within feasible bootstrap region
#' winsor=function(marginalSims,y)  {
#'   l=min(y)
#'   u=max(y)
#'   ifelse(marginalSims<l,l,ifelse(marginalSims>u,u, marginalSims))
#' }
#' #Create an example marginal posterior
#' marginal = list(Sepal.Length=winsor(rnorm(10000,mean=5.8, sd=.2),iris$Sepal.Length),
#'                Sepal.Width=winsor(rnorm(10000,mean=3,sd=.2), iris$Sepal.Width),
#'                Petal.Length=winsor(rnorm(10000,mean=3.7,sd=.2), iris$Petal.Length)
#' )
#' 
#' #simulate
#' w = tweights_bmr(dataset = iris, marginal = marginal, silent = TRUE)
#' post_sims = post_bmr(1000, weights = w)
#' 

post_bmr=function(nsims, weights_bmr) {
  if(!("tweights_bmr" %in% class(weights_bmr)))
    stop("'weights_bmr' must be an object of class 'tweights_bmr' from the 'tweights_bmr' function.")
  if(!is.numeric(nsims))
    stop("'nsims' must be numeric.")
  if(length(nsims)!=1)
    stop("'nsims' must be length 1.")
  
  
  p=nrow(weights_bmr$Csqrt)
  
  z = weights_bmr$Csqrt %*% matrix(rnorm(p*nsims),nrow=p,ncol=nsims)
  u = apply(z, 1, function(z) list(pnorm(z))) #list to stop simplification
  sims= mapply(function(u,marginal) as.numeric(quantile(marginal, probs = u[[1]])),
             u, weights_bmr$marginal)
  if(nsims==1)
    sims=matrix(sims,nrow=1)
  colnames(sims)=names(weights_bmr$marginal)
  return(sims)
}

