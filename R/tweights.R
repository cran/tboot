#' @title Function \code{tweights}
#' @description Returns a vector \code{p} of resampling probabilities 
#' such that the column means of \code{tboot(dataset = dataset, p = p)}
#' equals \code{target} on average.
#' @seealso \code{\link{tboot}} 
#' @export
#' @param dataset Data frame or matrix to use to find row weights.
#' @param target Numeric vector of target column means. If the 'target' is named, then all elements of names(target) should be in the dataset.
#' @param distance The distance to minimize. Must be either 'euchlidean,' 'klqp' or 'klpq' (i.e. Kullback-Leibler). 'klqp' which is exponential tilting is recommended.
#' @param maxit Defines the maximum number of iterations for optimizing 'kl' distance.
#' @param tol Tolerance. If the achieved mean is to0 far from the target (i.e. as defined by tol) an error will be thrown.
#' @param warningcut Sets the cutoff for determining when a large weight will trigger a warning.
#' @param silent Allows silencing of some messages.
#' @param Nindependent Assumes the input also includes 'Nindependent' samples with independent columns. See details.
#' @details
#' Let \eqn{p_i = 1/n} be the probability of sampling subject \eqn{i} from a dataset with \eqn{n} individuals (i.e. rows of the dataset) in the classic resampling with replacement scheme.
#' Also, let \eqn{q_i} be the probability of sampling subject \eqn{i} from a dataset with \eqn{n} individuals in our new resampling scheme. Let \eqn{d(q,p)} represent a distance between the two resampling schemes.  The \code{tweights}
#' function seeks to solve the problem: 
#' \deqn{q = argmin_p d(q,p)}
#' Subject to the constraint that:
#' \deqn{ sum_i q_i = 1} and
#' \deqn{  dataset' q = target}
#' where dataset is a n x K matrix of variables input to the function.
#' 
#'   \deqn{d_{euclidian}(q,p) = sqrt( \sum_i (p_i-q_i)^2 )}
#'   \deqn{d_{kl}(q,p) = \sum_i (log(p_i) - log(q_i))}
#'
#' Optimization for Euclidean distance is a quadratic program and utilizes the ipop function in kernLab.
#' Optimization for the others utilize a Newton-Raphson type iterative algorithm.
#' 
#' If the original target cannot be achieved. Something close to the original target will be selected.
#' A warning will be produced and the new target displayed.
#' 
#' The 'Nindependent' option augments the dataset by assuming some additional specified
#' number of patients. These patients are assumed to made up of a random bootstrapped sample
#' from the dataset for each variable marginally leading to independent variables. 
#' @return 
#' An object of type \code{tweights}. This object contains the following components:
#' \describe{
#'   \item{weights}{Tilted weights for resampling}
#'   \item{originalTarget}{Will be null if target was not changed.}
#'   \item{target}{Actual target that was attempted.}
#'   \item{achievedMean}{Achieved mean from tilting.}
#'   \item{dataset}{Inputed dataset.}
#'   \item{X}{Reformated dataset.}
#'   \item{Nindependent}{Inputed 'Nindependent' option.}
#' }
#' @examples
#'  target=c(Sepal.Length=5.5, Sepal.Width=2.9, Petal.Length=3.4)
#'  w = tweights(dataset = iris, target = target, silent = TRUE)
#'  simulated_data = tboot(nrow = 1000, weights = w)

tweights <-function(
  dataset,
  target = apply(dataset, 2, mean),
  distance="klqp",
  maxit = 1000,
  tol=1e-8,
  warningcut=0.05,
  silent=FALSE,
  Nindependent=0
  ) {
  
  originalTarget=NULL #will be replaced if we need to fix the target
  originalDataset=dataset
  

  
  if(is.null(colnames(dataset)))
    stop("'dataset' must have named columns starting with version 0.2.0.")


  #Check input
  if(!is.numeric(target)) {
    stop("'target' must be a named numeric vector.")
  } else if(is.null(names(target))) {
      stop("'target' must be a named vector starting with version 0.2.0.")
  } else if(!all(names(target) %in% colnames(dataset))) {
      stop("Some names of 'target' have no match in colnames  'dataset.'")
  } else {
    bounds=apply(dataset[,names(target), drop=FALSE], 2, function(x) return(c(min(x), max(x))))
    if(any(target<bounds[1,]) || any(target>bounds[2,]))
      stop("Target for each variable must be between maximum and minimum of the values in the dataset.")
  }
  
  dataset=dataset[,names(target)]
  dataset=as.matrix(dataset)
  
  if(!is.numeric(dataset))
    stop("All targeted columns of 'dataset' must be numeric.")

  if(  sum(!is.finite(target))  > 0)
    stop("'target' is not valid. 'target' must be finite non missing.")
  
  if(  sum(!is.finite(dataset))  > 0)
    stop("'dataset' is not valid. 'dataset' must be finite non missing for all values.")
  
  if( any(apply(dataset, 2, var)==0) )
    stop("At least on column of 'dataset' has no variation (i.e. all subjects are the same for at least one column). Consider removing a column and its 'target'.")
  
  if(Nindependent!=0) {
    if(floor(Nindependent)!=Nindependent)
      stop("'Nindependent' must be an integer.")
   # browser()
    #find a target that is achievable set the independent weights
    # to offset the so that we can achieve the bound.
    possible_target = .how_close(dataset, target, supress_warnings=TRUE)  
    independent_target = target - (possible_target-target)*1.01 #1.01 used to numerically insure not on boundary of achievable
    independent_target = ifelse(independent_target<bounds[1,],bounds[1,],independent_target)
    independent_target = ifelse(independent_target>bounds[2,],bounds[2,],independent_target)
    
    augmentWeights=lapply(names(target), function(nm){
      ret=tweights(
        dataset[,nm, drop=FALSE],
        target = independent_target[nm],
        distance=distance,
        maxit = maxit,
        tol=tol,
        warningcut=1,#dont warn
        silent=TRUE)#dont talk - remember it is the final check that matters
      return(ret$weights)
    })
    names(augmentWeights)=colnames(dataset)
    
    augmentMeans=sapply(names(target), function(nm) 
      return(crossprod( dataset[,nm], augmentWeights[[nm]])))
    augmentMeansrep=do.call(rbind, 
                            replicate(Nindependent, augmentMeans,
                                      simplify = FALSE))
    dataset=rbind(dataset,augmentMeansrep)
  } else
    augmentWeights=NULL
  
  #Include probability constraint to sum to 1
  b <- c(1, target)
  A <- (as.matrix(
    cbind(
      int = 1,
      dataset
    )
  ))
  n=nrow(dataset)
  
  if(distance=="euchlidean") {
    #Find best weights using euchlidian distance - if the target is infeasible try using ipop to get a closer.
    opt <- tryCatch(
      solve.QP(dvec =rep(1/n,n), Dmat =diag(n),
               Amat=cbind(A,diag(n)),
               bvec=c(b,rep(0,n)), meq =length(b), factorized = TRUE),
      error = function(e) NULL)
    
    if(is.null(opt)) {
      originalTarget=target
      target = .how_close(dataset, target)  #find a target that is achievable
      b <- c(1, target)
      opt=solve.QP(dvec =rep(1/n,n), Dmat =diag(n),
                   Amat=cbind(A,diag(n)),
                   bvec=c(b,rep(0,n)), meq =length(b), factorized = TRUE) #This time error out if you cant find it
    }
    
    return(.print_ret(originalDataset,weights=opt$solution, dataset, target, originalTarget,
                      warningcut, silent, Nindependent, augmentWeights))
    
  } else if(distance=="klpq") {
    warning("The 'klpq' distance is difficult to optimize and may lead to unstable results. It has not been validated and is not recomended.")
    opt=.newtonKLpq(A,b,maxit,tol)
    if(opt$steps==maxit) {
      originalTarget=target
      target = .how_close(dataset, target)  #find a target that is achievable
      b <- c(1, target)
      opt=.newtonKLpq(A,b,maxit,tol)
      if(opt$steps==maxit)
        stop("Optimization failed. Maximum iterations reached.")
    }
    
    return(.print_ret(originalDataset,weights=opt$weights, dataset, target,originalTarget,
                      warningcut,silent, Nindependent, augmentWeights))
  } else if(distance=="klqp") {
    opt=.newtonKLqp(A,b,maxit,tol)
    if(opt$steps==maxit) {
      originalTarget=target
      target = .how_close(dataset, target)  #find a target that is achievable
      b <- c(1, target)
      opt=.newtonKLqp(A,b,maxit,tol)
      if(opt$steps==maxit)
        stop("Optimization failed. Maximum iterations reached.")
    }
    return(.print_ret(originalDataset,weights=opt$weights, dataset, target,originalTarget, 
                      warningcut,silent, Nindependent, augmentWeights))
  } else
    stop("distance must be 'klqp', 'klpq' or 'euchlidean.'")
}



.how_close <- function(dataset, target, supress_warnings=FALSE) {
  xstar=scale(dataset, center = FALSE)
  scl=attr(xstar,"scaled:scale")
  targetstar=target/scl
  
  #optimize and check convergence
  opt = ipop(c=-as.vector(xstar %*% targetstar), H=t(xstar),
             A=matrix(1,1,nrow(xstar)), b=1, r=0,
             l=rep(0,nrow(xstar)), u=rep(1,nrow(xstar)), maxiter=100)
  if(how(opt)!="converged")
    stop("'ipop' did not converge.")
  
  best=as.vector(t(xstar) %*% primal(opt))*scl
  if(!supress_warnings)
    warning(paste("Target apears to not be achievable. Replacing with the nearest achievable target in terms of scaled euclidian distance. New target is: \n", paste(best,collapse=", "),"\n"))
  return(best)
}

.newtonKLpq = function(A, b, maxit, tol) {
  #Parameters for transformed problem to (hopefuly) make the optimization numerically stable
  s=svd(A)
  if( (s$d[length(s$d)]/s$d[1]) < 1e-6)
    warning("Matrix is ill conditioned (e.g. columns may be colinear). Strongly consider removing some columns, using more data or not using 'tboot'.")
  vdinv = s$v %*% diag(1/s$d)
  x_star = (A %*% vdinv )
  target_star =   as.vector(t(vdinv) %*% b)
  lambda_n = .normalize(x_star, 
                        as.vector( t(s$v %*% diag(s$d)) %*% c(1,rep(0, length(target_star)-1)) ))
  
  for(steps in 1:maxit) {
    xlambda_n=x_star %*% lambda_n      
    pi_n=as.vector(1/xlambda_n)
    tmp=x_star * (pi_n)
    negF_deriv=  crossprod(tmp)
    dif=as.vector(pi_n %*% x_star -target_star)
    if(any(pi_n<0))
      stop("Error in optimization step. Target may be to far from data.")
    
    if(any(is.na(dif)))
      stop("Error in optimization step. Target may be to far from data.")

    if(max(abs(dif))<tol)
      break
    
    lambda_proposal= lambda_n + .solveTrap(negF_deriv, dif)
    xlambda_proposal=x_star %*% lambda_proposal 
    boundary=xlambda_n/(xlambda_n-xlambda_proposal)
    boundary=boundary[boundary>0]
    if(length(boundary)>0) {
      boundary=min(boundary)
      if(boundary>1) {
        alpha=1
      } else {
        alpha=.5*boundary
      }
    }
    else
      alpha=1
    # if(alpha< 0 )
    #   browser()
    lambda_n=as.vector(alpha*lambda_proposal + (1-alpha)*lambda_n)
  }
  xlambda_n=x_star %*% lambda_n      
  pi_n=as.vector(1/xlambda_n)
  return(list(steps=steps, weights=pi_n))
} 



.renormKLqp=function(lambda_n, x_star,lambda_nnorm_direction) {
  xlambda_n=x_star %*% lambda_n 
  mx=max(xlambda_n)
  pi_n=as.vector(exp(xlambda_n -mx))
  log_cur_total_prob = log(sum(pi_n)) + mx
  lambda_n = lambda_n -  lambda_nnorm_direction * log_cur_total_prob
  return(lambda_n)
}

.line_searchKLqp=function(x_star,target_star,lambda_nnorm_direction, lambda_n, v0) {
  g=function(alpha) {
    lambda_n=lambda_n + v0*alpha
    xlambda_n=x_star %*% lambda_n 
    mx=max(xlambda_n)
    pi_n=as.vector(exp(xlambda_n -mx))
    cur_total_prob = sum(pi_n)
    pi_n = pi_n/cur_total_prob
    dif=as.vector(pi_n %*% x_star -target_star)
    return(dif %*% dif)
  }
  opt=optimise(g,interval=c(0,1))
  alpha=opt$minimum
  ret=lambda_n + v0*alpha

  return(ret)#.renormKLqp(ret, x_star,lambda_nnorm_direction))
}

.solveTrap = function(A,b) {
  tryCatch(solve(A,b), error = function(e) {
    warning("Matrix was not invertible. Using pseudo inverse.\n")
    e=eigen(A, symmetric=TRUE)
    eval= e$values
    eval[eval < (max(eval)*1E-5) ]=1
    return(e$vectors %*% (diag(1/eval) %*% (t(e$vector) %*% b)))
  })
}




.newtonKLqp=function(A, b, maxit, tol, maxrestart=2) {
  
  #Parameters for transformed problem to (hopefuly) make the optimization numerically stable
  s=svd(A)
  if( (s$d[length(s$d)]/s$d[1]) < sqrt(1e-6))
    warning("Matrix is ill conditioned (e.g. columns may be colinear).  Strongly consider removing some columns, using more data or not using 'tboot.'\n")
  vdinv = s$v %*% diag(1/s$d)
  x_star = (A %*% vdinv )
  target_star =   as.vector(t(vdinv) %*% b)
  
  #setup for renormalization
  lambda_nnorm_direction =  s$d * s$v[1,]
  lambda_nnorm_direction = lambda_nnorm_direction/mean(x_star %*% lambda_nnorm_direction)
  
  #start
  lambda_n =as.vector( t(s$v %*% diag(s$d)) %*% c(-log(nrow(A)),rep(0, length(target_star)-1)) )
  restart=0
  
  for(steps in 1:maxit) {
    xlambda_n=x_star %*% lambda_n
    mx=max(xlambda_n) # suptract mx below throughout to avoid overflow
    pi_n_expmx=as.vector(exp(xlambda_n-mx))
    F_deriv_expmx=  crossprod(x_star * pi_n_expmx, x_star)
    m_expmx=pi_n_expmx %*% x_star
    #dif=as.vector(m-target_star)
    dif_expmx=as.vector(m_expmx-target_star*exp(-mx))
    if(any(is.na(dif_expmx))) {      #restart due to overflow (does not count as restart - shouldnt happen)
      lambda_n=.renormKLqp(rnorm(ncol(A)), x_star,lambda_nnorm_direction)    
    } else if(log(max(abs(dif_expmx)))< log(tol) -mx) { # We are done - yeah!
      break 
    } else { #Find next step direction and perform linesearch to avoid overstepping
      v0= - .solveTrap(F_deriv_expmx, dif_expmx)
      lambda_n=.line_searchKLqp(x_star,target_star,lambda_nnorm_direction, lambda_n, v0)
    }
    
    
    if(restart<maxrestart & (steps %% 100)==0 ) {
      probs=sort(pi_n_expmx, decreasing = TRUE)
      probsCumsum=cumsum(probs)
      normconst=probsCumsum[length(probsCumsum)]
      cuti=max(which(probsCumsum<.9*normconst),1)

      if(cuti<=max(2, round(.1*nrow(A)))) { #90% of probability belongs to 10% of samples
        cutoff = probs[cuti]
        flg = pi_n_expmx < cutoff
        possible_target=2*b-colMeans(A[!flg,,drop=FALSE])
        lambda_n=tryCatch(.newtonKLqp(A[flg,,drop=FALSE], possible_target, maxit=maxit, tol=tol, maxrestart=0)$lambda_n, 
                          error = function(e) rnorm(ncol(A))    
        )
        lambda_n=.renormKLqp(lambda_n, x_star, lambda_nnorm_direction)   
        restart=restart+1
      } 
    }
  }
  return(list(steps=steps, lambda_n=lambda_n, weights=pi_n_expmx/sum(pi_n_expmx)))
}



.print_ret = function(originalDataset,weights, dataset, target, originalTarget, 
                      warningcut, silent,
                      Nindependent, augmentWeights) {
  weights=ifelse(weights>0,weights,0) #Just in case of numeric issues
  weights=weights/sum(weights)
  achievedMean=as.vector(t(dataset) %*% weights)
  names(achievedMean)=colnames(dataset)


  if(!silent){
    if(is.null(originalTarget)) {
      toprint= t(cbind(achievedMean, target))
      rownames(toprint) =c("Achieved Mean", "Target Mean")
      colnames(toprint)=colnames(dataset)
    } else {
      toprint= t(cbind(achievedMean, target, originalTarget))
      rownames(toprint) =c("Achieved Mean", "Adjusted Target Mean", "Original Target Mean")
      colnames(toprint)=colnames(dataset)
    }
    cat("----------------------------------------------------------------\n")
    cat("Optimization was successful. The weights have a sampling\ndistribution with means close to the attempted target:\n")
    print(toprint)
    cat("Maximum weight was: ", max(weights),"\n")
    if( Nindependent >0 )
      cat("Data augmented with", Nindependent, "sample(s) with independent variables.",
          "\nThe final weight of the indpendent sample(s) was: ", 
          sum(weights[(length(weights)-Nindependent+1):length(weights)]), "\n")
    
    cat("----------------------------------------------------------------\n")
  }
  if(any(weights>warningcut))
    warning(paste0("Some of the weights are larger than ", warningcut, 
                   ". Thus your bootstrap sample may be overly dependent on a few samples. See vignette.\n"))

  
  ret = list(weights=weights,
             target=target,
             dataset=originalDataset,
             X=dataset[1:nrow(originalDataset),],
             originalTarget=originalTarget,
             achievedMean=achievedMean,
             Nindependent = Nindependent,
             augmentWeights = augmentWeights) 
  class(ret)="tweights"
  return(ret)
}

.normalize=function(X,lambda) {
  pi=1 / (X %*% lambda)
  const=sum(pi)
  return(lambda*const)
}

