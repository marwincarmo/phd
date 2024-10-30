## nortara functions from github.com/superdesolator/NORTARA/blob/master/R ##
## copied here to fix bugs resulting from the updated class() function in R 4.x.x

Perform_chol <- function(r_matrix, w_k_bar,
                         invcdfnames, paramslists) {
  ndim <- ncol(r_matrix)
  mk <- nrow(w_k_bar)
  tmp_paramslists <- list()
  #For better Cholesky decomposition, and  the while statement can stop
  #when ncol( r_adjust_matrix ) equals 2, that is , b equals (ndim-2)
  #replace lower.tri and upper.tri elements of r_matrix which equal 1 by 0.999
  r_matrix[lower.tri(r_matrix)] <- 0
  upper_elements <- r_matrix[upper.tri(r_matrix)]
  upper_elements[upper_elements==1] <- 0.999
  upper_elements[upper_elements==-1] <- -0.999
  r_matrix[upper.tri(r_matrix)] <- upper_elements
  r_matrix <- r_matrix + t(r_matrix)
  diag(r_matrix) <- 1
  r_adjust_matrix <- r_matrix
  b<- 0
  while (1) {
    chol_decompose_val <- try(chol(r_adjust_matrix), silent = TRUE)
    #Test whether the Cholesky decomposition fail
    if(inherits(chol_decompose_val, "try-error")) {
      b <- b + 1
      r_adjust_matrix <- r_matrix[1:(ndim - b),1:(ndim - b)]
    } else break
  }
  # compute simultaneously for (1:(ndim-b)) , rest shoud be computed separately
  mk_correlated_standard_norm <- w_k_bar[ ,1:(ndim - b)] %*% chol_decompose_val
  y_estimator <- matrix(rep(0,ndim * ndim ), nrow = ndim)
  diag(y_estimator) <- 1
  if (mk < 60)
    y_estimator[1:(ndim - b),1:(ndim - b)] <- y_crude_estimator(mk_correlated_standard_norm,
                                                                invcdfnames[1:(ndim - b)],
                                                                paramslists[1:(ndim - b)])
  else
    y_estimator[1:(ndim - b),1:(ndim - b)] <- y_control_estimator(mk_correlated_standard_norm,
                                                                  r_matrix[1:(ndim - b),1:(ndim - b)],
                                                                  invcdfnames[1:(ndim - b)],
                                                                  paramslists[1:(ndim - b)])
  if (b != 0) {
    for (i in ndim:(ndim - b + 1) )
      for (j in 1:(i - 1)) {
        #The Cholesky decomposition will always succeed here for 1->0.99 transformation
        r_mat<- matrix(c(1,r_matrix[j,i],r_matrix[j,i],1), nrow = 2)
        mk_correlated_standard_norm<- w_k_bar[ ,c(j,i)] %*% chol(r_mat)
        if (mk < 60)
          tmp <- y_crude_estimator(mk_correlated_standard_norm,
                                   invcdfnames[c(i,j)], paramslists[c(i,j)])[1,2]
        else
          tmp <- y_control_estimator(mk_correlated_standard_norm, r_mat,
                                     invcdfnames[c(i,j)], paramslists[c(i,j)])[1,2]
        y_estimator[j,i] <- y_estimator[i,j] <- tmp
      }
  }
  y_estimator
}

BoundingRA <- function(cor_matrix, invcdfnames,
                       paramslists,  m1 = 60,
                       c1 = 2, c2 = 1, delta1 = 0.0001,
                       sigma0 = 0.01, epsilon = 1e+50, maxit = 1000){
  if( length(invcdfnames) != length(paramslists)) {
    stop("inversecdfs should have the same length paramslists!")
  }
  ndim <- ncol(cor_matrix)
  m_size_history <- NULL
  r_solution_mat_history <- list()
  r_estimator_mat_history <- list()
  var_r_estimator <- matrix(rep(0, ndim * ndim), ndim)
  # initialize to make the while  statement can continue for the first time
  var_r_estimator[1,ndim] <- sigma0 + 0.1
  mk <- m1
  epsilonk <- epsilon
  k <- 0
  while (k < 4 || any(var_r_estimator > sigma0)) {
    k <- k + 1
    # Here choose w_bar the third choice for better effciency while the reference paper
    # choose the first choice for saving computer's storage capacity
    w_k_bar_initial <- matrix(rnorm(mk*ndim, mean = 0, sd = 1), nrow = mk)
    mk_initial <- mk
    add_sample_size_k <-NULL
    fail_search <- TRUE
    while (fail_search){
      if ( k == 1) r_matrix <- cor_matrix
      else   r_matrix <- r_estimator_mat_history[[k-1]]
      r_k_bar_matrix <- matrix(rep(0, ndim * ndim ), ndim)
      r_solution_matrix <- matrix(rep(0, ndim * ndim ), ndim)
      msave <- mk
      # start with new mk in k iteration  with the same initial random number seeds
      w_k_bar <- rbind( w_k_bar_initial, add_sample_size_k )
      y_estimator <- Perform_chol(r_matrix, w_k_bar, invcdfnames,
                                  paramslists)
      check_result <- check_y_estimator(y_estimator, r_matrix, msave, mk,
                                        invcdfnames, paramslists)
      y_estimator <- check_result$y_estimator
      mk <- check_result$mk
      l <- r_matrix
      u <- r_matrix
      y_bar_l <- cor_matrix + 1
      y_bar_u <- cor_matrix - 1
      for (i in 1:(ndim - 1)) {
        Breakouter <- FALSE
        for (j in (i + 1):ndim) {
          p <- 1
          delta_k <- delta_k_i_j(i, j, delta1, k, mk, c1,
                                 c2, m_size_history,
                                 r_solution_mat_history,
                                 r_estimator_mat_history)
          fail_search <- FALSE
          while (p < maxit){
            if (y_estimator[i,j] < cor_matrix[i,j]) {
              if (-1 <= r_matrix[i,j] && r_matrix[i,j] < 1) {
                l[i,j] <- r_matrix[i,j]
                y_bar_l[i,j] <- y_estimator[i,j]
              } else {
                r_solution_matrix[i,j] <- 1
                r_k_bar_matrix[i,j] <- r_k_bar_matrix_i_j(i,j,k,r_solution_matrix[i,j],mk,m_size_history, r_solution_mat_history)
              }
            } else {
              if (-1 < r_matrix[i,j] && r_matrix[i,j] <= 1) {
                u[i,j] <- r_matrix[i,j]
                y_bar_u[i,j] <- y_estimator[i,j]
                
              } else {
                r_solution_matrix[i,j] <- -1
                r_k_bar_matrix[i,j] <- r_k_bar_matrix_i_j(i,j,k,r_solution_matrix[i,j],mk,m_size_history, r_solution_mat_history)
              }
            }
            if ((y_bar_l[i,j] - cor_matrix[i,j]) * (y_bar_u[i,j] - cor_matrix[i,j]) > 0)
            {
              if (y_estimator[i,j] < cor_matrix[i,j]) {
                
                r_matrix[i,j] <- min(r_matrix[i,j] + delta_k,  1)
                
              } else {
                
                r_matrix[i,j] <- max(r_matrix[i,j] - delta_k, -1)
                
              }
              delta_k <- 2 * delta_k
              r_mat <- matrix(c(1,r_matrix[i,j], r_matrix[i,j],1),2)
              y_estimator[i,j] <- Perform_chol(r_mat, w_k_bar[ ,c(i,j)],
                                               invcdfnames[c(i,j)],
                                               paramslists[c(i,j)] )[1,2]
              
            } else
            {
              while (p < maxit) {
                r_regula_falsi <- l[i,j] + (u[i,j] - l[i,j]) * (cor_matrix[i,j] - y_bar_l[i,j]) /
                  (y_bar_u[i,j] - y_bar_l[i,j])
                
                if (u[i,j] - l[i,j] < epsilonk)
                {
                  r_solution_matrix[i,j] <- r_regula_falsi
                  r_k_bar_matrix[i,j] <- r_k_bar_matrix_i_j(i,j,k,r_solution_matrix[i,j],mk,m_size_history, r_solution_mat_history)
                  break
                } else {
                  r_mat <- matrix(c(1,r_regula_falsi,r_regula_falsi,1),2)
                  y_estimator[i,j] <- Perform_chol(r_mat, w_k_bar[ ,c(i,j)],
                                                   invcdfnames[c(i,j)],
                                                   paramslists[c(i,j)])[1,2]
                  if (y_estimator[i,j] < cor_matrix[i,j]) {
                    if (-1 <= r_regula_falsi && r_regula_falsi < 1) {
                      l[i,j] <- r_regula_falsi
                      y_bar_l[i,j] <- y_estimator[i,j]
                      
                    } else {
                      r_solution_matrix[i,j] <- 1
                      r_k_bar_matrix[i,j] <- r_k_bar_matrix_i_j(i,j,k,r_solution_matrix[i,j],mk,m_size_history, r_solution_mat_history)
                    }
                  } else {
                    if (-1 < r_regula_falsi && r_regula_falsi <= 1) {
                      u[i,j] <- r_regula_falsi
                      y_bar_u[i,j] <- y_estimator[i,j]
                      
                    } else {
                      r_solution_matrix[i,j] <- -1
                      r_k_bar_matrix[i,j] <- r_k_bar_matrix_i_j(i,j,k,r_solution_matrix[i,j],mk,m_size_history, r_solution_mat_history)
                    }
                  }
                  
                }
                p <- p + 1
              }
              break
            }
            p <- p + 1
          }
          if (p >= maxit) {
            fail_search <- TRUE
            Breakouter <-TRUE
            break
          }
        }
        if(Breakouter) break
      }
      if (fail_search) {
        mk <- c1 * mk
        add_sample_size_k <-  matrix(rnorm((mk - mk_initial) * ndim, mean = 0, sd = 1), nrow = mk - mk_initial)
      }
    }
    m_size_history <-  c(m_size_history, mk)
    r_solution_mat_history[[k]] <- r_solution_matrix
    r_estimator_mat_history[[k]] <- r_k_bar_matrix
    for ( i in 1:(ndim - 1))
      for (j in (i + 1):ndim) {
        tmp <- v_i_j_estimator(i, j, m_size_history,k, r_solution_mat_history,
                               r_estimator_mat_history)
        var_r_estimator[i,j] <- tmp / sum(m_size_history[1:k])
      }
    epsilonk <- epsilonk / sqrt(c1)
    mk <- c1 * mk
  }
  rou_estimator <- r_estimator_mat_history[[k]]
  rou_estimator <- rou_estimator+t(rou_estimator)
  diag(rou_estimator) <- 1
  if (!corpcor::is.positive.definite(rou_estimator)) {
    warning( "The estimator of the target Normal correlation matrix is not positive definite. It was replaced by  Nearest positive definite matrix by using function Matrix::nearPD !")
    rou_estimator <- as.matrix(Matrix::nearPD(rou_estimator, corr = TRUE, keepDiag = TRUE)$mat)
  }
  rou_estimator
}

check_input_cormat <- function(invcdfnames, paramslists, cor_matrix){
  
  if (ncol(cor_matrix) != length(invcdfnames)) {
    stop("The dims of input cor_matrix does not match the length of invcdfnames!")
  }
  if (isSymmetric(cor_matrix) == FALSE) {
    stop("Input cor_matrix should be symmetric!")
  }
  ndim <- ncol(cor_matrix)
  upbound_cormat <- valid_input_cormat(invcdfnames, paramslists)$max_valid_cormat
  lowbound_cormat <- valid_input_cormat(invcdfnames, paramslists)$min_valid_cormat
  outofboundcounts <- 0
  for (i in 1:(ndim - 1))
    for (j in (i + 1):ndim){
      if ( cor_matrix[i,j] < lowbound_cormat[i,j] || cor_matrix[i,j] > upbound_cormat[i,j]) {
        outofboundcounts <- outofboundcounts + 1
        cat("The input cor_matrix[",i,",",j,"] value should be greater than ",
            lowbound_cormat[i,j]," and less than ",upbound_cormat[i,j],"!\n"
        )
      }
    }
  if (outofboundcounts > 0) {
    stop ("Please correct the above values and try again!")
  }
  TRUE
}

check_y_estimator<- function (y_estimator, r_matrix, msave, mk,
                              invcdfnames, paramslists) {
  r_matrix[lower.tri(r_matrix)] <- 0
  upper_elements <- r_matrix[upper.tri(r_matrix)]
  upper_elements[upper_elements==1] <- 0.999
  upper_elements[upper_elements==-1] <- -0.999
  r_matrix[upper.tri(r_matrix)] <- upper_elements
  ndim <- ncol(r_matrix)
  y_estimator[lower.tri(y_estimator)] <- 0
  diag(y_estimator) <- 0
  indexmat <- which(abs(y_estimator) > abs(r_matrix), arr.ind = TRUE)
  if (length(indexmat) == 0) {
    y_estimator <- y_estimator + t(y_estimator)
    diag(y_estimator) <- 1
    return(list(y_estimator = y_estimator, mk = mk))
  }
  t <- 1
  while (any(abs(y_estimator) > abs(r_matrix))) {
    t <- t + 1
    w_bar <- matrix(rnorm(msave*ndim, mean = 0, sd = 1), nrow = msave)
    y_msave_estimator <- Perform_chol(r_matrix , w_bar, invcdfnames, paramslists)
    y_new_estimator <- ((t - 1) / t) * y_estimator + 1 / t * y_msave_estimator
    y_estimator <- y_new_estimator
    y_estimator[lower.tri(y_estimator)] <- 0
    diag(y_estimator) <- 0
    mk <- mk + msave
  }
  y_estimator <-  y_estimator + t( y_estimator)
  diag( y_estimator) <- 1
  return(list(y_estimator = y_estimator, mk = mk))
}

gennortaRA <- function(n, cor_matrix, invcdfnames,
                       paramslists = NULL, defaultindex = NULL,
                       m1 = 60, c1 = 2, c2 = 1, delta1 = 0.0001,
                       sigma0 = 0.01, epsilon = 1e+50, maxit = 1000) {
  if (length(invcdfnames) != length(paramslists)+length(defaultindex)) {
    stop("inversecdfs should have the same length paramslists,check paramslists and defaultindex !")
  }
  if (all(invcdfnames == "qnorm")) {
    stop("You should not use gennortaRA to generate multi-normal varibles!\n
          Please choose anthor package to do so if you still want!")
  }
  #----------------------------------------------------------
  # Create inversecdfs's paramslists from raw input
  tmp_paramslists <- list()
  cdfscount <- length(invcdfnames)
  length(tmp_paramslists) <- cdfscount   # create list of NULLs
  if (length(defaultindex) != 0) {
    count <- 0
    for (i in 1:(cdfscount)) {
      if (!i %in% defaultindex) {
        count <- count + 1
        tmp_paramslists[[i]] <- paramslists[[count]]
      }
    }
    paramslists <- tmp_paramslists
    rm(tmp_paramslists)
  }
  #------------------------------------------------------------
  #Compute the intermediate normal correlation matrix
  if (check_input_cormat(invcdfnames, paramslists, cor_matrix)) {
    Sigma <- BoundingRA(cor_matrix, invcdfnames,
                        paramslists,  m1,
                        c1, c2, delta1,
                        sigma0, epsilon, maxit)
    #Generate virables with NoRTA by using Sigma
    ndim <- ncol(cor_matrix)
    transform_mat <- NULL
    n_correlated_standard_normal <- matrix(rnorm(n * ndim), nrow = n) %*% chol(Sigma)
    
    for (i in 1:ndim) {
      
      funcall <- as.call(c(as.name(invcdfnames[i]),
                           list(pnorm(n_correlated_standard_normal)[ ,i]), paramslists[[i]]))
      transform_mat <- cbind(transform_mat, eval(funcall))
    }
    return(transform_mat)
  }
}

v_i_j_estimator <- function(i, j, m_size_history, k,
                            r_solution_mat_history,
                            r_estimator_mat_history){
  if ((!(inherits(r_solution_mat_history, "list"))) || (!(inherits(r_estimator_mat_history, "list")))) {
    stop("r_solution_mat_history and r_estimator_mat_history must be list !")
  }
  if (length(r_solution_mat_history) < k || length(r_estimator_mat_history) < k
      || length(m_size_history) < k) {
    stop( paste0("r_solution_mat_history and r_estimator_mat_history
                 and m_size_history must be all no less than ",k,"!"))
  }
  if (k == 1) return(0)
  r_solu_i_j_vec <- NULL
  for (t in 1:k) {
    r_solu_i_j_vec <- c(r_solu_i_j_vec, r_solution_mat_history[[t]][i,j])
  }
  a <- sum(m_size_history[1:k] *  r_solu_i_j_vec^2)
  b <- sum(m_size_history[1:k]) *  r_estimator_mat_history[[k]][i,j]^2
  res <- ((a - b) / (k - 1))^(1 / 2)
  res
}


# Described in Appendeix:NORTA RVG Methods step 6.
delta_k_i_j <- function(i, j, delta1, k, mk, c1,
                        c2, m_size_history,
                        r_solution_mat_history,
                        r_estimator_mat_history) {
  
  if (k == 1 || k == 2) return(delta1)
  a <- v_i_j_estimator(i, j, m_size_history,
                       k-1, r_solution_mat_history, r_estimator_mat_history)
  b <- 1 / sum(m_size_history[1:(k - 1)])
  d <- 1 / (c1 * mk)
  res <- c2 * a * ( b + d )^(1/2)
  res
}

# Described in Appendeix:NORTA RVG Methods step 4.
r_k_bar_matrix_i_j <- function(i, j, k, r_k_solution_i_j, mk, m_size_history, r_solution_mat_history){
  if (!(inherits(r_solution_mat_history, "list"))) {
    stop("r_solution_mat_history  must be list ! ")
  }
  if (length(r_solution_mat_history) < (k - 1) || length(m_size_history) < (k - 1)) {    stop( paste0("r_solution_mat_history and  m_size_history must be all no less than ",k - 1,"!"))
  }
  if (k == 1) return(r_k_solution_i_j)
  r_solu_i_j_vec <- NULL
  for( t in 1:(k - 1)) {
    r_solu_i_j_vec <- c(r_solu_i_j_vec, r_solution_mat_history[[t]][i,j])
  }
  a <-sum(  m_size_history[1:(k - 1)] *  r_solu_i_j_vec ) + mk * r_k_solution_i_j
  b <-sum(  m_size_history[1:(k - 1)] ) + mk
  res <- a / b
  res
}

valid_input_cormat <- function(invcdfnames, paramslists){
  
  ndim <- length(invcdfnames)
  samples <- 100000
  normal_mat <- matrix(rnorm(samples * ndim), samples)
  max_valid_cormat <- min_valid_cormat <- diag(1/2,ndim,ndim)
  transform_mat <- NULL
  for (i in 1:ndim) {
    
    funcall <- as.call(c(as.name(invcdfnames[i]),
                         list(pnorm(normal_mat[ ,i])), paramslists[[i]]))
    transform_mat <- cbind(transform_mat, eval(funcall))
  }
  
  for (i in 1:(ndim - 1))
    for (j in (i + 1):ndim){
      X <- transform_mat[ ,i]
      Y <- transform_mat[ ,j]
      if (length(which(!duplicated(X)[-1]))==0 || length(which(!duplicated(Y)[-1]))==0) {
        max_valid_cormat[i,j] <- 0
        min_valid_cormat[i,j] <- 0
      } else {
        max_valid_cormat[i,j] <- cor(X[order(X)],Y[order(Y)])
        min_valid_cormat[i,j] <- cor(X[order(X,decreasing=TRUE)],Y[order(Y)])
      }
    }
  max_valid_cormat <- max_valid_cormat + t(max_valid_cormat)
  min_valid_cormat <- min_valid_cormat + t(min_valid_cormat)
  res <- list(max_valid_cormat =  max_valid_cormat,
              min_valid_cormat =  min_valid_cormat)
  res
}


y_control_estimator <- function (mk_correlated_standard_norm, r_mat,
                                 invcdfnames, paramslists) {
  ndim <- ncol(mk_correlated_standard_norm)
  mk <- nrow(mk_correlated_standard_norm )
  if (!(inherits(mk_correlated_standard_norm, "matrix"))) {
    stop("mk_correlated_standard_norm must be matrix !")
  }
  if (length(invcdfnames) != length(paramslists)) {
    stop("inversecdfs should have the same length paramslists !")
  }
  ndim <- ncol(mk_correlated_standard_norm)
  if (mk < 60) {
    stop("the number of observations must be not less than 60!")
  }
  transform_mat <- NULL
  for (i in 1:ndim) {
    funcall <- as.call(c(as.name(invcdfnames[i]),
                         list(pnorm(mk_correlated_standard_norm)[ ,i]), paramslists[[i]]))
    transform_mat <- cbind(transform_mat, eval(funcall))
  }
  res <- matrix(rep(0, ndim * ndim), nrow = ndim)
  diag(res) <- 1
  for (i in 1:(ndim-1))
    for (j in (i+1):ndim) {
      if (length(which(!duplicated(transform_mat[ ,i])[-1]))==0 ||
          length(which(!duplicated(transform_mat[ ,j])[-1]))==0)
        tmp_crude <- 0
      else
        tmp_crude <- cor(transform_mat[ ,i], transform_mat[ ,j])
      theta <- cor(mk_correlated_standard_norm[ ,i],
                   mk_correlated_standard_norm[ ,j])
      r <- r_mat[i,j]
      # micro/macro replications, 20 macro replications
      micro_size <- floor(mk / 20)
      lowbound <- 0:19*micro_size+1
      upbound <- 1:20*micro_size
      tmp <- Map(function(x, y) {
        theta_micro <- cor(mk_correlated_standard_norm[ ,i][x:y],
                           mk_correlated_standard_norm[ ,j][x:y])
        if(length(which(!duplicated(transform_mat[ ,i][x:y])[-1]))==0 ||
           length(which(!duplicated(transform_mat[ ,j][x:y])[-1]))==0  )
          y_micro_crude_estimator <- 0
        else
          y_micro_crude_estimator <- cor(transform_mat[ ,i][x:y],
                                         transform_mat[ ,j][x:y])
        return(list(theta_micro =  theta_micro,
                    y_micro_crude_estimator = y_micro_crude_estimator))
        
      }, lowbound, upbound)
      tmp <- unlist(tmp)
      theta_macro <- tmp[names(tmp)=="theta_micro"]
      y_macro_crude_estimator <- tmp[names(tmp)=="y_micro_crude_estimator"]
      cov_estimator <- cov(y_macro_crude_estimator, theta_macro)
      var_estimator <- var(theta_macro)
      beta <- cov_estimator / var_estimator
      res[j,i] <- res[i,j] <- tmp_crude - beta * (theta - r)
    }
  res
}


y_crude_estimator <- function(mk_correlated_standard_norm,
                              invcdfnames, paramslists) {
  if (!(inherits(mk_correlated_standard_norm, "matrix"))) {
    stop("mk_correlated_standard_norm must be matrix !")
  }
  if (length(invcdfnames) != length(paramslists)) {
    stop("inversecdfs should have the same length paramslists !")
  }
  ndim <- ncol(mk_correlated_standard_norm)
  mk <- nrow(mk_correlated_standard_norm)
  if (mk >= 60) {
    stop("the number of observations must be less than 60!")
  }
  transform_mat <- NULL
  for (i in 1:ndim) {
    
    funcall <- as.call(c(as.name(invcdfnames[i]),
                         list(pnorm(mk_correlated_standard_norm)[ ,i]), paramslists[[i]]))
    transform_mat <- cbind(transform_mat, eval(funcall))
  }
  
  res <- matrix(rep(0, ndim * ndim), nrow = ndim)
  diag(res) <- 1
  for (i in 1:(ndim-1))
    for (j in (i+1):ndim)
      if (length(which(!duplicated(transform_mat[ ,i])[-1]))==0 ||
          length(which(!duplicated(transform_mat[ ,j])[-1]))==0)
        res[j,i] <- res[i,j] <- cor(transform_mat[ ,i], transform_mat[ ,j])
  else
    res[j,i] <- res[i,j] <- 0
  res
}

