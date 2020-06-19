
#' @name gen_random
#' @title Generate Random string
#' @description Function used to generate random strings
#' for databases when no name is provided.
gen_random <- function(n = 6){
  paste(sample(c(letters,0:9), size = n, T,prob = c(rep(.5/26,26),rep(.05,10))), collapse = "")
}


#' @name all_within
#' @title All Within
#' @description wrapper to test all unique values of x and y are the same
all_within <- function(x,y){
  x <- unique(x)
  y <- unique(y)
  res <- all(x%in%y)&&all(y%in%x)
  return(res)
}

#' @name invertnames
#' @title Invert names
#' @description Inverts the values and names of a named vector.
#' @example 
#' \dontrun{
#' vec <- c("A"="a","B"="b")
#' invertnames(vec)
#' }
invertnames <- function(x){
  if(is.null(names(x))){
    stop("x must be a named vector")
  }
  y <- names(x)
  names(y) <- x
  return(y)
}

df_match <- function(x, y) {
  if(!all_within(colnames(x),colnames(y))){
    stop("x and y must have exact col names")
  }
  y <- y[,colnames(x)] #reorders columns
  matches <- mapply(function(x,y){x%in%y}, x = x, y = y)
  matches <- apply(matches,1,all)
  matches
}

#' @name update_left
#' @title Update Left
#' @description Special function intended only to be
#' used on two different version of the same data frame. 
#' Intended to be used to efficently update entries into
#' the master ldb if y is a subset. Only pulls in rows of
#' y whose keys match the keys of x.
#' @example 
#' x <- data.frame(key1 = 1:4, key2 = letters[1:4], value1 = rnorm(4), value2 = runif(4))
#' y <- x[c(2,4),]
#' y$value1 <- -10
#' update_left(x,y, by = c("key1"="key1","key2"="key2")) 
#' @export
update_left <- function(x, y, by = NULL){
  #browser()
  if(!all_within(colnames(x),colnames(y))){
    stop("x and y must have the same colnames")
  }
  y <- y[,colnames(x)]
  if(is.null(names(by))){
    if(!all(by %in% colnames(x))||!all(by%in%colnames(y))){
      stop("by not represented in both x and y")
    }
    key <- by
  } else {
    #colnames(y)[colnames(y)%in%by] <- names(by)
    key <- names(by)
  }
  .left <- x %>% select(key)
  .right <- y %>% select(key)
  
  matched <- df_match(x = .left, y = .right)
  if(sum(matched)!=nrow(y)){
    stop("Cannot match by specified keys distinctly.")
  }
  index <- apply(.left[matched,],
                 1,
                 function(x,y){which(apply(y,
                                           1,
                                           function(y,x){all(y%in%x)},
                                           x = x))},
                 y = .right)
  x[matched,] <- y[index,]
  return(x)
}
