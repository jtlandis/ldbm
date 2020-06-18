
gen_random <- function(n = 6){
  paste(sample(c(letters,0:9), size = n, T,prob = c(rep(.5/26,26),rep(.05,10))), collapse = "")
}


all_within <- function(x,y) {
  if(length(unique(x))!=length(unique(y))){
    return(FALSE)
  }
  if(length(intersect(x,y))!=length(unique(x))){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

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
  y <- y[,colnames(x)]
  matches <- mapply(function(x,y){x%in%y}, x = x, y = y)
  matches <- apply(matches,1,all)
  matches
}

update_left <- function(x, y, by = NULL){
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
    colnames(y)[colnames(y)%in%by] <- names(by)
    key <- names(by)
  }
  .left <- x %>% select(key)
  .right <- y %>% select(key)
  
  matched <- df_match(x = .left, y = .right)
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
