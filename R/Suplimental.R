
#' @name gen_random
#' @title Generate Random string
#' @description Function used to generate random strings
#' for databases when no name is provided.
gen_random <- function(n = 6){
  paste(sample(c(letters,0:9), size = n, T,prob = c(rep(.5/26,26),rep(.05,10))), collapse = "")
}

#' @name invertnames
#' @title Invert names
#' @description Inverts the values and names of a named vector.
#' @examples 
#' \dontrun{
#' vec <- c("A"="a","B"="b")
#' invertnames(vec)
#' }
invertnames <- function(x){
  if(is.null(names(x))&&is.character(x)){
    return(x)
  } else {
    stop("x should be a character vector")
  }
  y <- names(x)
  names(y) <- x
  return(y)
}



#' @name update_left
#' @title Update Left
#' @description Special function intended only to be
#' used on two different version of the same data frame. 
#' It is expected that y is either the same size as x or
#' smaller. Function will still work if y is larger than x
#' however, this function Only pulls in rows of y whose
#' keys match the keys of x.
#' @param x left table to be updated
#' @param y right table with updated values
#' @param by imitates dplyr's expectations with joining
#' @param x.col.only Logical if output table should be restricted to
#' x columns only. Default set to TRUE.
#' @examples 
#' x <- data.frame(key1 = 1:4, key2 = letters[1:4], value1 = rnorm(4), value2 = runif(4))
#' y <- x[c(2,4),]
#' y$value1 <- -10
#' update_left(x,y, by = c("key1"="key1","key2"="key2")) 
#' @importFrom tidyselect all_of
#' @export
update_left <- function(x,y,by=NULL, x.col.only = T){
  if(x.col.only){
    y <- select(y, all_of(colnames(x))) #if x.col.only is TRUE limit y to only cols of x
  }
  order.colName <- paste0("idx_",gen_random()) #make an index to order things
  x[[order.colName]] <- 1:nrow(x)
  suppressWarnings({
    y <- left_join(x = y, y = select(x, all_of(c(by,order.colName))), invertnames(by)) #pull in index to y
    .nm <- names(by)
    by <- c(by, order.colName) #update common joining factors
    if(!is.null(.nm)){
      names(by) <- c(.nm, order.colName) #incase named vector is needed
    }
    x_semi <- semi_join(x,y, by)
    x_anti <- anti_join(x,y, by)
    y_semi <- semi_join(x = y, y = x_semi, by = invertnames(by))
  })
  
  if(nrow(y_semi)==nrow(x_semi)){
    suppressWarnings({
      x <- bind_rows(x_anti, y_semi) 
      x <- x %>%
        select(all_of(order.colName)) %>%
        arrange_all %>%
        left_join(x = ., y = x, by = order.colName) %>%
        select(-all_of(order.colName))
    })
  } else {
    stop("Cannot distinctly match by specified key(s)")
  }
  return(x)
}

# #' @name all_within
# #' @title All Within
# #' @description wrapper to test all unique values of x and y are the same
# all_within <- function(x,y){
#   x <- unique(x)
#   y <- unique(y)
#   res <- all(x%in%y)&&all(y%in%x)
#   return(res)
# }

# df_match <- function(x, y) {
#   if(!all_within(colnames(x),colnames(y))){
#     stop("x and y must have exact col names")
#   }
#   y <- y[,colnames(x)] #reorders columns
#   matches <- mapply(function(x,y){x%in%y}, x = x, y = y)
#   matches <- apply(matches,1,all)
#   matches
# }

# update_left <- function(x, y, by = NULL){
#   #browser()
#   if(!all_within(colnames(x),colnames(y))){
#     stop("x and y must have the same colnames")
#   }
#   y <- y[,colnames(x)]
#   if(is.null(names(by))){
#     if(!all(by %in% colnames(x))||!all(by%in%colnames(y))){
#       stop("by not represented in both x and y")
#     }
#     key <- by
#   } else {
#     #colnames(y)[colnames(y)%in%by] <- names(by)
#     key <- names(by)
#   }
#   .left <- x %>% select(key)
#   .right <- y %>% select(key)
#   
#   matched <- df_match(x = .left, y = .right)
#   if(sum(matched)!=nrow(y)){
#     stop("Cannot match by specified keys distinctly.")
#   }
#   index <- apply(.left[matched,],
#                  1,
#                  function(x,y){which(apply(y,
#                                            1,
#                                            function(y,x){all(y%in%x)},
#                                            x = x))},
#                  y = .right)
#   x[matched,] <- y[index,]
#   return(x)
# }
