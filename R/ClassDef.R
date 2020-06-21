# set up database s4 class



#' @name ldb
#' @title local data base
#' @description Creates an s4 object of ldb (local data base).
#' Object only holds information regarding the data that is stored.
#' @slot name scalar Character of data name
#' @slot col_names Character vector of column names
#' @slot col_types Character vector indicating expected data types
#' of each column
#' @slot key Character vector of column names that represent key
#' values. It is good practice that the length of the interaction
#' of key values equals the number of rows of the data.
#' @slot path Scalar Character of the physical path to the data
#' @slot dependency a named list. Names should correspond to another 
#' ldb object. Each entry is a named vector between keys indicating how
#' these databases are related.
#' @export Ldb
Ldb <- setClass("ldb", representation(name = "character",
                              col_names = "character",
                              col_types = "character",
                              key = "character",
                              path = "character",
                              dependency = "list"),
         prototype(name = "ldb_prototype",
                   col_names = NA_character_,
                   col_types = NA_character_,
                   key = NA_character_,
                   path = NA_character_,
                   dependency = list()),
         validity = function(object){
           errors <- character()
           if(is.na(object@name)){
             msg <- "name should not be NA"
             errors <- c(errors, msg)
           }
           if(!is.na(object@col_names)&&
              !is.na(object@col_types)&&
              (length(object@col_names)!=length(object@col_types))){
             errors <- c(errors, "col_names and col_types must be the same length")
           }
           
           if(!is.na(object@col_names)&&!is.na(object@key)&&!all(object@key%in%object@col_names)){
             errors <- c(errors, "All key(s) must be within col_names")
           }
           
           if(!is.na(object@path)&&!dir.exists(object@path)){
             errors <- c(errors, paste0(object@name," directory does not exist: ", object@path))
           }
           
           if (length(errors) == 0) TRUE else errors
        
         })
setMethod("initialize", "ldb",
          function(.Object, ...){
            .Object <- callNextMethod(.Object, ...)
            if(.Object@name=="ldb_prototype"){
              .Object@name <- gsub("prototype", gen_random(), .Object@name)
            }
            if(!is.na(.Object@col_names)&&is.na(.Object@col_types)){
              .Object@col_types <- rep("?",length(.Object@col_names))
            }
            validObject(.Object)
            .Object
          })
setMethod("show","ldb",
          function(object){
            cat("Column Summary:\n")
            print(colsummary(object))
            cat("\n")
            if(is.na(object@path)){
              cat(paste0("'",object@name,"' path not set\n"))
            } else {
              cat(paste0("'",object@name,"' path set to:\n", object@path,"\n"))
            }
            cat("\n")
            if(length(object@dependency)==0){
              cat(paste0("'",object@name,"' has no dependencies\n"))
            } else {
              cat(paste0("'",object@name,"' has the following dependencies:\n"))
              nms <- names(object@dependency)
              .df <- c()
              for(i in 1:length(object@dependency)){
                df <- data.frame(x = names(object@dependency[[i]]), y = unname(object@dependency[[i]]))
                colnames(df) <- c(object@name,nms[i])
                .df <- bind_rows(.df, df)
              }
              print(.df)
            }
          })


#' @name ldbm
#' @title Local Data base Manager
#' @description Creates an s4 object of ldbm (local data base manager).
#' Object only holds information regarding the data bases it is watching.
#' @slot name scalar Character of manager name
#' @slot ldb List of ldb - exact copies of ldb objects.
#' @slot managing numeric summarising number of databases.
#' @slot path Scalar Character of the physical path to the data
#' @slot colsummary dataframe summarising the columns of each database
#' @slot keysummary dataframe summarising the keys of each database 
#' @importFrom stringr str_detect
#' @export Ldbm
Ldbm <- setClass("ldbm", representation(name = "character",
                               ldb = "list",
                               managing = "numeric",
                               path = "character",
                               colsummary = "data.frame",
                               dependency_tree = "data.frame"),
         prototype(name = "ldbmanager_prototype",
                   ldb = list(),
                   managing = 0,
                   path = NA_character_,
                   colsummary = data.frame(ldb.names = character(),
                                           col_names = character(),
                                           col_types = character(),
                                           key = logical()),
                   dependency_tree = data.frame(key = character())))
setMethod("initialize", "ldbm",
          function(.Object, ...){
            .Object <- callNextMethod(.Object, ...)
            if(.Object@name=="ldbmanager_prototype"){
              .Object@name <- gsub("prototype", gen_random(), .Object@name)
            }
            if(!is.null(.Object@ldb)&&all(unlist(lapply(.Object@ldb, is)))){
              .Object@managing <- length(.Object@ldb)
              .Object@colsummary <- colsummary(.Object)
            }
            if(is.na(.Object@path)){
              .Object@path <- get_ldb_path()
            }else if(!dir.exists(.Object@path)){
              dir.create(.Object@path)
            }
            if(str_detect(.Object@path,"/$", negate = T)){
              .Object@path <- paste0(.Object@path,"/")
            }
            .Object
          })
setMethod("show", "ldbm",
          function(object){
            cat(paste0(object@name," is managing ", object@managing, " data tables\n"))
            cat("Column Summary:\n")
            print(colsummary(object))
            cat("\n")
            if(is.na(object@path)){
              cat(paste0("'",object@name,"' path not set\n"))
            } else {
              cat(paste0("'",object@name,"' path set to:\n", object@path,"\n"))
            }
            cat("\n")
            cat("Showing dependency tree\n")
            print(object@dependency_tree)
          })
#' @name updateClass
#' @title Update Class
#' @description function used to check and update values
setGeneric("updateClass", function(object) standardGeneric("updateClass"))
#' @describeIn updateClass Function to update ldbm. Checks number of databases it is watching,
#' recalls colsummary, and updates dependencies.
setMethod("updateClass",
          signature(object = "ldbm"),
          function(object){
            #browser()
            if(!is.null(object@ldb)){
              object@managing <- length(object@ldb)
              object@colsummary <- colsummary(object)
              #object@dependency_tree <- genDependencyTree(object)
            } else {
              object@managing <- 0
              object@colsummary <- data.frame(ldb.names = character(),
                                              col_names = character(),
                                              col_types = character())
              object@dependency_tree <- data.frame(key = character())
            }
            
            return(object)
          })







