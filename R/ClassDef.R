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
         validity = function(ldb){
           errors <- character()
           if(is.na(ldb@name)){
             msg <- "name should not be NA"
             errors <- c(errors, msg)
           }
           if(!is.na(ldb@col_names)&&
              !is.na(ldb@col_types)&&
              (length(ldb@col_names)!=length(ldb@col_types))){
             errors <- c(errors, "col_names and col_types must be the same length")
           }
           
           if(!is.na(ldb@col_names)&&!is.na(ldb@key)&&!all(ldb@key%in%ldb@col_names)){
             errors <- c(errors, "All key(s) must be within col_names")
           }
           
           if(!is.na(ldb@path)&&!dir.exists(ldb@path)){
             errors <- c(errors, paste0(ldb@name," directory does not exist: ", ldb@path))
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
#' @export Ldbm
Ldbm <- setClass("ldbm", representation(name = "character",
                               ldb = "list",
                               managing = "numeric",
                               path = "character",
                               colsummary = "data.frame",
                               keysummary = "data.frame"),
         prototype(name = "ldbmanager_prototype",
                   ldb = list(),
                   managing = 0,
                   path = NA_character_,
                   colsummary = data.frame(ldb.names = character(),
                                           col_names = character(),
                                           col_types = character(),
                                           key = logical()),
                   keysummary = data.frame(key = character())))
setMethod("initialize", "ldbm",
          function(.Object, ...){
            .Object <- callNextMethod(.Object, ...)
            if(.Object@name=="ldbmanager_prototype"){
              .Object@name <- gsub("prototype", gen_random(), .Object@name)
            }
            if(!is.null(.Object@ldb)&&all(unlist(lapply(.Object@ldb, is)))){
              .Object@managing <- length(.Object@ldb)
              .Object@colsummary <- ldb_colsummary(.Object)
            }
            if(is.na(.Object@path)){
              .Object@path <- get_ldb_path()
            }
            .Object
          })

setGeneric("updateClass", function(object) standardGeneric("updateClass"))
#' @name updateClass
#' @description Used to update slot information in ldbm
setMethod("updateClass",
          signature(object = "ldbm"),
          function(object){
            if(!is.null(object@ldb)){
              object@managing <- length(object@ldb)
              object@colsummary <- ldb_colsummary(object)
            } else {
              object@managing <- 0
              object@colsummary <- data.frame(ldb.names = character(),
                                              col_names = character(),
                                              col_types = character())
            }
            return(object)
          })







