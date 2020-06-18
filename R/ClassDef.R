# set up database s4 class



#
setClass("db", representation(name = "character",
                              col_names = "character",
                              col_types = "character",
                              key = "character",
                              path = "character",
                              dependency = "list"),
         prototype(name = "db_prototype",
                   col_names = NA_character_,
                   col_types = NA_character_,
                   key = NA_character_,
                   path = NA_character_,
                   dependency = list()),
         validity = function(db){
           errors <- character()
           if(is.na(db@name)){
             msg <- "name should not be NA"
             errors <- c(errors, msg)
           }
           if(!is.na(db@col_names)&&
              !is.na(db@col_types)&&
              (length(db@col_names)!=length(db@col_types))){
             errors <- c(errors, "col_names and col_types must be the same length")
           }
           
           if(!is.na(db@col_names)&&!is.na(db@key)&&!all(db@key%in%db@col_names)){
             errors <- c(errors, "All key(s) must be within col_names")
           }
           
           if(!is.na(db@path)&&!dir.exists(db@path)){
             errors <- c(errors, paste0(db@name," directory does not exist: ", db@path))
           }
           
           if (length(errors) == 0) TRUE else errors
        
         })

setMethod("initialize", "db",
          function(.Object, ...){
            .Object <- callNextMethod(.Object, ...)
            if(.Object@name=="db_prototype"){
              .Object@name <- gsub("prototype", gen_random(), .Object@name)
            }
            if(!is.na(.Object@col_names)&&is.na(.Object@col_types)){
              .Object@col_types <- rep("?",length(.Object@col_names))
            }
            validObject(.Object)
            .Object
          })



setClass("dbm", representation(name = "character",
                               db = "list",
                               managing = "numeric",
                               path = "character",
                               colsummary = "data.frame",
                               keysummary = "data.frame"),
         prototype(name = "dbmanager_prototype",
                   db = list(),
                   managing = 0,
                   path = NA_character_,
                   colsummary = data.frame(db.names = character(),
                                           col_names = character(),
                                           col_types = character(),
                                           key = logical()),
                   keysummary = data.frame(key = character())))
setMethod("initialize", "dbm",
          function(.Object, ...){
            .Object <- callNextMethod(.Object, ...)
            if(.Object@name=="dbmanager_prototype"){
              .Object@name <- gsub("prototype", gen_random(), .Object@name)
            }
            if(!is.null(.Object@db)&&all(unlist(lapply(.Object@db, is)))){
              .Object@managing <- length(.Object@db)
              .Object@colsummary <- db_colsummary(.Object)
            }
            if(is.na(.Object@path)){
              .Object@path <- get_db_path()
            }
            .Object
          })

setGeneric("updateClass", function(object) standardGeneric("updateClass"))
setMethod("updateClass",
          signature(object = "dbm"),
          function(object){
            if(!is.null(object@db)){
              object@managing <- length(object@db)
              object@colsummary <- db_colsummary(object)
            } else {
              object@managing <- 0
              object@colsummary <- data.frame(db.names = character(),
                                              col_names = character(),
                                              col_types = character())
            }
            return(object)
          })







