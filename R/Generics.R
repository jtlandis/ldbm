
#' @name ldb_add
#' @title ldb_add
#' @description Adds a ldb object to an existing ldbm. ldb must added with the constructor function,
#' or exist within ldbm's path as an .rds file.
#' @export
setGeneric("ldb_add", function(ldbm, ldb) standardGeneric("ldb_add"))
#' @describeIn ldb_add Method that allows the user to refer to the symbol reference of ldb
#' ldb is then saved in ldbm's path under the name of ldb \(not the symbol\). If a ldb already
#' exits in ldbm's path with the same name, ldb_add will output a warning message and return
#' ldbm unchanged.
setMethod("ldb_add",
          signature(ldbm = "ldbm", ldb = "ldb"),
          function(ldbm,ldb){
            nam <- names(ldbm@ldb)
            if(!ldb@name %in% names(ldbm@ldb)){
              if(is.na(ldb@path)){ #if no directory is set with file Make one
                newldbpath <- paste0(ldbm@path,ldb@name,"/")
                if(!dir.exists(newldbpath)){
                  dir.create(newldbpath)
                }
                ldb@path <- newldbpath
              }
              ldbm@ldb <- c(ldbm@ldb, ldb)
              names(ldbm@ldb) <- c(nam, ldb@name)
              ldbm <- updateClass(ldbm)
              saveRDS(ldb,file = paste0(ldbm@path,ldb@name,".rds"))
            } else {
              warning(paste0(ldb@name," already exists in ldbm"))
            }
            return(ldbm)
})
#' @describeIn ldb_add Method that allows the user to refer to ldb by a character name. Same as
#' the previous method, if a ldb already exits in ldbm's path with the same name, ldb_add will 
#' output a warning message and return ldbm unchanged. However, If ldb's rds does yet not exist
#' ldbm's path, then this method will fail. This method is purely conviences if a ldb is removed
#' unintentionally and it may be added quickley without the symbol reference. This also prevents
#' the user from unintentionally adding an empty ldb where they are expecting an predefined ldb.
setMethod("ldb_add",
          signature(ldbm = "ldbm", ldb = "character"),
          function(ldbm,ldb){
            path <- paste0(ldbm@path,ldb,".rds")
            if(file.exists(path)){
              if(!ldb %in% names(ldbm@ldb)){
                ldb <- readRDS(path)
                if(is.na(ldb@path)){ #if no directory is set with file Make one
                  newldbpath <- paste0(ldbm@path,ldb@name,"/")
                  if(!dir.exists(newldbpath)){
                    dir.create(newldbpath)
                  }
                  ldb@path <- newldbpath
                }
                nam <- names(ldbm@ldb)
                ldbm@ldb <- c(ldbm@ldb, ldb)
                names(ldbm@ldb) <- c(nam, ldb@name)
                ldbm <- updateClass(ldbm)
              } else {
                warning(paste0(ldb," already exists in ldbm"))
              }
              return(ldbm)
            } else {
              stop(paste0("could not find ldb pointer: ",path))
            }
          })
#' @name ldb_rm 
#' @title ldb_rm
#' @description Removes a ldb from a ldbm. This function and its methods
#' DO NOT delete any instance of ldb and its data from the file system. 
#' This purly stops ldbm from tracking the ldb.
#' @export
setGeneric("ldb_rm", function(ldbm, ldb) standardGeneric("ldb_rm"))
#' @describeIn ldb_rm Method that allows the user to refer to ldb via a symbol. It
#' should be noted that this method only checks if ldb@@name is in the names
#' of ldbm@@ldb. If so, then it is removed. This could lead to errors if a 
#' user is not wise.
setMethod("ldb_rm",
          signature(ldbm = "ldbm", ldb = "ldb"),
          function(ldbm,ldb){
            nam <- ldb@name
            if(nam %in% names(ldbm@ldb)){
              saveRDS(ldb,file = paste0(ldbm@path,ldb@name,".rds"))
              ldbm@ldb <- ldbm@ldb[!names(ldbm@ldb)%in%nam]
              ldbm <- updateClass(ldbm)
            } else {
              warning(paste0("ldb \"", nam,"\" is not present in ldbm"))
            }
            return(ldbm)
          })
#' @describeIn ldb_rm Method that allows user to refer to ldb via its character name.
#' This is more explicit as the user may check the names of ldbm easily.
setMethod("ldb_rm",
          signature(ldbm = "ldbm", ldb = "character"),
          function(ldbm,ldb){
            if(ldb %in% names(ldbm@ldb)){
              .ldb <- ldbm@ldb[[ldb]]
              saveRDS(.ldb,file = paste0(ldbm@path,.ldb@name,".rds"))
              ldbm@ldb <- ldbm@ldb[!names(ldbm@ldb)%in%ldb]
              ldbm <- updateClass(ldbm)
            } else {
              warning(paste0("ldb \"",ldb,"\" is not present in ldbm"))
            }
            return(ldbm)
          })

#' @name colsummary
#' @title Column Summary
#' @description returns a data frame that summarises information on each
#' column such as name, column type, and if it is a key column 
#' @export
setGeneric("colsummary", function(object) standardGeneric("colsummary"))
#' @describeIn colsummary Method to be used on ldb.
setMethod("colsummary",
          signature(object = "ldb"),
          function(object){
            n <- length(object@col_names)
            data.frame(ldb.names = rep(object@name, n),
                       col_names = object@col_names,
                       col_types = object@col_types,
                       key = object@col_names%in%object@key)
          })
#' @describeIn colsummary applies ldb method on each ldb in ldbm.
setMethod("colsummary",
          signature(object = "ldbm"),
          function(object){
            suppressWarnings({
              dplyr::bind_rows(lapply(object@ldb, colsummary))
            })
          })


setGeneric("ldb_read", function(ldb, ...) standardGeneric("ldb_read"))
setMethod("ldb_read",
          signature(ldb = "ldb"),
          function(ldb){
            path <- paste0(ldb@path,ldb@name,".tsv")
            if(!file.exists(path)){
              tib <- ldb@col_names %>%
                rlang::rep_named(list(character())) %>%
                tibble::as_tibble()
              
            } else {
              tib <- vroom(path, 
                           col_types = paste0(ldb@col_types,collapse = ""),
                           col_names = TRUE)
            }
            return(tib)
          })
setMethod("ldb_read",
          signature(ldb = "ldbm"),
          function(ldb, ldb.name){
            ldbm <- ldb
            if("ldb" %in% class(ldb.name)){
              ldb <- ldb.name
            } else if("character" %in%class(ldb.name)){
              if(ldb.name%in%names(ldb@ldb)){
                ldb <- ldb@ldb[[ldb.name]]
              } else {
                stop(paste0(ldb.name, " is not managed by ", ldbm@name))
              }
            }
            path <- paste0(ldb@path,ldb@name,".tsv")
            if(!file.exists(path)){
              tib <- ldb@col_names %>%
                rlang::rep_named(list(character())) %>%
                tibble::as_tibble()
              
            } else {
              tib <- vroom(path, 
                           col_types = paste0(ldb@col_types,collapse = ""),
                           col_names = TRUE)
            }
            return(tib)
          })
setGeneric("ldb_write", function(ldb, data, append = TRUE, ...) standardGeneric("ldb_write"))
setMethod("ldb_write",
          signature(ldb  = "ldb",data = "ANY", append = "ANY"),
          function(ldb, data, append){
            path <- paste0(ldb@path,ldb@name,".tsv")
            if(!all(ldb@col_names %in%colnames(data))){
              stop("data must contain all col_names of ldb")
            }
            data <- data[,ldb@col_names]
            vroom::vroom_write(x = data, path = path, append = append)
          })
setMethod("ldb_write",
          signature(ldb  = "ldbm",data = "ANY", append = "ANY"),
          function(ldb, ldb.name, data, append){
            ldbm <- ldb
            if("character" %in%class(ldb.name)&&ldb.name%in%names(ldb@ldb)){
              ldb <- ldb@ldb[[ldb.name]]
            } else if("ldb" %in% class(ldb.name)){
              ldb <- ldb.name
            }
            path <- paste0(ldb@path,ldb@name,".tsv")
            if(!all(ldb@col_names %in%colnames(data))){
              stop("data must contain all col_names of ldb")
            }
            data <- data[,ldb@col_names]
            #before writing --- check each dependency if new data (keys) are represented
            #if not -- append changes to dependent ldb inserting NAs to other columns.
            
            if(length(ldb@dependency)>0){
              for(dep in names(ldb@dependency)){
                depen <- ldb_read(ldbm, ldb.name = dep) 
                depen <- depen %>%
                  select(ldb@dependency[[dep]]) %>% distinct()
                .tmp <- data %>% select(names(ldb@dependency[[dep]])) %>% distinct()
                which_match <- mapply(all_within, x = depen, y = .tmp)
                if(!all(which_match)){
                  .tmpdepen <- full_join(ldb_read(ldbm, ldb.name = dep),
                                         .tmp, by = invertnames(ldb@dependency[[dep]]))
                  ldb_write(ldbm@ldb[[dep]], .tmpdepen, append = F)
                }
              }
              
            }
            vroom::vroom_write(x = data, path = path, append = append)
          })


setGeneric("ldb_setdependency", function(ldbm, ldb, dependency, links) standardGeneric("ldb_setdependency"))
setMethod("ldb_setdependency",
          signature(ldbm = "ldbm", ldb = "ldb", dependency = "ldb", links = "character"),
          function(ldbm, ldb, dependency, links){
            current.list <- ldbm@ldb[[ldb@name]]@dependency
            if(length(current.list)!=0&&
               dependency@name %in% names(current.list)){
              warning(paste0(dependency@name, " is already a dependency of ", ldb@name,". Overwriting."))
              ldbm@ldb[[ldb@name]]@dependency[[dependency@name]] <- links
            } else {
              ldbm@ldb[[ldb@name]]@dependency <- c(current.list, list(links))
              names(ldbm@ldb[[ldb@name]]@dependency) <- c(names(ldbm@ldb[[ldb@name]]@dependency), dependency@name)
            }
            return(ldbm)
          })



