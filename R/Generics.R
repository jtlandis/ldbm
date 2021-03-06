
#' @name ldb_add
#' @title ldb_add
#' @description Adds a ldb object to an existing ldbm. ldb must added with the constructor function,
#' or exist within ldbm's path as an .rds file.
#' @return ldbm
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
#' @return ldbm
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
#' @return data.frame
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

#' @name ldb_read
#' @title read ldb
#' @description Function to read ldb from disk. Files are read in with
#' the vroom package. ldb@@col_types controls how vroom attempts to 
#' coerce the data types.
#' @return tibble
#' @export
setGeneric("ldb_read", function(ldb, ...) standardGeneric("ldb_read"))
#' @describeIn ldb_read expects an ldb object and will attempt to read
#' the file set in ldb@@path
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
#' @describeIn ldb_read expects ldbm as first argument, second argument may either
#' be a character value of ldb or an ldb object. If the character vector is not in
#' the path of ldbm, then it fails. If the ldb object's name is not in the path of
#' ldbm, then the returned tibble will be empty. This method implies the user is expecting
#' the ldb to be in the ldbm's path. While ldbs do not need to be in the ldbm path and 
#' may be stored elsewhere, these ldbs removed with ldb_rm may be difficult to find if
#' the user forgets. 
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
#' @describeIn ldb_read Method that allows the user to specify the
#' file location of the .rds ldb file on the file system. After reading
#' the .rds file, ldb method of ldb_read is then called.
setMethod("ldb_read",
          signature(ldb= "character"),
          function(ldb){
            if(file.exists(ldb)){
              ldb <- readRDS(ldb)
              res <- ldb_read(ldb)
              return(res)
            } else {
              stop(paste0("Cannot open: ", ldb))
            }
          })
#' @name ldb_write
#' @title write ldb
#' @description writes to file location of ldb@@path. Currently does not support 
#' what file type is written. Current table written to disk is tab separated values (.tsv).
#' @param data data to be written
#' @param append controls if data should be appended. Default set to TRUE
#' @export
setGeneric("ldb_write", function(ldb, data, append = TRUE, ...) standardGeneric("ldb_write"))
#' @describeIn ldb_write ldb argument is expect as class ldb. Writes directly to ldb@@path.
#' This method is done without ldbm, which overlooks any dependencies.
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
#' @describeIn ldb_write ldb argument expect as class ldbm, and ldb.name may be supplied as
#' a character that matches the names of the ldbm.
setMethod("ldb_write",
          signature(ldb  = "ldbm",data = "ANY", append = "ANY"),
          function(ldb, ldb.name, data, append){
            ldbm <- ldb
            if("character" %in%class(ldb.name)&&ldb.name%in%names(ldb@ldb)){
              ldb <- ldb@ldb[[ldb.name]]
            } else if("ldb" %in% class(ldb.name)){
              ldb <- ldb.name
            } else{
              stop("ldb.name is neither a ldb or a name of an ldb managed by ldbm")
            }
            path <- paste0(ldb@path,ldb@name,".tsv")
            if(!all(ldb@col_names %in%colnames(data))){
              stop("data must contain all col_names of ldb")
            }
            data <- data[,ldb@col_names]
            #before writing --- check each dependency if new data (keys) are represented
            #if not -- append changes to dependent ldb inserting NAs to other columns.
            
            if(length(ldb@dependency)>0){ #NEED TO UNDERSTAND THIS IFSTATEMENT
              #browser()
              for(dep in names(ldb@dependency)){
                depen <- ldb_read(ldbm, ldb.name = dep)
                #if(nrow(dep))
                depen <- depen %>%
                  select(all_of(ldb@dependency[[dep]])) %>% distinct()
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

#' @name ldb_setdependency
#' @title Set Dependency
#' @description Sets a one way dependency between
#' two ldbs within the same ldbm. Objects must be within
#' ldbm to work. 'dependency' will become a dependent of
#' 'ldb' and the name of 'dependency' and its key links will be 
#' stored in the dependency slot of the 'ldb' arugment.
#' If a ldb ever updates with new information, then tables dependent
#' on it should pull in new key information as well.
#' @export
setGeneric("ldb_setdependency", function(ldbm, ldb, dep, links, join.direction = "bidirection") standardGeneric("ldb_setdependency"))
setMethod("ldb_setdependency",
          signature(ldbm = "ldbm", ldb = "ldb", dep = "ldb", links = "character"),
          function(ldbm, ldb, dep, links, join.direction = "bidirection"){
            join.direction <- match.arg(arg = tolower(join.direction), choices = c("right","left","bidirection"))
            current.list.L <- ldbm@ldb[[ldb@name]]@dependency
            current.list.R <- ldbm@ldb[[dep@name]]@dependency
            
            if(join.direction%in%c("left","bidirection")){
              if(length(current.list.L)!=0&&
                 dep@name %in% names(current.list.L)){
                warning(paste0(dep@name, " is already a dependency of ", ldb@name,". Overwriting."))
                ldbm@ldb[[ldb@name]]@dependency[[dep@name]] <- links
              } else {
                nms <- names(ldbm@ldb[[ldb@name]]@dependency)
                ldbm@ldb[[ldb@name]]@dependency <- c(current.list.L, list(links))
                names(ldbm@ldb[[ldb@name]]@dependency) <- c(nms, dep@name)
              }
            } 
            if(join.direction%in%c("right","bidirection")){
              if(length(current.list.R)!=0&&
                 ldb@name %in% names(current.list.R)){
                warning(paste0(ldb@name, " is already a dependency of ", dep@name,". Overwriting."))
                ldbm@ldb[[dep@name]]@dependency[[ldb@name]] <- invertnames(links)
              } else {
                links <- invertnames(links)
                nms <- names(ldbm@ldb[[dep@name]]@dependency)
                ldbm@ldb[[dep@name]]@dependency <- c(current.list.R, list(links))
                names(ldbm@ldb[[dep@name]]@dependency) <- c(nms, ldb@name)
              }
            }
            
            ldbm <- updateClass(ldbm)
            return(ldbm)
          })

#' @name genDependencyTree
#' @title Generate Dependency Tree
#' @description Generates a dependency tree that
#' shows how the ldbs of a ldbm may be joined.
#' 
#' @importFrom tidyselect any_of
setGeneric("genDependencyTree", function(object) standardGeneric("genDependencyTree"))
setMethod("genDependencyTree",
          signature(object = "ldbm"),
          function(object){
            ldbList <- object@ldb
            df <- c()
            for(L in ldbList){
              if(length(L@dependency)>0){
                .df <- tibble(Left = L@name, Right = names(L@dependency),
                              left.name = lapply(L@dependency,names), right.name = lapply(L@dependency, unname)) %>%
                  {suppressWarnings({unnest(.,right.name, left.name)})} %>%
                  mutate(connect = "left")
                if(!is.null(df)&&nrow(df)>0){
                  df_semi <- semi_join(df, .df, by = c("Left"="Right","Right"="Left","left.name"="right.name","right.name"="left.name"))
                  if(!is.null(df_semi)&&nrow(df_semi)>0){
                    df <- df_semi %>%
                      mutate(connect = "bidirection") %>%
                      update_left(x = df, y = ., by = c("Left","Right","left.name","right.name"))
                    .df <- anti_join(.df, df, by = c("Left"="Right","Right"="Left","left.name"="right.name","right.name"="left.name"))
                  }
                }
              } else{
                .df <- c()
              }
              df <- bind_rows(df, .df)
            }
            return(df)
          })

#' @name getDepTree
#' @title Get Dependency Tree
#' @description Grab a dependency tree that
#' shows how the ldbs of a ldbm may be joined.
#' @export
setGeneric("getDepTree", function(object) standardGeneric("getDepTree"))
setMethod("getDepTree",
          signature(object = "ldbm"),
          function(object){
            object@dependency_tree
          })

#' @name genNetGraph
#' @title Generate Network Graph
#' @importFrom igraph graph
#' @importFrom tidyr gather
#' @export
setGeneric("genNetGraph", function(object) standardGeneric("genNetGraph"))
setMethod("genNetGraph",
          signature(object = "ldbm"),
          function(object){
            getDepTree(object) %>%
              rowwise() %>%
              mutate(edgemap = list(graph_edges(Left, Right, connect))) %>%
              pull(edgemap) %>% unlist() %>% matrix(nrow=2) %>% t() %>%
              {.[!duplicated(.),]} %>%
              t() %>% as.vector() %>% graph()
          })


