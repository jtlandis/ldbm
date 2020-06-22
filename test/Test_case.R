library(ldbm)
library(tibble)
library(dplyr)
vroom.coltypes <- tibble(classes = c("character","integer","numeric","logical","factor","date","date.time"),
                         vroom = c("c","i","d","l","f","D","T"))

parse_columnClass <- function(x, ref = vroom.coltypes){
  x <- unlist(lapply(x,class)) %>%
    data.frame(classes = ., stringsAsFactors = F) %>%
    left_join(., y = ref, by = c("classes")) %>%
    pull(vroom)
  x[is.na(x)] <- "?"
  return(x)
}
make_ldb <- function(name = NULL, data, key = NA_character_, path = NA_character_, ...){
  if(is.null(name)){
    name <- paste0("ldb_",gen_random())
  }
  ldb <- Ldb(name = name, col_names = colnames(data), col_types = parse_columnClass(data), key = key, path = path, ...)
  return(ldb)
}
manager <- Ldbm(name = "Sequencing_Tracker",path = "~/Desktop/Sequencing_db")
ori.data.path <- "~/Desktop/Seq_doc/Sequencing_to_organize2.xlsx"

.proj <- readxl::read_excel(ori.data.path, sheet = "Project")
.sample <- readxl::read_excel(ori.data.path, sheet = "Sample")
.lib <- readxl::read_excel(ori.data.path, sheet = "Library")
.Run <- readxl::read_excel(ori.data.path, sheet = "Run")
.Archive <- readxl::read_excel(ori.data.path, sheet = "Archive")


manager <- ldb_add(manager, make_ldb(name = "Projects",
                                     data = .proj,
                                     key = "Proj.ref"))
manager <- ldb_add(manager, make_ldb(name = "Sample",
                                     data = .sample,
                                     key = c("Proj.ref","Sample.name","Library.id")))
manager <- ldb_add(manager, make_ldb(name = "Library",
                                     data = .lib,
                                     key = c("Sample.name","Library.id")))
manager <- ldb_add(manager, make_ldb(name = "Run", 
                                     data = .Run, key = c("Run.name","run.num","Library.id")))
manager <- ldb_add(manager, make_ldb(name = "Archive",
                                     data = .Archive, key = c("Proj.ref","run.name","run.num")))
ldb_write(manager, ldb.name = "Projects", data = .proj, append = F)
ldb_write(manager, ldb.name = "Sample", data = .sample, append = F)
ldb_write(manager, ldb.name = "Library", data = .lib, append = F)
ldb_write(manager, ldb.name = "Run", data = .Run, append = F)
ldb_write(manager, ldb.name = "Archive", data = .Archive, append = F)
# ldb_setBidependency<- function(ldbm, ldb, dep, links){
#   ldbm <- ldb_setdependency(ldbm, ldb, dep, links = links)
#   ldbm <- ldb_setdependency(ldbm, dep, ldb, links = invertnames(links))
#   return(ldbm)
# }
manager <- ldb_setdependency(manager, manager@ldb$Projects, manager@ldb$Sample, links = c("Proj.ref"="Proj.ref"))
manager <- ldb_setdependency(manager, manager@ldb$Sample, manager@ldb$Library, links = c("Sample.name"="Sample.name","Library.id"="Library.id"))
manager <- ldb_setdependency(manager, manager@ldb$Sample, manager@ldb$Run, links = c("Library.id"="Library.id"))
manager <- ldb_setdependency(manager, manager@ldb$Library, manager@ldb$Run, links = c("Library.id"="Library.id"))
manager <- ldb_setdependency(manager, manager@ldb$Projects, manager@ldb$Archive, links = c("Proj.ref"="Proj.ref"))
manager <- ldb_setdependency(manager, manager@ldb$Run, manager@ldb$Archive, links = c("Run.name"="run.name","run.num"="run.num"))



#get info on "Barcode", "Proj.name", "reads", "run.num"
find_ldb_path <- function(ldbm, selection) {
  nodes <- colsummary(manager) %>%
    rowwise() %>%
    mutate(matches_selection=col_names%in% selection) %>% ungroup() %>%
    filter(matches_selection) %>%
    pull(ldb.names) %>%
    unique()
  
  deptree <- getDepTree(manager)
  ConAvail <- deptree %>%
    filter_at(vars(Left, Right), any_vars( . %in% nodes ))
  path_ <- c()
  AnyLeft_ <- ConAvail %>%
    filter(connect %in% "left") %>%
    distinct(Left) %>% pull
  if(length(AnyLeft_)>0){
    start.db <- AnyLeft_[1]
  } else {
    start.db <- nodes[1]
  }
  while(!all(nodes %in% path_)){
    fist <- deptree %>%
      filter_at(vars(Left, Right), any_vars(.%in% start.db)) %>%
      filter_at(vars(Left, Right), all_vars(!.%in% path_))
    next.targets <- fist %>% distinct(Left, Right) %>% gather() %>%
      pull(value) %>% {.[!.%in%start.db]} %>% unique()
    if(any(next.targets%in%nodes)){ #set next.targets to the first nodes available.
      next.targets <- next.targets[next.targets %in% nodes][1]
    } #leave next.targets and repeat looking for next connection.
    next_path <- fist %>%
      filter_at(vars(Left,Right), any_vars(.%in%next.targets)) %>%
      distinct(Left,Right) %>% gather() %>%
      pull(value) %>% {.[!.%in%next.targets]} %>% unique()
    path_ <- c(path_, next_path)
    start.db <- next.targets
  }
  
  return(path_)
}

getDepen <- function(ldbm, ldb, dep){
  ldbm@ldb[[ldb]]@dependency[[dep]]
}

ldb_select <- function(ldbm, selection, keep.keys = F){
  #browser()
  path <- find_ldb_path(ldbm, selection = selection)
  if(length(ldbm@ldb[[path[1]]]@dependency)==0){
    path <- rev(path)
  }
  x <- ldb_read(ldbm, ldb.name = path[1])
  if(length(path)==1){
    df <- x
  } else {
    y <- ldb_read(ldbm, ldb.name = path[2])
    by_list <- getDepen(ldbm, path[1],path[2])
    df <- full_join(x,y, by = by_list)
  }
  if(length(path)>2){
    for(i in 3:length(path)){
      y <- ldb_read(ldbm, ldb.name = path[i])
      by_list <- c(by_list, getDepen(ldbm, path[i-1],path[i]))
      df <- full_join(x = df, y = y, by_list[by_list%in%colnames(y)])
    }
  }
  if(keep.keys){
    keys <- colsummary(ldbm) %>%
      filter(ldb.names %in% path&key) %>%
      pull(col_names) %>%
      unique
    selection <- c(keys, selection)
  }
  df <- select(df, any_of(selection)) %>%
    distinct()
  return(df)
}
# 
# test <- ldb_select(manager, c("rerun","run.name","location"), T)
# 

ldb_select(manager, c("host_organism", "kit","Technology"))

graph_edges <- function(Left, Right, join.direction){
  x <- c(Left, Right)
  if(join.direction%in%"bidirection"){
    x <- c(x, rev(x))
  } else {
    x <- rev(x)
  }
  return(x)
}





fist2 <- deptree %>%
  filter_at(vars(Left, Right), any_vars(. %in% next.targets)) %>%
  filter_at(vars(Left, Right), all_vars(!.%in% "Library"))
