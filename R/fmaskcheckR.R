#' Creates a list of image date folders based on outputs from fmask
#' 
#' This function is used internally in the jpegR function to create a list of 
#' image date folders to process. This function differs from checkR by querying
#' fmask outputs on a site by site basis to reject dates with cloud. It also:
#' \itemize{
#'     \item splits an ESRI shape file into multiple shape files based on unique 
#'     entries in a field of the attribute table
#'     \item stores the split shape files in a folder named \emph{QAshapes}
#'     \item performs checks to see that leap year image folders are named 
#'     correctly
#'     \item outputs results in \strong{DateSiteList} RData file
#'     }
#'
#'@param wkdir character string of the working directory where your original 
#' shape file is located
#' @param imdir character string of the directory where imagery is located to 
#' path/row level
#' @param shp character string of the name of the shape file (no extension)
#' @param shp.ID character string of the name of the field in the attribute
#' table of the shape file that contains the unique entries (e.g.site numbers)
#' 
#' @return creates a list of image date folders in RDA format used in the jpegR
#' function
#' 
#' @author Bart Huntley, \email{bart.huntley@@dpaw.wa.gov.au}
#' @keywords utilities
#'
#' @examples
#' \dontrun{
#' checkR("../wkdir", "../imdir", "plot_locations", "Plot_ID")
#' }
#'
#'
#' @export
#' @import raster rgdal maptools





fmaskcheckR <- function(wkdir, imdir, shp, shp.ID){
  ## Create QAshapes working folder for shapefiles #############################
  setwd(wkdir)
  if(!file.exists("QAshapes")){ dir.create("QAshapes")}
  shpdir <- paste0(wkdir, "\\", "QAshapes")
  
  ## QA leap file folders for correct date #####################################
  leapR <- function(imdir){
    setwd(imdir)
    allfiles <- list.files(recursive = TRUE)
    result <- allfiles[grepl("*pre.ers", allfiles)]
    result <- result[!grepl("^[a-zA-Z]", result)]
    #date for folder
    fold <- substr(result, 1, 8)
    fdate <- as.character(as.Date(fold, "%Y%m%d"))
    #date for image
    ldate <- as.character(as.Date(substr(result, 21, 26), "%d%m%y"))
    #find mismatch
    bad.fold.dates <- setdiff(fdate, ldate)
    #correct the folder names and path
    corr.fold.dates <- as.Date(bad.fold.dates, "%Y-%m-%d") - 1
    corr.fold <- gsub("-", "", as.character(corr.fold.dates))
    new.name <- paste0(imdir, "\\", corr.fold)
    #old folder names and path to correct
    bad.fold <- gsub("-", "", as.character(bad.fold.dates))
    old.name <- paste0(imdir, "\\", bad.fold)
    #rename folders
    file.rename(old.name, new.name)
  }
  
  leapR(imdir)
  
  
  ## Handle shape file to single parts (sites) #################################
  shpSplitR2 <- function(wkdir, shpdir, shp, shp.ID){
    setwd(wkdir)
    data <-  readOGR(dsn = ".", shp)
    unique <- unique(data@data[,shp.ID])
    sites <- as.character(unique)
    setwd(shpdir)
    for(i in 1:length(sites)){
      tmp <- data[data@data[,shp.ID] == sites[i], ]
      writeOGR(tmp, dsn=getwd(), sites[i], driver="ESRI Shapefile",
               overwrite_layer=TRUE)
    }
  }
  
  shpSplitR2(wkdir, shpdir, shp, shp.ID)
  
  
  ## Get shp names to iterate through ##########################################
  shpfiles <- list.files(path = shpdir, pattern = "*.shp")
  shpfiles <- shpfiles[!grepl("xml", shpfiles)] #xml handler
  shpnames <- unlist(strsplit(shpfiles, split = "\\."))
  shpnames <- shpnames[c(TRUE,FALSE)] #this returns the odd indexes
  
  
  ## Get full list of folders and filepaths ####################################
  list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                        full.names=FALSE, ignore.case=FALSE) {
    # use full.names=TRUE to pass to file.info
    all <- list.files(path, pattern, all.dirs,
                      full.names=TRUE, recursive=FALSE, ignore.case)
    dirs <- all[file.info(all)$isdir]
    # determine whether to return full names or just dir names
    if(isTRUE(full.names))
      return(dirs)
    else
      return(basename(dirs))
  }
  
  allimdirFolds <- list.dirs(imdir)
  allimdirFolds <- as.Date(allimdirFolds, "%Y%m%d")
  readyFolds <- allimdirFolds[!is.na(allimdirFolds)]
  readyFolds <- gsub("-", "", readyFolds)
  readyFoldsFP <- paste0(imdir, "\\", readyFolds)
  
  
  ## Create list to hold cloud free dates per site #############################
  cloudDateSiteList <- vector("list", length(shpnames))
  goodnames <- paste0("site_", shpnames, "_dates")
  names(cloudDateSiteList) <- goodnames
  
  
  ## Loop through fmask cloud rasters to QA ####################################
  for(i in 1:length(shpnames)){
    siteshp.i <- readOGR(dsn = shpdir, shpnames[i])
    means.i <- numeric(length(readyFoldsFP))
    for(k in 1:length(readyFoldsFP)){
      setwd(readyFoldsFP[k])
      cloud.k <- list.files(pattern = "*cloud.img")
      cloud.k <- cloud.k[!grepl("xml", cloud.k)] #xml handler
      imgcloud.k <- raster(cloud.k)#, band=1
      e.k <- extract(imgcloud.k, siteshp.i)
      means.i[k] <- unlist(lapply(e.k, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
    }
    means.i[means.i != 1] <- 0 
    ind.i <- means.i == 1
    #date.k <- as.Date(readyFolds, "%Y%m%d")
    #date.out <- date.k[ind.i]
    folds.out <- readyFolds[ind.i]
    folds.out <- folds.out[!is.na(folds.out)]
    
    cloudDateSiteList[[i]] <- folds.out
  }
  
  setwd(wkdir)
  save(cloudDateSiteList, file = "cloudDateSiteList.RData")
  return(cloudDateSiteList)
}


