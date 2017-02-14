#' Extracts pixel values from Landsat imagery
#' 
#' This function must be run only after jpegR and visual QA of jpegs has been 
#' completed. It will extract either the pixel value or mean pixel values based 
#' on a point or polygon shape file respectively, from a stack of Landsat 
#' imagery. It can also extract some predefined vegetation indices (VI). This 
#' function:
#' \enumerate{
#'    \item copies individual site shape files to respective QA jpeg folders
#'    \item obtains the dates of the remaining jpegs found in each of the QA 
#'    jpeg folders
#'    \item uses the dates obtained from step 2 to extract data (pixel values or 
#'    VI) from the local USGS Landsat archive
#'    \item writes the results of step 3 to a csv file within each QA jpeg 
#'    folder
#'    \item combines all individual site data extract csv files and writes to 
#'    one csv file at the working directory level
#'    }
#'    
#' @param wkdir character string of the working directory where your original 
#' shape file is located
#' @param imdir character string of the directory where imagery is located to 
#' path/row level
#' @param shp character string of the name of the shape file (no extension)
#' @param shp.ID character string of the name of the field in the attribute
#' table of the shape file that contains the unique entries (e.g.site numbers)
#' @param option character string choice for extracted values. One of "i35", 
#' "ndvi", "b1", "b2", "b3", "b4", "b5", "b6"
#' 
#' @return The csv from step 5 will contain all extracted data (pixel values or
#' VI) for each site. An 'NA' in the data indicates that a site was impacted by 
#' cloud or fell within a missing data stripe (Landsat 7 post 2003).
#' 
#' @author Bart Huntley, \email{bart.huntley@@dpaw.wa.gov.au}
#' @keywords utilities
#'
#' @examples
#' \dontrun{
#' extractR("../wkdir", "../imdir", "plot_locations", "Plot_ID", "i35")
#' }
#'
#' @export
#' @import raster rgdal maptools dplyr stringr




extractR <- function(wkdir, imdir, shp, shp.ID, option){
  
  ## fp for shp folder #########################################################
  shpdir <- paste0(wkdir, "\\", "QAshapes")
 
  ## Get names of QA folders ###################################################
  setwd(wkdir)
  
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
  
  directories <- list.dirs()
  qind <- grep("QA_jpegs", directories)
  directories <- directories[qind]
  
  ## Get full list of image folders ############################################
  allimfolds <- list.dirs(imdir)
  allimfolds <- as.Date(allimfolds, "%Y%m%d")
  allimfolds <- allimfolds[!is.na(allimfolds)]
  
  beg <- gsub("-", "", as.character(allimfolds[1]))
  end <- gsub("-", "", as.character(allimfolds[length(allimfolds)]))
  
 
  ## Copy shape files to relevant QA folders ###################################
  setwd(shpdir)
  
  shpfiles <- list.files(pattern = "*.shp")
  shpfiles <- shpfiles[!grepl("xml", shpfiles)]
  shpnames <- unlist(strsplit(shpfiles, split = "\\."))
  shpnames <- shpnames[c(TRUE,FALSE)] #this returns the odd indexes
  
  for(i in 1:length(shpnames)){
    shp.i <- shpnames[i]
    pattern <- paste0("^", shp.i, "\\.")
    shps.i <- list.files(pattern = pattern)
    from.i <- paste0(shpdir,"\\", shps.i)
    pattern2 <- paste0("site_", shp.i, "_")
    to.i <- paste0(wkdir, "\\", directories[grep(pattern2, directories)])
    file.copy(from=from.i, to=to.i, recursive = FALSE, overwrite = TRUE, 
              copy.mode = TRUE)
  }
  
  ## get QA'd date folders per site ############################################
  qadlist <- vector("list", length(directories))
  names(qadlist) <- directories
  
  
  for(h in 1:length(directories)){
    setwd(paste0(wkdir, "\\", directories[h]))
    jpegs <- list.files(pattern = "*.jpeg")
    qadates <- substr(jpegs, 1, 10)
    qafolds <- str_replace_all(qadates, "[^[:alnum:]]", "")
    qadlist[[h]] <- qafolds
  }

  ## Extract values and right to csv's #########################################  
  for(j in 1:length(qadlist)){  
    setwd(paste0(wkdir, "\\", directories[j]))
    
    ## shp file biz
    shp.j <- strsplit(list.files(pattern = "*.shp"), split = "\\.")[[1]][1]
    sitesSHP <- readOGR(dsn = ".", shp.j)
    rnames <- as.character(sitesSHP@data[, shp.ID])
    rownames(sitesSHP@data) <- rnames
    namesSHP <- rownames(sitesSHP@data)
    
    ## obtain file paths for all .pre
    good.folds <- qadlist[[j]]
    good.paths <- paste0(imdir, "\\", good.folds)
    
    ## dates
    date<- as.Date(good.folds, "%Y%m%d")
    
    ## construct satellite designator
    sat <- numeric(length(good.paths))
    for(h in 1:length(good.paths)){
      setwd(good.paths[h])
      pre_im <- list.files(pattern = "*.pre$")
      sat[h] <- substr(pre_im, 2, 2)
    }
    
    ## empty df for results and options vector
    results.a <- as.data.frame(matrix(ncol=length(namesSHP), dimnames=list(NULL, namesSHP)))
    options <- c("i35", "ndvi", "b1", "b2", "b3", "b4", "b5", "b6")
    
    ## extract
    for (k in 1:length(good.folds)){                
      dir.i <- good.paths[k]
      setwd(dir.i)
      files.i <- list.files(pattern="pre.ers")#all pre.ers
      
      if(length(files.i) > 1){
        data.i <- files.i[grepl("USG", files.i)]
      } else { data.i <- list.files(pattern="pre.ers")}#preferentially USG if 2
      
      pr <- substr(data.i, 5, 9)
      
      b1R.i<-raster(data.i, band=1)
      b2R.i<-raster(data.i, band=2)
      b3R.i<-raster(data.i, band=3)
      b4R.i<-raster(data.i, band=4)
      b5R.i<-raster(data.i, band=5)
      b6R.i<-raster(data.i, band=6)
      
      ## transform shp to raster
      sitesSHPt <- spTransform(sitesSHP, crs(b1R.i))
      
      if(option == "i35"){
        b3X.i<-extract(b3R.i, sitesSHPt)
        b5X.i<-extract(b5R.i, sitesSHPt)
        m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        m5.i <- unlist(lapply(b5X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        i35.i<-(m3.i+m5.i)/2
        out<-i35.i
      } else if (option == "ndvi"){
        b3X.i<-extract(b3R.i, sitesSHPt)
        b4X.i<-extract(b4R.i, sitesSHPt)
        m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        m4.i <- unlist(lapply(b4X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        ndvi.i<-(m4.i-m3.i)/(m4.i+m3.i)
        out<-ndvi.i
      } else if (option == "b1"){
        b1X.i<-extract(b1R.i, sitesSHPt)
        m1.i <- unlist(lapply(b1X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m1.i
      } else if (option == "b2"){
        b2X.i<-extract(b2R.i, sitesSHPt)
        m2.i <- unlist(lapply(b2X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m2.i
      } else if (option == "b3"){
        b3X.i<-extract(b3R.i, sitesSHPt)
        m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m3.i
      } else if (option == "b4"){
        b4X.i<-extract(b4R.i, sitesSHPt)
        m4.i <- unlist(lapply(b4X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m4.i
      } else if (option == "b5"){
        b5X.i<-extract(b5R.i, sitesSHPt)
        m5.i <- unlist(lapply(b5X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m5.i
      } else {
        b6X.i<-extract(b6R.i, sitesSHPt)
        m6.i <- unlist(lapply(b6X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m6.i
      }              
      
      
      results.a<- rbind(results.a, out)
      
    } 
    
    ## write out results per shp file
    results.a <- as.data.frame(results.a[-1, ])
    names(results.a) <- namesSHP
    results.a<-cbind(date, sat, results.a)
    shpname.i <- shp.j
    setwd(paste0(wkdir, "\\", directories[j]))
    write.csv(file=paste0(pr,"_", option, "_", shpname.i, "_", beg, "-", end,
                          ".csv"), x=results.a)
              
  }
  
  ## Combine all results in one data source ####################################
  setwd(wkdir)
 
  ## empty list for storage
  resultslist <- vector("list", length(directories))
  
  ## populate list with all csv data
  dpaths <- paste0(wkdir, "\\", directories)
  for(l in 1:length(dpaths)){
    setwd(dpaths[l])
    dat <- read.csv(list.files(pattern = c(option, ".csv")), 
                    stringsAsFactors = FALSE)
    dat <- dat[,c(-1, -3)]
    dat$date <- as.Date(dat$date, "%Y-%m-%d")
    resultslist[[l]] <- dat
  }
  
  
  ## combine all data into 1 csv
  
  alldat <- data.frame(date = allimfolds)
 
  
  for(m in 1:length(resultslist)){
    alldat <- left_join(alldat, resultslist[[m]], "date")
  }
  setwd(wkdir)
  write.csv(alldat, file = paste0(pr, "_", option, "_QA_", beg, "-", end,
                                  ".csv"))
  

}

