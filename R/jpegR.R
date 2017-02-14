#' A function that produces small jpegs of satellite imagery
#' 
#' This function makes small jpeg images centred on and showing site locations. 
#' This step is part of the cloud quality check process prior to extracting 
#' satellite bands or indices. Although designed to help the QA process the jpegs 
#' can be useful for quickly checking many image dates for change (e.g. fires,
#' vegetation removal, etc). When run the function:
#' \enumerate{
#'     \item obtains a list of imagery folders to access. Can be from one of 2 
#'     sources. Either from auto cloud QA'd output (fmaskcheckR) or from all 
#'     available imagery output (checkR). These functions are called internally
#'     \item obtains individual site shps from \emph{QAshapes} folder
#'     \item creates a small jpeg of the individual sites for each image date as 
#'     per step 1 for the purposes of visual QA
#'     \item jpegs are based on band combination from user input and buffered by 
#'     desired distance
#'     \item all outputs are placed in appropriately named folders per site in 
#'     the working directory
#'     }
#'     
#' @param wkdir character string of the working directory where your original 
#' shape file is located
#' @param imdir character string of the directory where imagery is located to 
#' path/row level
#' @param shp character string of the name of the shape file (no extension)
#' @param shp.ID character string of the name of the field in the attribute
#' table of the shape file that contains the unique entries (e.g.site numbers)
#' @param buff numeric representing how much to buffer out the site in metres. 
#' It is a way to control zoom level for jpegs
#' @param red numeric representing what satellite band to display in the red 
#' channel
#' @param green numeric representing what satellite band to display in the green 
#' channel
#' @param blue numeric representing what satellite band to display in the blue 
#' channel
#' @param fmask character string 'y' or 'n' representing whether fmask products
#' exist in each of the image date folders. Suggest 'n' as majority of our 
#' Landsat archive has not been processed by fmask
#' 
#' @return creates named folders, based on site location and dates of images 
#' processed, containing small jpeg images for visual QA
#' 
#' @author Bart Huntley, \email{bart.huntley@@dpaw.wa.gov.au}
#' @keywords utilities
#'
#' @examples
#' \dontrun{
#' jpegR("../wkdir", "../imdir", "plot_locations", "Plot_ID", 2000, 5, 4, 2, 'n')
#' }
#'
#' @export
#' @import raster rgdal maptools


jpegR <- function(wkdir, imdir, shp, shp.ID, buff, red, green, blue, fmask){
  setwd(wkdir)
 
  ## Get correct list of data ##################################################
  if(fmask == "y"){
    foldsToDo <- fmaskcheckR(wkdir, imdir, shp, shp.ID)
  } else {
    foldsToDo <- checkR(wkdir, imdir, shp, shp.ID)
  }
  
  ## Get shp names to iterate through ##########################################
  shpdir <- paste0(wkdir, "\\", "QAshapes")
  shpfiles <- list.files(path = shpdir, pattern = "*.shp")
  shpfiles <- shpfiles[!grepl("xml", shpfiles)] #xml handler
  shpnames <- unlist(strsplit(shpfiles, split = "\\."))
  shpnames <- shpnames[c(TRUE,FALSE)] #this returns the odd indexes
  goodnames <- paste0("site_", shpnames)
  
  ## Find CRS of sample raster for reproject of shp###########################
  setwd(paste0(imdir, "\\", foldsToDo[[1]][1]))
  testD <- list.files(pattern = "*pre.ers")
  testD <- testD[!grepl("xml", testD)] #xml handler
  proj <- crs(raster(testD))
  
  ## Create combo variable #####################################################
  combo <- paste0(red, green, blue)
  
  for(i in 1:length(shpnames)){
    setwd(wkdir)
    shp <- readOGR(dsn = shpdir, shpnames[i])
    shp_t <- spTransform(shp, proj)
    ext <- extent(shp_t)
    ext <- ext + buff
    beg <- foldsToDo[[i]][1]
    end <- foldsToDo[[i]][length(foldsToDo[[i]])]
    folder <- paste0("QA_jpegs_", goodnames[i], "_", beg, "-", end)
    if(!file.exists(folder)){ dir.create(folder)}
    outdir <- paste0(wkdir,"\\", folder)
    
    for(j in 1:length(foldsToDo[[i]])){
      setwd(paste0(imdir, "\\", foldsToDo[[i]][j]))
      f <- list.files(pattern = '*pre.ers')
      date <- as.Date(substring(f, 12, 17),"%d%m%y")
      jname <- paste0(date, "-", combo, ".jpeg")
      fname <- paste0(wkdir, "\\", folder, "\\", jname)
      br <- raster(f, band = red)
      br <- crop(br, ext)
      br <- stretch(br)
      bg <- raster(f, band = green)
      bg <- crop(bg, ext)
      bg <- stretch(bg)
      bb <- raster(f, band = blue)
      bb <- crop(bb, ext)
      bb <- stretch(bb)
      b <- brick(br, bg, bb)
      jpeg(filename = fname, width = 842, height = 870)
      plotRGB(b, 1, 2, 3, axes = FALSE) 
      plot(shp_t, add = TRUE, lwd = 2, border = "green")
      dev.off()
    }

    
}
}
