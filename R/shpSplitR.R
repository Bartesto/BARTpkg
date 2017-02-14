#' A tool for splitting ESRI shape files
#'
#' Takes an ESRI shape file and splits it into multiple shape files based on
#' unique entries in a field of the attribute table.
#'
#' @param shp character string of the name of the shape file (no extension)
#' @param shp.ID character string of the name of the field in the attribute
#' table of the shape file that contains the unique entries (e.g.site numbers)
#'
#' @return creates multiple ESRI shape files each named after the unique entry
#' found in 'shp.ID'.
#'
#' @author Bart Huntley, \email{bart.huntley@@dpaw.wa.gov.au}
#' @keywords utilities
#'
#' @examples
#' \dontrun{
#' shpSplitR("plot_locations", "Plot_ID")
#' }
#'
#'
#' @export
#' @import raster rgdal maptools
#'



shpSplitR <- function(shp, shp.ID){
  data <-  readOGR(dsn = ".", shp)
  unique <- unique(data@data[,shp.ID])
  sites <- as.character(unique)
  for(i in 1:length(sites)){
    tmp <- data[data@data[,shp.ID] == sites[i], ]
    writeOGR(tmp, dsn=getwd(), sites[i], driver="ESRI Shapefile",
             overwrite_layer=TRUE)
    }
}







