#' @title nc2gains
#' @description writes csv files based on MAgPIE nc outputs
#' @return writes csv and returns a dataframe
#' @param file file to convert
#' @author David Chen
#' @importFrom  raster brick stack raster
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom  readxl read_xlsx
#' @importFrom dplyr inner_join mutate
#' @importFrom tidyr pivot_longer separate
#' @importFrom data.table setDT
#' @importFrom methods as
#' @importFrom utils write.csv
#' @export

nc2gains <- function(file){

#getmapping
mapping <- as.data.frame(read_xlsx(system.file("extdata", "GAINS_share.xlsx", package="MagpieNCGains")))

#nc variable names
nc <- nc_open(file)
names <- names(nc$var)
years <- ncvar_get(nc, "time")

#stack all variables in the raster
b <- raster()
for (i in 1:length(names)){
  a <- brick(file, varname=names[i])
  b<- stack(b, a)
  names(b)[(length(years)*(i-1)+1):(length(years)*(i-1)+length(years))] <- paste0(names(b)[(length(years)*(i-1)+1):(length(years)*(i-1)+length(years))], ".", names[i])
}

#clean up names
df <- as.data.frame(as(b, "SpatialPixelsDataFrame"))
colnames(df) <- gsub("X", "", colnames(df))
colnames(df) <- sub("\\.", "_", colnames(df))
df <- pivot_longer(df, c(1:(length(df)-2)), names_to = "year", values_to="value")
df <- separate(df, col="year", into=c("year", "variable"), sep="_")

#join mapping
df <- inner_join(df, mapping, by=c("x"="lon","y"="lat"))

#create a split column for the variable by the area share
df <- mutate(df, split= df$value * df$Share)

#convert to data table as setDT much faster for aggregating
df <- as.data.frame(df)
df <- setDT(df)[,(value = sum(split)), keyby = 'Idregions,year,variable']
colnames(df)[4] <- "value"
#save as file.csv
write.csv(df, file=paste0(gsub("(.*?)\\..*", "\\1", file),"_GAINS",".csv"))
return(df)
}
