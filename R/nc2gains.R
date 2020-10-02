#' @title nc2gains
#' @description writes csv files based on MAgPIE nc outputs
#' @return writes csv and returns a dataframe
#' @param file file to convert
#' @param check TRUE aggregates to global for each year and variable to compare against raw data (rasters)
#' @author David Chen
#' @importFrom  raster brick stack raster
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom  readxl read_xlsx
#' @importFrom dplyr inner_join mutate
#' @importFrom tidyr pivot_longer separate
#' @importFrom methods as
#' @importFrom utils write.csv
#' @importFrom tibble rownames_to_column
#' @export

nc2gains <- function(file, check = TRUE){

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
df <- inner_join(df, mapping, by=c("x" = "lon" , "y"="lat"))

#create a split column for the variable by the area share
df <- mutate(df, split= df$value * df$Share)

#aggregate by GAINS  region
df <- as.data.frame(df)
df <- aggregate(df$split, by=list(year=df$year, variable=df$variable,Idregions=df$Idregions), FUN=sum)
colnames(df)[4] <- "value"


if (check ==TRUE) {
#check with original rasters
checka <- as.data.frame(cellStats(b, sum))
checka <- rownames_to_column(checka)
  checka[,1] <- gsub("X", "", checka[,1])
  checka[,1] <- sub("\\.", "_", checka[,1])
checka <- separate(checka, col="rowname", into=c("year", "variable"), sep="_")
colnames(checka)[3] <- "original"
checkb <- aggregate(df$value, by=list(year=df$year, variable=df$variable), FUN=sum,na.rm=T)
check <- inner_join(checka,checkb, by=c("year","variable"))
check <- mutate(check, diff = ((check$x - check$original)/check$original*100))
avg_diff <- mean(check$diff,na.rm=T)
cat(paste0("Difference in global total by year and by variable is average ", round(avg_diff,2),"%"))
}
#save as file.csv
write.csv(df, file=paste0(gsub("(.*?)\\..*", "\\1", file),"_GAINS",".csv"))
return(df)
}
