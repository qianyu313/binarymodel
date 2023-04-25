##
## This script takes: 
##		1. a tiff population file 
##		2. a spatial polygon file
## and aggregate the population by the polygon regions

library(raster)
library(ggplot2)

pop  <- raster("../data/UR/zmb_f_15_49_2015_1km.tif")
pop_dt <- as.data.frame(pop, xy=TRUE)
colnames(pop_dt)[3] <- "population"
geo <- readRDS("../data/Geo/poly.adm2.rds")

# function to match point to polygon
match_loc_spdfvar <- function(loc_x, loc_y, spdf, varname){
    loc_df <- data.frame(x = loc_x, y = loc_y)
    coordinates(loc_df) <- ~x+y
    proj4string(loc_df) <- proj4string(spdf)
    return(over(loc_df, spdf)[, varname])
}
DistrictName <- match_loc_spdfvar(loc_x = pop_dt$x,
                                        loc_y = pop_dt$y,
                                        spdf = geo,
                                        varname = c("NAME_2"))
pop_dt <- cbind(pop_dt, DistrictName)
pop_dt <- pop_dt[!is.na(pop_dt$DistrictName), ]
pop_dt[is.na(pop_dt$population), "population"] <- 0
# plot of the grid level population
g1 <- ggplot(pop_dt, aes(x = x, y = y, fill = population)) + geom_raster() + scale_fill_viridis_c(trans = 'log') 
g1

# aggregated population
tab <- aggregate(population ~ DistrictName, data = pop_dt, FUN = sum)

g2 <- mapPlot(data = tab, geo = geo, by.data = "DistrictName", by.geo = "NAME_2", variables = "population")
g2