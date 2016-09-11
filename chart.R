library(rgeos)
library(maptools)
library(ggplot2)
library(scales)
library(plyr)
eurozone <- c('AL','AD','AT','BY','BE','BA','BG','HR','CY','CZ','DK','EE','FO','FI','FR','DE','GI','EL','IT', 'HU','IS','IE','LV','LI','LT','LU','MK','MT','MD','MC','NL','NO','PL','PT','RO','SM','RS','SK','SI','ES','SE','CH','UA','UK','VA','RS','IM','RS','ME','TR');
clean <- function(x) { return(as.numeric(gsub('b', '', x))); };
euroValue <- function(x) { return((100/x)*100); };
getEuroValue <- function(path, year) {
    rawPPI <- read.table(file = path, header = TRUE, sep = '\t', na.strings = ": ")
    colnames(rawPPI) <- c("country", paste0(seq(from = 2004, to = 2015)));
    rawPPI[,1] <- gsub('PLI_EU28,E011,', '', rawPPI[,1])
    rawPPI <- rawPPI[nchar(rawPPI$country) == 2 & is.element(rawPPI$country, eurozone), ]
    yearPpi <- with(rawPPI, data.frame(country=country, `value`=clean(rawPPI[,toString(year)])))
    yearPpi[,2] <- euroValue(yearPpi[,2]) 
    return(yearPpi)
};
euMap <- readShapeSpatial("eu.shp");
euMap.fort <- fortify(euMap, region = "CNTR_ID");
mapIsoCountries <- euMap@data$CNTR_ID;
centroids.df <- as.data.frame(coordinates(euMap));
names(centroids.df) <- c("clon", "clat"); 
countriesCentroids <- data.frame(country = mapIsoCountries, centroids.df);
euroValue2015 <- getEuroValue("rawData.tsv", 2015);
euroValue2005 <- getEuroValue("rawData.tsv", 2005);
mg <- merge(euroValue2015, countriesCentroids);
plot <- ggplot() + 
    geom_map(data = mg, aes(map_id = country, fill = value), map = euMap.fort) + 
    expand_limits(x = euMap.fort$long, y = euMap.fort$lat) + 
    scale_fill_gradient2(high = muted("blue"), limits = c(min(euroValue2015[,2]), max(euroValue2015[,2]))) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(), legend.position="none",text = element_text(size = 0.5)) + 
    coord_map(ylim=c(33, 72), xlim=c(-26, 50)) + 
    geom_text(aes(x = mg$clon, y = mg$clat, label = paste0(mg$country,'\n', round(mg$value, 1), ' €')));
ggsave("euro.png", units = "cm", width = 50, height = 50);
# computing differences
euDiff <- data.frame(country = euroValue2015[,1], delta = euroValue2015[,2] - euroValue2005[,2]);
minAndMax <- minAndMax <- rbind(euDiff[order(euDiff$delta,decreasing=F)[1:6],], euDiff[order(euDiff$delta,decreasing=T)[1:6],]);
diffPlot <- ggplot(data=minAndMax, aes(x=reorder(country, delta), y=delta)) + 
    geom_bar(stat="identity", fill=muted("blue")) + 
    labs(y='Difference between 2005 and 2015',x='Country') + 
    theme(panel.background = element_blank());
plot(diffPlot);
ggsave("differences.png", width = 20, height = 20, units = "cm")



