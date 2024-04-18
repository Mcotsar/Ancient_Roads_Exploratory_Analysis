library ("ggplot2")
library ("dplyr")
library ("viridis") 
library("tidyr")
library("GGally")
library("hexbin")
library("broom")
library("reshape2") ##melt package!!!
library("gridExtra")


################################################ EXPLORATORY ANALYSIS (BOXPLOT) ##########################################################


#cell 1 polygons new data #


#exploratory analysis

datacell1 = cell_1deg
datacell1
datacell1 = datacell1[,5:6]
datacell1

boxplot(datacell1)


#boxplot

databox1 = datacell1
databox1 = melt(databox1)
databox1
boxplot1 = ggplot(databox1, aes (x=variable, y=value, fill=variable)) + geom_boxplot(varwidth = TRUE, alpha=0.2) + ggtitle("1 degree cell") + theme (plot.title = element_text(hjust = 0.5)) + xlab("") + ylab ("P") + scale_fill_discrete(name = "") + theme(legend.position='none')
boxplot1

# cell 0.5 polygons #

#exploratory analysis


datacell0.5 = cell_05deg
datacell0.5
datacell0.5 = datacell0.5[,5:6]
datacell0.5

boxplot(datacell0.5)


#boxplot

databox0.5 = datacell0.5
databox0.5 = melt(databox0.5)
databox0.5
boxplot0.5 = ggplot(databox0.5, aes (x=variable, y=value, fill=variable)) + geom_boxplot(varwidth = TRUE, alpha=0.2) + ggtitle("0.5 degree cell") + theme (plot.title = element_text(hjust = 0.5)) + xlab("") + ylab ("P") + scale_fill_discrete(name = "") + theme(legend.position='none')
boxplot0.5


# provinces # no regular cells

#exploratory analysis

datacellprov = provinces
datacellprov
datacellprov = datacellprov[,6:7]
datacellprov

boxplot(datacellprov)


#boxplot

databoxprov = melt(datacellprov)
databoxprov
boxplotprovinces = ggplot(databoxprov, aes (x=variable, y=value, fill=variable)) + geom_boxplot(varwidth = TRUE, alpha=0.2) + ggtitle("Provinces") + theme (plot.title = element_text(hjust = 0.5)) + xlab("") + ylab ("P") + scale_fill_discrete(name = "") 
boxplotprovinces

### all together

boxplottogether = grid.arrange(boxplot1, boxplot0.5, boxplotprovinces, ncol = 3)
boxplottogether


########################################## LINEAR REGRESSION  ########################################################


#cell 1 polygons new data #

lm1 = ggplot(datacell1, aes(x = roman_buffer_area_percentage, y = modern_buffer_area_percentage)) + ggtitle("1 degree cell") + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "red") + theme_minimal()
lm1


#cell 0.5 polygons

lm0.5 = ggplot(datacell0.5, aes(x = roman_buffer_area_percentage, y = modern_buffer_area_percentage)) + ggtitle("0.5 degree cell") + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "red") + theme_minimal()
lm0.5


#provinces 

lmpro = ggplot(datacellprov, aes(x = roman_buffer_area_percentage, y = modern_buffer_area_percentage)) + ggtitle("provinces") + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "red") + theme_minimal()
lmpro

# all together

lmtogether = grid.arrange(lm1, lm0.5, lmpro, ncol = 3)
lmtogether


########################################## PEARSON CORRELATION  ########################################################


###### cell 1 polygons ######


correlationall1 = cell_1deg
correlationall1 = cell_1deg [, -1]
correlationall1
correlationall1 <- ggcorr(correlationall1, hjust = 0.95, method = c("everything", "pearson"), label = TRUE) + ggtitle("1 degree cell")

correlationall1

summary(correlationall1)

###### cell 0.5 polygons ######



correlationall0.5 = cell_05deg
correlationall0.5 = cell_05deg [, -1]
correlationall0.5
correlationall0.5 <- ggcorr(correlationall0.5, hjust = 0.95, method = c("everything", "pearson"), label = TRUE) + ggtitle("0.5 degree cell")

correlationall0.5


###### provinces ###### no regular cells


correlationallprov = provinces
correlationallprov = provinces [, -1]
correlationallprov
correlationallprov <- ggcorr(correlationallprov, hjust = 0.95, method = c("everything", "pearson"), label = TRUE) + ggtitle("provinces")

correlationallprov


## all together

pearsontogether = grid.arrange(correlationall1, correlationall0.5, correlationallprov, ncol = 3)
pearsontogether



########################################## LINEAR MODEL REGRESSION  ########################################################

##### roads vs site_density #########

#### 1 degree cell


graph1a = ggplot(cell_1deg,aes(roman_buffer_area_percentage, site_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (1 degree cell)") + theme(legend.position='none')
graph1a


graph1b = ggplot(cell_1deg,aes(modern_buffer_area_percentage, site_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (1 degree cell)") + guides(color=guide_legend(title="Roman roads"))
graph1b

#### 0.5 degree cell


model_regression2 <- lm(roman_buffer_area_percentage ~ site_density, data = cell_05deg)
graph1c = ggplot(model_regression2,aes(roman_buffer_area_percentage, site_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(modern_buffer_area_percentage ~ site_density, data = cell_05deg)
graph1d = ggplot(model_regression2b,aes(modern_buffer_area_percentage, site_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(roman_buffer_area_percentage ~ site_density, data = provinces)
graph1e = ggplot(model_regression3,aes(roman_buffer_area_percentage, site_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(modern_buffer_area_percentage ~ site_density, data = provinces)
graph1f = ggplot(model_regression3b,aes(modern_buffer_area_percentage, site_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1a, graph1b, graph1c, graph1d, graph1e, graph1f)



##### roads vs modern_population #########


#### 1 degree cell

model_regression2 <- lm(roman_buffer_area_percentage ~ modern_population, data = cell_1deg)
graph1a = ggplot(model_regression2,aes(roman_buffer_area_percentage, modern_population)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (1 degree cell)") + theme(legend.position='none')
graph1a

model_regression2b <- lm(modern_buffer_area_percentage ~ modern_population, data = cell_1deg)
graph1b = ggplot(model_regression2b,aes(modern_buffer_area_percentage, modern_population)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (1 degree cell)") + guides(color=guide_legend(title="Roman roads"))
graph1b

#### 0.5 degree cell


model_regression2 <- lm(roman_buffer_area_percentage ~ modern_population, data = cell_05deg)
graph1c = ggplot(model_regression2,aes(roman_buffer_area_percentage, modern_population)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(modern_buffer_area_percentage ~ modern_population, data = cell_05deg)
graph1d = ggplot(model_regression2b,aes(modern_buffer_area_percentage, modern_population)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(roman_buffer_area_percentage ~ modern_population, data = provinces)
graph1e = ggplot(model_regression3,aes(roman_buffer_area_percentage, modern_population)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(modern_buffer_area_percentage ~ modern_population, data = provinces)
graph1f = ggplot(model_regression3b,aes(modern_buffer_area_percentage, modern_population)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1a, graph1b, graph1c, graph1d, graph1e, graph1f)


##### roads vs road_density #########


#### 1 degree cell

model_regression2 <- lm(roman_buffer_area_percentage ~ road_density, data = cell_1deg)
graph1a = ggplot(model_regression2,aes(roman_buffer_area_percentage, road_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (1 degree cell)") + theme(legend.position='none')
graph1a

model_regression2b <- lm(modern_buffer_area_percentage ~ road_density, data = cell_1deg)
graph1b = ggplot(model_regression2b,aes(modern_buffer_area_percentage, road_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (1 degree cell)") + guides(color=guide_legend(title="Roman roads"))
graph1b

#### 0.5 degree cell


model_regression2 <- lm(roman_buffer_area_percentage ~ road_density, data = cell_05deg)
graph1c = ggplot(model_regression2,aes(roman_buffer_area_percentage, road_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(modern_buffer_area_percentage ~ road_density, data = cell_05deg)
graph1d = ggplot(model_regression2b,aes(modern_buffer_area_percentage, road_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(roman_buffer_area_percentage ~ road_density, data = provinces)
graph1e = ggplot(model_regression3,aes(roman_buffer_area_percentage, road_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(modern_buffer_area_percentage ~ road_density, data = provinces)
graph1f = ggplot(model_regression3b,aes(modern_buffer_area_percentage, road_density)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1a, graph1b, graph1c, graph1d, graph1e, graph1f)


##### roads vs mean_elevation #########

#### 1 degree cell

model_regression2 <- lm(roman_buffer_area_percentage ~ mean_elevation, data = cell_1deg)
graph1a = ggplot(model_regression2,aes(roman_buffer_area_percentage, mean_elevation)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (1 degree cell)") + theme(legend.position='none')
graph1a

model_regression2b <- lm(modern_buffer_area_percentage ~ mean_elevation, data = cell_1deg)
graph1b = ggplot(model_regression2b,aes(modern_buffer_area_percentage, mean_elevation)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (1 degree cell)") + guides(color=guide_legend(title="Roman roads"))
graph1b

#### 0.5 degree cell


model_regression2 <- lm(roman_buffer_area_percentage ~ mean_elevation, data = cell_05deg)
graph1c = ggplot(model_regression2,aes(roman_buffer_area_percentage, mean_elevation)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(modern_buffer_area_percentage ~ mean_elevation, data = cell_05deg)
graph1d = ggplot(model_regression2b,aes(modern_buffer_area_percentage, mean_elevation)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(roman_buffer_area_percentage ~ mean_elevaation, data = provinces)
graph1e = ggplot(model_regression3,aes(roman_buffer_area_percentage, mean_elevaation)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(modern_buffer_area_percentage ~ mean_elevaation, data = provinces)
graph1f = ggplot(model_regression3b,aes(modern_buffer_area_percentage, mean_elevaation)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1a, graph1b, graph1c, graph1d, graph1e, graph1f)


##### roads vs mean_tpi_5km #########


#### 1 degree cell

model_regression2 <- lm(roman_buffer_area_percentage ~ mean_tpi_5km, data = cell_1deg)
graph1a = ggplot(model_regression2,aes(roman_buffer_area_percentage, mean_tpi_5km)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (1 degree cell)") + theme(legend.position='none')
graph1a

model_regression2b <- lm(modern_buffer_area_percentage ~ mean_tpi_5km, data = cell_1deg)
graph1b = ggplot(model_regression2b,aes(modern_buffer_area_percentage, mean_tpi_5km)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (1 degree cell)") + guides(color=guide_legend(title="Roman roads"))
graph1b

#### 0.5 degree cell


model_regression2 <- lm(roman_buffer_area_percentage ~ mean_tpi_5km, data = cell_05deg)
graph1c = ggplot(model_regression2,aes(roman_buffer_area_percentage, mean_tpi_5km)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(modern_buffer_area_percentage ~ mean_tpi_5km, data = cell_05deg)
graph1d = ggplot(model_regression2b,aes(modern_buffer_area_percentage, mean_tpi_5km)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(roman_buffer_area_percentage ~ mean_tpi_5km, data = provinces)
graph1e = ggplot(model_regression3,aes(roman_buffer_area_percentage, mean_tpi_5km)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(modern_buffer_area_percentage ~ mean_tpi_5km, data = provinces)
graph1f = ggplot(model_regression3b,aes(modern_buffer_area_percentage, mean_tpi_5km)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1a, graph1b, graph1c, graph1d, graph1e, graph1f)



