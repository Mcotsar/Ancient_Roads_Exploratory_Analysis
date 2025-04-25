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



# cell 0.5 polygons #

#exploratory analysis


datacell0.5 = deg_0.5_stats
datacell0.5
datacell0.5 = datacell0.5[,5:6]
datacell0.5

boxplot(datacell0.5)


#boxplot

databox0.5 = datacell0.5
databox0.5 = melt(databox0.5)
levels(databox0.5$variable) <- c("Roman road buffer percentage", "Modern road buffer percentage")
databox0.5
#boxplot0.5 = ggplot(databox0.5, aes (x=variable, y=value, fill=variable)) + geom_boxplot(varwidth = TRUE, alpha=0.2) + ggtitle("0.5 degree cell") + theme (plot.title = element_text(hjust = 0.5)) + xlab("") + ylab ("P")  + scale_fill_discrete(name = "") + theme(legend.position='none') 

boxplot0.5 = ggplot(databox0.5, aes(x = variable, y = value, fill = variable)) + 
  geom_boxplot(varwidth = TRUE, alpha = 0.2) + 
  ggtitle("0.5 degree cell") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15)) + 
  xlab("") + 
  ylab("P") + 
  scale_fill_discrete(name = "") + 
  theme(legend.position = 'none', 
        axis.text.x = element_text(size = 14))  # Aumenta tamaño de las variables en X

boxplot0.5



# provinces # no regular cells

#exploratory analysis

datacellprov = province_stats
datacellprov
datacellprov = datacellprov[,5:6]
datacellprov

boxplot(datacellprov)


#boxplot

boxplotprovinces = ggplot(databoxprov, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(varwidth = TRUE, alpha = 0.2) +
  ggtitle("Provinces") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  xlab("") +
  ylab("P") +
  scale_fill_discrete(name = "") +
  scale_y_continuous(limits = c(0, 100)) +  # <-- Aquí fijas el rango del eje Y
  theme(
    legend.position = 'none',
    axis.text.x = element_text(size = 14)
  )

boxplotprovinces


# databoxprov = melt(datacellprov)
# levels(databoxprov$variable) <- c("Roman road buffer percentage", "Modern road buffer percentage")
# databoxprov
# boxplotprovinces = ggplot(databoxprov, aes (x=variable, y=value, fill=variable)) + geom_boxplot(varwidth = TRUE, alpha=0.2) + ggtitle("Provinces") + theme (plot.title = element_text(hjust = 0.5, size = 15)) + xlab("") + ylab ("P") + scale_fill_discrete(name = "") + 
#   theme(legend.position = 'none', 
#         axis.text.x = element_text(size = 14)) 


### all together

boxplottogether = grid.arrange(boxplot0.5, boxplotprovinces, ncol = 2)
boxplottogether



########################################## LINEAR REGRESSION  ########################################################



#cell 0.5 polygons

lm0.5 = ggplot(deg_0.5_stats, aes(x = rAreaPrc, y = mAreaPrc)) + ggtitle("0.5 degree cell") + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "red") + theme_minimal()
lm0.5


#provinces 

lmpro = ggplot(province_stats, aes(x = rAreaPrc, y = mAreaPrc)) + ggtitle("provinces") + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "red") + theme_minimal()
lmpro

# all together

lmtogether = grid.arrange(lm0.5, lmpro, ncol = 2)
lmtogether


########################################## PEARSON CORRELATION  ########################################################



###### cell 0.5 polygons ######



correlationall0.5 = deg_0.5_stats
correlationall0.5 = deg_0.5_stats [, c(5, 6, 7, 8, 10, 12, 14)]
correlationall0.5
correlationall0.5 <- ggcorr(correlationall0.5, method = c("everything", "pearson"), label = TRUE, label_round = 2) + ggtitle("0.5 degree cell")

correlationall0.5




####### provinces ###### no regular cells


correlationallprov = province_stats
correlationallprov = province_stats [, c(5, 6, 7, 8, 10, 12, 14)]
correlationallprov
correlationallprov <- ggcorr(correlationallprov, method = c("everything", "pearson"), label = TRUE, label_round = 2) + ggtitle("provinces")

correlationallprov


## all together

pearsontogether = grid.arrange(correlationall0.5, correlationallprov, ncol = 2)
pearsontogether




########################################## LINEAR MODEL REGRESSION  ########################################################

##### roads vs site_density #########


#### 0.5 degree cell


model_regression2 <- lm(rAreaPrc ~ siteDens, data = deg_0.5_stats)
graph1c = ggplot(model_regression2,aes(rAreaPrc, siteDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(mAreaPrc ~ siteDens, data = deg_0.5_stats)
graph1d = ggplot(model_regression2b,aes(mAreaPrc, siteDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(rAreaPrc ~ siteDens, data = province_stats)
graph1e = ggplot(model_regression3,aes(rAreaPrc, siteDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(mAreaPrc ~ siteDens, province_stats)
graph1f = ggplot(model_regression3b,aes(mAreaPrc, siteDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1c, graph1d, graph1e, graph1f, top = "Site Density")


##### roads vs modern_population_density #########

#### 0.5 degree cell


model_regression2 <- lm(rAreaPrc ~ mPopDens, data = deg_0.5_stats)
graph1c = ggplot(model_regression2,aes(rAreaPrc, mPopDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(mAreaPrc ~ mPopDens, data = deg_0.5_stats)
graph1d = ggplot(model_regression2b,aes(mAreaPrc, mPopDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(rAreaPrc ~ mPopDens, data = province_stats)
graph1e = ggplot(model_regression3,aes(rAreaPrc, mPopDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(mAreaPrc ~ mPopDens, data = province_stats)
graph1f = ggplot(model_regression3b,aes(mAreaPrc, mPopDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1c, graph1d, graph1e, graph1f, top = "Modern Population Density")



##### roads vs modern_population #########


#### 0.5 degree cell


model_regression2 <- lm(rAreaPrc ~ modernPop, data = deg_0.5_stats)
graph1c = ggplot(model_regression2,aes(rAreaPrc, modernPop)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(mAreaPrc ~ modernPop, data = deg_0.5_stats)
graph1d = ggplot(model_regression2b,aes(mAreaPrc, modernPop)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(rAreaPrc ~ modernPop, data = province_stats)
graph1e = ggplot(model_regression3,aes(rAreaPrc, modernPop)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(mAreaPrc ~ modernPop, data = province_stats)
graph1f = ggplot(model_regression3b,aes(mAreaPrc, modernPop)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1c, graph1d, graph1e, graph1f, top = "Modern Population")


##### roads vs road_density #########


#### 0.5 degree cell


model_regression2 <- lm(rAreaPrc ~ roadDens, data = deg_0.5_stats)
graph1c = ggplot(model_regression2,aes(rAreaPrc, roadDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(mAreaPrc ~ roadDens, data = deg_0.5_stats)
graph1d = ggplot(model_regression2b,aes(mAreaPrc, roadDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(rAreaPrc ~ roadDens, data = province_stats)
graph1e = ggplot(model_regression3,aes(rAreaPrc, roadDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(mAreaPrc ~ roadDens, data = province_stats)
graph1f = ggplot(model_regression3b,aes(mAreaPrc, roadDens)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1c, graph1d, graph1e, graph1f, top = "Road density")


##### roads vs mean_elevation #########


#### 0.5 degree cell


model_regression2 <- lm(rAreaPrc ~ meanElev, data = deg_0.5_stats)
graph1c = ggplot(model_regression2,aes(rAreaPrc, meanElev)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(mAreaPrc ~ meanElev, data = deg_0.5_stats)
graph1d = ggplot(model_regression2b,aes(mAreaPrc, meanElev)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(rAreaPrc ~ meanElev, data = province_stats)
graph1e = ggplot(model_regression3,aes(rAreaPrc,meanElev)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(mAreaPrc ~ meanElev, data = province_stats)
graph1f = ggplot(model_regression3b,aes(mAreaPrc, meanElev)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1c, graph1d, graph1e, graph1f, top= "Mean Elevation")


##### roads vs mean_tpi_5km #########



#### 0.5 degree cell


model_regression2 <- lm(rAreaPrc ~ meanTpi, data = deg_0.5_stats)
graph1c = ggplot(model_regression2,aes(rAreaPrc, meanTpi)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (0.5 degree cell)") + theme(legend.position='none')
graph1c


model_regression2b <- lm(mAreaPrc ~ meanTpi, data = deg_0.5_stats)
graph1d = ggplot(model_regression2b,aes(mAreaPrc, meanTpi)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (0.5 degree cell)") + guides(color=guide_legend(title="Modern roads"))
graph1d


#### provinces

model_regression3 <- lm(rAreaPrc ~ meanTpi, data = province_stats)
graph1e = ggplot(model_regression3,aes(rAreaPrc, meanTpi)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Roman ancient roads (provinces)") + theme(legend.position='none')
graph1e


model_regression3b <- lm(mAreaPrc ~ meanTpi, data = province_stats)
graph1f = ggplot(model_regression3b,aes(mAreaPrc, meanTpi)) + geom_point() + geom_smooth(method='lm', se=TRUE) + labs(title= "Modern roads (provinces)") + theme(legend.position='none')
graph1f

## join together ##

lmtogether = grid.arrange(graph1c, graph1d, graph1e, graph1f, top= "mean TPI (5km neighbourhood)")




