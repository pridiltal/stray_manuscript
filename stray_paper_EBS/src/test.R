data1 <- as.data.frame(matrix(rnorm(3*100), ncol = 3))
colnames(data1) <-c("v1", "v2", "v3")
head(data1)


unitize <- function(z) {
  zrange <- range(z)
  if (!(dif <- diff(zrange))) {
    return(rep(0, length(z)))
  }
  (z - zrange[1]) / dif
}
data2 <- as.data.frame(apply(as.matrix(data1), 2, unitize))
colnames(data2)<-c("u1", "u2", "u3")
head(data2)
data<- bind_cols(data1, data2)
head(data)

#library(GGally)
#p2 <- ggparcoord( d, columns = 1:3)


data1<- data1%>% mutate(score = c(10,rnorm(99)))
library(plotly)
d <- highlight_key(data1[1:5,])
p1<- ggplot(d, aes(v1, v2))+
  geom_point(data = data1, aes(v1, v2), color ="gray" )+
  geom_point(color = "red")+
  theme(aspect.ratio = 1)

pa <- ggplot()+
  geom_point(data = data1, aes(x=v1, y= score), color= "gray")+
  geom_point(data= d,aes(x=v1, y= score), color="red" )
pb<- ggplot()+
  geom_point(data = data1, aes(x=v2, y= score), color= "gray")+
  geom_point(data= d,aes(x=v2, y= score), color="red" )

p2<- ggplot(d, aes(x = factor(1), y = v1)) +
  geom_boxplot(fill = "grey80", colour = "black")+
  geom_jitter(color= "red",  width = 0.1, size = 1) +
  labs(x = NULL) 

  
  p3<- ggplot(d, aes(x = factor(1), y = v2)) +
    geom_boxplot(fill = "grey80", colour = "black")+
    geom_jitter(color="red", width = 0.1, size = 1) +
  labs(x = NULL) 

  p4 <- ggplot(d, aes(x = factor(1), y = v3)) +
    geom_boxplot(fill = "grey80", colour = "black")+
    geom_jitter(color="red", width = 0.1, size = 1) +
    labs(x = NULL) 
#p2<-ggplot(d, aes(x = 1, y = v1)) +
 # geom_dotplot(binaxis = "y", stackdir = "center")
#p3<-ggplot(d, aes(x = 1, y = v3)) +
#  geom_dotplot(binaxis = "y", stackdir = "center")


subplot(p1, subplot(pa,pb, nrows = 2), p2, p3,p4) %>% hide_legend() %>% highlight("plotly_hover")


##################################
m <- highlight_key(mpg, ~class)
p1 <- ggplot(m, aes(displ, fill = class)) + geom_density()
p2 <- ggplot(m, aes(displ, hwy, fill = class)) + geom_point()
subplot(p1, p2) %>% hide_legend() %>% highlight("plotly_hover")


http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
ggplot(wdata, aes(x = factor(1), y = weight)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = sex, shape = sex), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)   # Remove x axis label




## ---- AusiPedestrianSensorData
#Running the following on the command line resolved rJava issue for me:
# https://stackoverflow.com/questions/42394244/rstudio-knit-button-fail-to-load-rjava
# sudo R CMD javareconf
library(rJava)
#library(rwalkr)
#rwalkr::melb_shine() ## this is to download sensor data 
ped_data <- read.csv(file="data/pedestrian-2018-DecJan.csv", header=TRUE)
ped_data <- data.frame(ped_data)
Dec_data <- ped_data %>% filter(Date != "2019-01-01")
Dec_data%>% ggplot(aes(Time, Count)) +
  geom_point()+
  facet_wrap(vars(Date)) ->p


day <- unique(Dec_data$Date)

calc_scat_features <- function(x,Dec_data )
{
  return(Dec_data %>% filter(Date == x) %>% select(Time, Count)%>%
           scagnostics::scagnostics())
}

features <- sapply(day, calc_scat_features, Dec_data=Dec_data) %>%t()%>% data.frame()
features_select <- features%>% select(Outlying, Convex, Skinny, Stringy,Monotonic)
out <- stray::find_HDoutliers(features_select, knnsearchtype = "FNN_brute")

#day[out$outliers]
output_data <- Dec_data %>% mutate(Type = ifelse(Date %in% day[out$outliers], "Outlier", "Typical"))
output_data$Type <- factor(output_data$Type)
output_data$Date <- factor(output_data$Date)


p <- ggplot(data = output_data, aes(x = Time, y = Count)) +
  geom_point()
q <- p  + facet_wrap(vars(Date), ncol = 8 ) + theme(aspect.ratio = 1)
out_data <- subset(output_data, Type == "Outlier")
out_plot <- q + geom_point(data = out_data, color = "red")

print(out_plot)


## ---- FeaturePedetrian
library(GGally)
features <- sapply(day, calc_scat_features, Dec_data=Dec_data) %>%t()%>% data.frame()
features_select <- features%>% select(Outlying, Convex, Skinny, Stringy,Monotonic)
out <- stray::find_HDoutliers(features_select, knnsearchtype = "FNN_brute")
feature_data <- features_select%>%mutate(Type = out$type)



p <- GGally::ggpairs(feature_data,
                     columns = 1:5,upper = "blank", 
                     #diag = "blank",
                     ggplot2::aes(colour = Type, shape = Type, alpha = Type)
)

for (i in 1:p$nrow) {
  for (j in 1:p$ncol) {
    p[i, j] <- p[i, j] +
      scale_fill_manual(values = c( "outlier" ="red", "typical" = "black")) +
      scale_color_manual(values = c("outlier" ="red", "typical" ="black")) +
      scale_shape_manual(values = c( "outlier" = 17, "typical" =16)) +
      scale_alpha_manual(values = c( "outlier" = 1, "typical" =1))
  }
}

p1<- p + theme(aspect.ratio = 1 )
print(p1)


#### High dimensional data

load(file = "data/real_3.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")

p1 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000", "#000000"), values = c(0, 0.1, max(tsframe))) +
  ylab("Sensor ID") +
  scale_y_continuous(breaks = seq(0, 2500, 500)) +
  scale_x_continuous(breaks = seq(0, 500, 100)) +
  xlab("Time") +
  theme(legend.position = "none") +
  ggtitle("(c) Multivariate time series plot") +
  theme(
    title = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 12)
  )+ 
  geom_vline(xintercept = c(0,250,499), colour="blue", linetype = "longdash")



## Panel 1
data1 <- tsframe
f1 <- oddstream::extract_tsfeatures(data1)
HDoutliers::HDoutliers(f1)
out <- stray::find_HDoutliers(f1, knnsearchtype = "FNN_brute")
data<- f1 %>% data.frame()%>% mutate(Type = out$type)
pc <- pcaPP::PCAproj(data, k = 2, scale = sd, center = mean)
pcnorm <- pc$scores[, 1:2]  %>% data.frame()
colnames(pcnorm) <- c("PC1", "PC2")
data <- data %>% mutate(PC1 = pcnorm$PC1, PC2 = pcnorm$PC2, Sensor = 1:nrow(data))
q1 <- ggplot(data, aes(x=PC1,y=PC2, color = Type))+
  geom_point()+
  theme(aspect.ratio = 1)+
  scale_colour_manual(
    name = "Type",
    values = c("typical" = "black", "outlier" = "red")
  ) +
  geom_text(aes(label=ifelse(Type == "outlier", as.character(Sensor),'')),
            hjust=1,vjust=1)

## Panel 2
data1 <- tsframe[250:499,]
f1 <- oddstream::extract_tsfeatures(data1)
out <- stray::find_HDoutliers(f1, knnsearchtype = "FNN_brute")
data<- f1 %>% data.frame()%>% mutate(Type = out$type)
pc <- pcaPP::PCAproj(data, k = 2, scale = sd, center = mean)
pcnorm <- pc$scores[, 1:2]  %>% data.frame()
colnames(pcnorm) <- c("PC1", "PC2")
data <- data %>% mutate(PC1 = pcnorm$PC1, PC2 = pcnorm$PC2, Sensor = 1:nrow(data))
q2 <- ggplot(data, aes(x=PC1,y=PC2, color = Type))+
  geom_point()+
  theme(aspect.ratio = 1)+
  scale_colour_manual(
    name = "Type",
    values = c("typical" = "black", "outlier" = "red")
  ) +
  geom_text(aes(label=ifelse(Type == "outlier", as.character(Sensor),'')),
            hjust=1,vjust=1)

p<- ggpubr::ggarrange(p1,
                      ggarrange(q1,q2, ncol= 2), 
                      nrow=2)

print(p)

## ---- LaterDelete
output <- NULL
creditcard_data <- read.csv(file="data/creditcard.csv", header=TRUE)

data <- data.frame(creditcard_data)
set.seed(123)
data<- data[sample(5000),]
pc <- pcaPP::PCAproj(data[,-31], k = 2, scale = sd, center = mean)
pcnorm <- pc$scores[, 1:2]  %>% data.frame()
colnames(pcnorm) <- c("PC1", "PC2")
data<- data%>% mutate(PC1 = pcnorm$PC1, PC2 = pcnorm$PC2)
ggplot(data, aes(x=PC1,y=PC2, color =factor( Class) ))+
  geom_point()+
  theme(aspect.ratio = 1)

data<- data[,-31]
HDoutliers::HDoutliers(data)

data <- ped_data[1:100,-1]
t <- microbenchmark::microbenchmark( out_case<- HDoutliers(data, maxrows = 70000))
print(t)
time <-   mean(t$time) / 10^6
FP <- length(out_case)/60000
output <-  rbind(output, c( FP,time))

print(output)
outlier_HD_WoC <-  HDoutliers(ped_data, )

