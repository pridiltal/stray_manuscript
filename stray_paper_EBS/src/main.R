## ---- load
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(lvplot)
library(FNN)
library(ggthemes)
library(kableExtra)
library(gridExtra)
library(ggforce)
library(HDoutliers)
library(stray)
library(lubridate)
#library(scagnostics)
## Load user-defined functions
source("src/function.R")

## ---- outtype
set.seed(12456)
x<- c(rnorm(2000,1,2) , rnorm(2000,13,2),  40, rnorm(5,2, 1), -6, 14, -8)
y<- c(rnorm(2000,1,2),  rnorm(2000,13,2),  40, rnorm(5, 35, 0.5), 12, -6 , -8)
type <- c(rep("Typical", 4000), rep("Outlier", 9))
data <- data.frame(x,y, type)
label_data <- data[c(4001, 4002, 4008),] %>% mutate(outtype = c("Global Anomaly", "Micro clusters", "Local Anomaly"))
p <- ggplot(data, aes(x,y, color = type, shape = type, size = type))+
  geom_point()+
  scale_colour_manual(
    name = "Type",
    values = c("Typical" = "black", "Outlier" = "red")
  ) +
  scale_size_manual(
    name = "Type",
    values = c("Typical" = 3, "Outlier" = 3)
  ) +
  scale_shape_manual(
    name = "Type",
    values = c("Typical" = 16, "Outlier" = 17)
  ) +
  geom_label(data = label_data, aes(label= as.character(outtype)),
              nudge_x = -4, nudge_y = -2)+
  theme(aspect.ratio = 1, legend.position="none")
print(p)

## ---- outlierVsInlier

set.seed(120)
n <- 1000
rho <- sqrt(runif(n))+1.5
theta <- runif(n, 0, 2*pi)
x <- rho * cos(theta)
y <- rho * sin(theta)
data1 <- rbind(cbind(x,y), c(0,0), c(4,4)) %>% as.data.frame()
data1 %>% mutate(type = c(rep("Typical", n), rep("Outlier", 2))) -> data1 

label_data <- data1[1001:1002,] %>% mutate(outtype = c("Inlier", "Outlier"))
p <- ggplot(data1, aes(x,y, color = type, shape = type, size = type))+
  geom_point()+
  scale_colour_manual(
    name = "Type",
    values = c("Typical" = "black", "Outlier" = "red")
  ) +
  scale_size_manual(
    name = "Type",
    values = c("Typical" = 3, "Outlier" = 3)
  ) +
  scale_shape_manual(
    name = "Type",
    values = c("Typical" = 16, "Outlier" = 17)
  ) +
  geom_label(data = label_data, aes(label= as.character(outtype)),
             nudge_x = -0.5, nudge_y = -0.5)+
  theme(aspect.ratio = 1, legend.position="none")


x <- c(rnorm(250, mean = -6), 3, rnorm(250, mean = 10), 20)
x <- as.data.frame(x)

data <- x %>% mutate(y= rep(0, 502), type = c(rep("Typical", 250), "Outlier",rep("Typical", 250), "Outlier")) 

label_data <- data[c(251,502),] %>% mutate(outtype = c("Inlier", "Outlier"))
q <- ggplot(data, aes(x,y, color = type, shape = type, size = type))+
  geom_point()+
  scale_colour_manual(
    name = "Type",
    values = c("Typical" = "black", "Outlier" = "red")
  ) +
  scale_shape_manual(
    name = "Type",
    values = c("Typical" = 16, "Outlier" = 17)
  ) +
  scale_size_manual(
    name = "Type",
    values = c("Typical" = 3, "Outlier" = 3)
  ) +
  geom_label(data = label_data, aes(label= as.character(outtype)),
             nudge_x = 0, nudge_y = 0.25)+
  theme( legend.position="none") +
  ylim(-0.5,0.5)


t <- ggarrange(p,q, ncol = 1, nrow = 2, heights = c(1.5, 0.75))
print(t)


## ---- whyknn
set.seed(120)
x<- c(rnorm(2000,1,2),  15)
y<- c(rnorm(2000,1,2),  16.5)
data1 <- rbind(cbind(x,y))
data2 <- rbind(cbind(x,y), c(16, 15.5), c(15.5,16))

## plot a) only nearest neighbour distances
d <- knn.dist(data1, 1 )
threshold <- 1.107
df <- as.data.frame(data1) 
df_label <- tibble( x = data1[d>threshold,1], y = data1[d>threshold,2], nn_dist = round(d[d>threshold],1))
p1<-df %>% ggplot(aes(x=x, y=y)) + 
  geom_point(shape = 1, colour= "darkgray")+
  geom_point(data = df_label,  shape = 1, colour= "black")+
  geom_point(data = df[2001,],  shape = 17, colour= "red", size =3)+
  geom_text(data = df_label, aes(x = x, y= y, label=nn_dist) ,hjust=1.5,vjust=1, col="black", size=3)+
  ggtitle("(a)")+
  theme(aspect.ratio = 1)

## plot b) k nearest neighbour distances change
k=10
kdd <- knn.dist(data1, k )
score<- c(0,kdd[2001,])
df1 <- tibble(NN_Rank= 1:length(score), knndist = score)
p2 <- df1 %>% ggplot(aes(x= NN_Rank, y = knndist))+
  geom_line()+
  geom_point()+
  ylab("k-NN distance") +
  scale_x_discrete(name ="NN rank", 
                   limits=0:11, labels = c(0,0:10))+
  ggtitle("(b)")

## plot c)  only nearest neighbour distances
d <- knn.dist(data2, 1 )
threshold <- 1.033
df <- as.data.frame(data2) 
df_label <- tibble( x = data2[c(which(d>threshold), 2001:2003),1],
                    y = data2[c(which(d>threshold), 2001:2003),2],
                    nn_dist = round(d[c(which(d>threshold), 2001:2003)],1))
p3<-df %>% ggplot(aes(x=x, y=y)) + 
  geom_point(shape = 1, colour= "darkgray")+
  geom_point(data = df_label,  shape = 1, colour= "black")+
  geom_point(data = df[2001:2003,],  shape = 17, colour= "red", size=3)+
  geom_text(data = df_label, aes(x = x, y= y, label=nn_dist) ,hjust=1.5,vjust=1, col="black", size=3)+
  ggtitle("(c)")+
  theme(aspect.ratio = 1)

## plot d) k nearest neighbour distances
k=10
d_knn <- FNN::knn.dist(data2, k, algorithm = "kd_tree")
d_knn1 <- cbind(rep(0, nrow(d_knn)), d_knn)
diff1 <- t(apply(d_knn1, 1, diff))
max_diff <- apply(diff1, 1, which.max)
d <- d_knn[cbind(1:nrow(d_knn), max_diff)]
threshold <- 1.314
df <- as.data.frame(data2) 
df_label <- tibble( x = data2[(d>1.25) ,1], y = data2[(d>1.25),2], nn_dist = round(d[(d>1.25)],1))
p4<-df %>% ggplot(aes(x=x, y=y)) + 
  geom_point(shape = 1, colour= "darkgray")+
  geom_point(data = df_label,  shape = 1, colour= "black")+
  geom_point(data = df[2001:2003,],  shape = 17, colour= "red",  size=3)+
  geom_text(data = df_label, aes(x = x, y= y, label=nn_dist) ,hjust=1.5,vjust=1, col="black", size=3)+
  ggtitle("(d)")+
  theme(aspect.ratio = 1)

## plot e) k nearest neighbour distances change
k=10
kdd <- knn.dist(data2, k )
score<- c(0,kdd[2001,])
df1 <- tibble(NN_Rank= 1:length(score), knndist = score)
p5 <- df1 %>% ggplot(aes(x= NN_Rank, y = knndist))+
  geom_line()+
  geom_point()+
  ylab("k-NN distance") +
  scale_x_discrete(name ="NN rank", 
                   limits=0:11, labels = c(0,0:10))+
  ggtitle("(e)")


p<- ggarrange(ggarrange(p1,p3,p4,  ncol = 3), ggarrange(p2,p5,  ncol = 2),  nrow = 2 ,  heights = c(1.5, 1)) 
print(p)




## ---- spacings
set.seed(123)
nobs <- 20000
nsamples <- 1000
trials <- matrix(rnorm(nobs*nsamples,0,1),nrow=nsamples)
trials.sorted <- t(apply(trials, 1, function(x) sort(x, decreasing=TRUE)))
trials.means <- apply(trials.sorted, 2, mean)
mean.spacings <- diff(trials.means)
mean.spacings <- -mean.spacings
x <- 1/(1:(nobs-1))
a <- coef(lm(mean.spacings ~ 0 + x))
pred.spacings <- a*x

trial_mean <- tibble::tibble(Rank = 1:10 , order_mean = trials.means[1:10] )




ts_matrix <- trials.sorted[,1:10]
colnames(ts_matrix) <- 1:10
trial_sorted <- as.data.frame(ts_matrix) %>%
  gather(Rank, "order_stat") %>% mutate(Rank = as.numeric(Rank))


p1 <- ggplot(trial_sorted,aes(x=factor(Rank), y=order_stat)) + 
  geom_lv(fill = "grey80", colour = "black")+
  ggtitle("(a)")+
  ylab(expression(paste("Order Statistics")))+
  xlab("Rank")+
  stat_summary(fun.y=mean, geom="point", shape=3, size=2)+
  theme(    text = element_text(size=8))



ts_matrix_diff<- t(apply(ts_matrix, 1, (diff) )) * -1
ts_matrix_stdiff<- t(apply(ts_matrix_diff, 1, function(x) x*(1:9)))
data<- reshape2::melt(ts_matrix_stdiff)
p2 <- ggplot(data,aes(x=factor(Var2), y=value)) + 
  geom_lv(fill = "grey80", colour = "black")+
  ggtitle("(b)")+
  ylab(expression(paste("Standardized Spacings (",i*D[i],")")))+
  xlab("Rank")+
  stat_summary(fun.y=mean, geom="point", shape=3, size=2)+
  theme(    text = element_text(size=8))   



p<- ggarrange(p1, p2, nrow = 2, heights = c(2, 2)  ) 
print(p)


## ---- performEval
#load(file = "data/HDoutlier_with_out_cluster.rda")
#data_HDoutlier_WoC <- output
load(file = "data/stray_k_1.rda")
data_stray_k_1 <- output
#load(file = "data/HDoutlier_WC_not_5000.rda")
load(file = "data/HDoutlier_with_cluster.rda")
data_HDoutlier_WC <- output
load(file = "data/stray_k_10.rda")
data_stray_k_10 <- output
load(file = "data/stray_k_10_FNN_brute.rda")
data_stray_k_10_FNN_brute <- output
load(file = "data/stray_k_10_nabor_brute.rda")
data_stray_k_10_nabor_brute <- output
load(file = "data/stray_k_10_nabor_LH.rda")
data_stray_k_10_nabor_LH <- output
#load(file = "data/stray_k_10_nabor_TH.rda")
#data_stray_k_10_nabor_TH <- output
results<- rbind(data_stray_k_1 , data_HDoutlier_WC,  
                data_stray_k_10_FNN_brute,data_stray_k_10,
                data_stray_k_10_nabor_brute, data_stray_k_10_nabor_LH)

results <- data.frame(results)
#levels(results$Method)
#[1] "stray_k_1"             
#[2] "HDoutlier_WC_3"        
#[3] "stray_k_10_FNN_brute"  
#[4] "stray_k_10"            
#[5] "stray_k_10_nabor_brute"
#[6] "stray_k_10_nabor_LH"     
levels(results$Method) <- c("HDoutlier WoC","HDoutlier WC", 
                            "stray - FNN brute", "stray - FNN k-d trees",
                            "stray - nabor brute", "stray - nabor k-d trees")

#method_names <- list(
#  'stray_k_1'="HDoutlier WoC", 
#  'HDoutlier_WC_3'="HDoutlier WC",
  #'HDoutlier_WC'="HDoutlier WC",
#  'stray_k_10_FNN_brute' = "stray - FNN brute",
#  'stray_k_10' = "stray - FNN k-d trees",
#  'stray_k_10_nabor_brute' = "stray - nabor brute",
#  'stray_k_10_nabor_LH'="stray - nabor k-d trees"
  #'stray_k_10_nabor_TH' = "stray- nabor TH (k=10)"
#)

method_names <- list(
  'stray_k_1'="(a)", 
  'HDoutlier_WC_3'="(b) - I",
  #'HDoutlier_WC'="HDoutlier WC",
  'stray_k_10_FNN_brute' = "(c)",
  'stray_k_10' = "(d)",
  'stray_k_10_nabor_brute' = "(e)",
  'stray_k_10_nabor_LH'="(f)"
  #'stray_k_10_nabor_TH' = "stray- nabor TH (k=10)"
)


method_labeller <- function(variable,value){
  return(method_names[value])
}


p1 <- results %>% ggplot(aes(x = n_size, y = Time, color = factor(dim))) +
  geom_point()+
  geom_line() + 
  #scale_colour_colorblind(name = "Dimensions")+
  scale_colour_manual(values = c(
    "1" = "black", "100" = "#b10026","50"= "#fc4e2a", "10"= "#fd8d3c",
    "2"= "#fed976"), name = "Dimensions" ) +
  facet_grid(cols = vars(Method), labeller=method_labeller)+
  labs(x= "Sample size", y ="Anomaly Detection Time (in milliseconds)")+
  theme(legend.position="top", axis.text.x = element_text(angle = 90)) +
  ylim(0, 25000)

#print(p)

load(file = "data/HDoutlier_with_cluster.rda")
d=data.frame(x1=c(0), x2=c(10000), y1=c(0), y2=c(25000))
data_HDoutlier_WC <- output
p2 <- data.frame(data_HDoutlier_WC) %>% ggplot(aes(x = n_size, y = Time, color = factor(dim))) +
  geom_point()+
  geom_line() + 
  #scale_colour_colorblind(name = "Dimensions")+
  scale_colour_manual(values = c(
    "1" = "black", "100" = "#b10026","50"= "#fc4e2a", "10"= "#fd8d3c",
    "2"= "#fed976"), name = "Dimensions" ) +
  labs(x= "Sample size", y ="Running time (in milliseconds)")+
  geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), inherit.aes = FALSE, alpha = 0, color = "black") +
  ggtitle("(b) - II")+
  theme(legend.position="none", axis.text.x = element_text(angle = 90)) 

p<- ggpubr::ggarrange(p1,p2, nrow = 2)
print(p)

## ---- performEvalFP
load(file = "data/HDoutlier_WoC_FP.rda")
data_HDoutlier_WoC <- output
load(file = "data/HDoutlier_WC_FP.rda")
data_HDoutlier_WC <- output
load(file = "data/stray_FNN_brute_FP.rda")
stray_FNN_brute_FP <- output
load(file = "data/stray_FNN_kd_FP.rda")
stray_FNN_kd_FP <- output
load(file = "data/stray_nabor_brute_FP.rda")
stray_nabor_brute_FP <- output
load(file = "data/stray_nabor_kd_FP.rda")
stray_nabor_kd_FP <- output
#load(file = "data/stray_k_10_nabor_TH.rda")
#data_stray_k_10_nabor_TH <- output
results<- rbind(data_HDoutlier_WoC, data_HDoutlier_WC, stray_FNN_brute_FP, stray_FNN_kd_FP, 
                stray_nabor_brute_FP, stray_nabor_kd_FP)
levels(results$Method) <- c("HDoutliers WoC","HDoutliers WC", "stray - brute force", "stray - FNN kd-tree",
                            "stray - nabor brute", "stray - nabor kd-tree")
results$FP <- round(results$FP, 3)
results %>%
  filter(dim%in% c(1, 10, 100))%>% 
  group_by(Method ) %>%  
  spread(key = "n_size", value = "FP") %>% 
  kable(caption = "Performance metrics -- False positive rates. 
        The values given are based on 100 iterations and the mean values 
        are reported. Different versions of the two algorithms
        (stray and Hdoutliers) are applied on datasets where each column
        is randomly generated from the standardised normal distribution.
        All the datasets are free from anomalies
        HDoutliers WoC: HDoutliers algorithm without clustering step;
        HDoutliers WC: HDoutliers algorithm with clustering step.",
                                                 booktabs = T)

#kable(result_FP, "latex", caption = "Performance metrics", booktabs = T) %>%
# kable_styling(latex_options = "scale_down") 


## ---- compareAlgorithm

set.seed(123)
## typical example
data1<-cbind(x= c(rnorm(1000, mean =10, sd=6), 28),
             y= c(rnorm(1000,mean =10, sd=3),40))  %>% data.frame()
## Multimodal classes
data2 <-cbind(x= c(rnorm(1000), rnorm(3, mean = 10, sd = .3), rnorm(1000, mean= 20)),
              y= c(rnorm(1000), rnorm(3, mean = 10, sd = .2), rnorm(1000) ) )  %>% data.frame()
## Neighbouring clusters - Masking
data3<- cbind(x= c(rnorm(1000), rnorm(5, mean = 10, sd = .5), 0,0,5,8.5),
              y= c(rnorm(1000), rnorm(5, mean = 10, sd = .5), 8,8.5,4,-1) )  %>% data.frame()
## Inliers
n <- 1e3
rho <- sqrt(runif(n))+0.9
theta <- runif(n, 0, 2*pi)
x <- rho * cos(theta)
y <- rho * sin(theta)
data4 <- rbind(cbind(x,y),c(0,0), c(0.2,0.2)) %>% data.frame()
# problem of clustering - false positive
data5<-cbind(x= c(rnorm(1000, mean =10, sd=1), 18, c(rnorm(1000, mean =28, sd=0.15))),
             y= c(rnorm(1000,mean =28, sd=1.5), 18,  c(rnorm(1000, mean =10, sd=0.15))))  %>% data.frame()
# problem of dense clusters
#data6 <- cbind(x= c(rnorm(2000, mean =10, sd=.25), 18),
#                     y= c(rnorm(2000,mean =27, sd=.25), 18) ) %>% data.frame()

#problem of clustering - false negatives
set.seed(123)
data6<-cbind(x= c(rnorm(2000, mean =10, sd=.25), 18),y= c(rnorm(2000,mean =28, sd=.25), 18) )
data6 <- as.data.frame(data6)


plot_results_withclust <- function(data, out)
{
  unitize <- function(z) {
    zrange <- range(z)
    if (!(dif <- diff(zrange))) 
      return(rep(0, length(z)))
    (z - zrange[1])/dif
  }
  udata <- apply(as.matrix(data), 2, unitize)
  members <- getHDmembers(udata, radius = NULL, maxrows = 100)
  index<- unlist(lapply(members, `[[`, 1))
  r= .1/(log(nrow(data))^(1/ncol(data)))
  
  p <- data%>% ggplot()+ 
    geom_point( aes(x=x, y=y), color = "gray")+
    ggforce::geom_circle( data = data[index, ],aes(x0 = x, y0=y, r=r), color = "blue")+
    geom_point(data=data[out,], aes(x=x, y=y), colour = "red", shape=17, fill = "red", size = 2) +
    theme(aspect.ratio = 1, axis.text = element_text(size=8), 
          axis.title = element_blank(), title = element_text(size = 10))
  return(p)
}


plot_results <- function(data, out)
{
  p <- data%>% ggplot()+ 
    geom_point( aes(x=x, y=y), color = "black")+
    geom_point(data=data[out,], aes(x=x, y=y), colour = "red", shape=17, fill = "red", size = 2) +
    theme(aspect.ratio = 1, axis.text = element_text(size=8), 
          axis.title = element_blank(), title = element_text(size = 10))
  return(p)
}


## Row 1 : HDoutliers without clustering

out<-HDoutliers(data1)
p1a <- plot_results(data1, out) + ggtitle("a) - I)") 
out<-HDoutliers(data2)
p2a <- plot_results(data2, out)+ ggtitle("b) - I)")
out<-HDoutliers(data3)
p3a <- plot_results(data3, out)+ ggtitle("c) - I)")
out<-HDoutliers(data4)
p4a <- plot_results(data4, out)+ ggtitle("d) - I)")
out<-HDoutliers(data5)
p5a <- plot_results(data5, out)+ ggtitle("e) - I)")
out<-HDoutliers(data6)
p6a<- plot_results(data6, out)+ggtitle("f) - I)")




## Row 2 : HDoutliers with clustering using leader algorithm

out<-HDoutliers(data1, maxrows = 100)
p1b <- plot_results_withclust(data1, out)+ ggtitle("a) - II)")
out<-HDoutliers(data2, maxrows = 100)
p2b <- plot_results_withclust(data2, out)+ ggtitle("b) - II)")
out<-HDoutliers(data3, maxrows = 100)
p3b <- plot_results_withclust(data3, out)+ ggtitle("c) - II)")
out<-HDoutliers(data4, maxrows = 100)
p4b <- plot_results_withclust(data4, out)+ ggtitle("d) - II)")
out<-HDoutliers(data5, maxrows = 100)
p5b <- plot_results_withclust(data5, out) +
  stat_density_2d(data = data5,aes(x=x, y=y),
                  alpha = 0.4, bins = 4, color = "darkgreen")+
   ggtitle("e) - II)")
out<-HDoutliers(data6, maxrows = 100)
p6b <- plot_results_withclust(data6, out) +
  stat_density_2d(data = data6,aes(x=x, y=y),
                  alpha = 0.4, bins = 4, color = "darkgreen")+
  ggtitle("f) - II)")



## Row 3 : stray algorithm

out<-find_HDoutliers(data1, knnsearchtype= "kd_tree")
p1c <- plot_results(data1, out$outliers)+ ggtitle("a) - III)")
out<-find_HDoutliers(data2, knnsearchtype= "kd_tree")
p2c <- plot_results(data2, out$outliers)+ ggtitle("b) - III)")
out<-find_HDoutliers(data3, knnsearchtype= "kd_tree")
p3c <- plot_results(data3, out$outliers)+ ggtitle("c) - III)")
out<-find_HDoutliers(data4, knnsearchtype= "kd_tree")
p4c<- plot_results(data4, out$outliers)+ ggtitle("d) - III)")
out<-find_HDoutliers(data5, knnsearchtype= "kd_tree")
p5c <- plot_results(data5, out$outliers)+ ggtitle("e) - III)")
out<-find_HDoutliers(data6, knnsearchtype= "kd_tree")
p6c <- plot_results(data6, out$outliers)+ ggtitle("f) - III)")


#p<- gridExtra::grid.arrange(grid.arrange(p1a,p2a, p3a,p4a,p5a,p6a, nrow= 1),
#                            grid.arrange(p1b,p2b, p3b,p4b,p5b,p6b, nrow= 1), 
#                            grid.arrange(p1c,p2c, p3c,p4c,p5c,p6c, nrow= 1), nrow=3)

#p<- ggpubr::ggarrange(ggarrange(p1a,p2a, p3a,p4a,p5a,p6a, nrow= 6),
#                      ggarrange(p1b,p2b, p3b,p4b,p5b,p6b, nrow= 6), 
#                      ggarrange(p1c,p2c, p3c,p4c,p5c,p6c, nrow= 6), 
#                      ncol=3)

p<- ggpubr::ggarrange(ggarrange(p1a,p2a, p3a,p4a,p5a, p6a, ncol= 6),
                      ggarrange(p1b,p2b, p3b,p4b,p5b, p6b, ncol= 6), 
                      ggarrange(p1c,p2c, p3c,p4c,p5c, p6c, ncol= 6), 
                     nrow=3)

#p <- ((p1a+p2a+p3a+p4a+p5a) + (p1b+p2b+ p3b+p4b+p5b) + 
#        (p1c+p2c+p3c+p4c+p5c) + plot_layout(ncol = 1))
print(p)


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
out <- stray::find_HDoutliers(features_select, knnsearchtype = "brute")

#day[out$outliers]
output_data <- Dec_data %>% mutate(Type = ifelse(Date %in% day[out$outliers], "Outlier", "Typical"))
output_data <- mutate(output_data, Day = wday(Date, label = TRUE))
output_data <- mutate(output_data, Day = factor(Day, levels = rev(levels(Day))))
output_data$Type <- factor(output_data$Type)
output_data$Date <- factor(output_data$Date)


p <- ggplot(data = output_data, aes(x = Time, y = Count)) +
  geom_point()
q <- p  + facet_wrap(vars(Date, Day), ncol = 7 ) 
#+ theme(aspect.ratio = 1)
out_data <- subset(output_data, Type == "Outlier")
out_plot <- q + geom_point(data = out_data, color = "red") + xlab("Time") + 
  ylab ( "Hourly pedestrian counts")

print(out_plot)



## ---- FeaturePedetrian
library(GGally)
features <- sapply(day, calc_scat_features, Dec_data=Dec_data) %>%t()%>% data.frame()
features_select <- features%>% select(Outlying, Convex, Skinny, Stringy,Monotonic)
out <- stray::find_HDoutliers(features_select, knnsearchtype = "brute")
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

p1<- p 
#+ theme(aspect.ratio = 1 )
print(p1)

## ---- PCPedetrian
features <- sapply(day, calc_scat_features, Dec_data=Dec_data) %>%t()%>% data.frame()
features_select <- features%>% select(Outlying, Convex, Skinny, Stringy,Monotonic)
out <- stray::find_HDoutliers(features_select, knnsearchtype = "brute")
feature_data <- features_select%>%mutate(Type = out$type)


pc <- pcaPP::PCAproj(features_select, k = ncol(features_select), scale = sd, center = mean)
pcnorm <- pc$scores[, 1:2]
colnames(pcnorm) <- c("PC1", "PC2")

#library(Rtsne)
#tsne <- Rtsne(features_select, dims = 2, perplexity=5, verbose=TRUE, max_iter = 500)
#exeTimeTsne<- system.time(Rtsne(features_select, dims = 2, perplexity=5, verbose=TRUE, max_iter = 500))
p <- data.frame(pcnorm) %>%mutate(Type = out$type) %>% 
  ggplot(aes(PC1, PC2, color = Type)) + 
  geom_point() +
  theme(aspect.ratio = 1)+
  scale_colour_manual(
    name = "Type",
    values = c("typical" = "black", "outlier" = "red")
  ) 

p

## ---- Dec31AnomSensors
#####################################################
###################### Analysis for Dec 1st, 2018
#####################################################
Dec_31_data <- ped_data %>% filter(Date == "2018-12-31")
Dec_31_data%>% ggplot(aes(x=Time,y= Count, group = Sensor)) +
  geom_point(color = "black")+
  geom_line(color = "black") ->p

Dec_31_data%>% select(Sensor, Time, Count) %>%
  spread(key = Sensor, value = Count) -> sensor_data

sensor_data <- sensor_data[,-1]
extract_tsfeatures1(sensor_data) %>% as.data.frame() ->features_dec_31

selected_features<-features_dec_31%>% select(moment3, mean, variance, linearity, spikiness, BurstinessFF, maximum)

out<- stray::find_HDoutliers(selected_features, knnsearchtype = "brute" )
outlying_sensor<- colnames(sensor_data)[out$outliers]

out_Dec_31 <- Dec_31_data %>% filter(Sensor %in% outlying_sensor )
out_Dec_31 <- data.frame(out_Dec_31)
q1 <- p+ geom_point(data = out_Dec_31, aes(x=Time,y= Count), color= "red")+
  geom_line(data = out_Dec_31, aes(x=Time,y= Count), color= "red") +
  geom_label(data = out_Dec_31, aes(x = 18, y = 7800,label = Sensor)) 
#out$outliers
print(q1)


## ---- Dec31AnomSensorsFeatures
############ Calculating time series features for each sensor
Dec_31_data%>% select(Sensor, Time, Count) %>%
  spread(key = Sensor, value = Count) -> sensor_data

sensor_data <- sensor_data[,-1]
extract_tsfeatures1(sensor_data) %>% as.data.frame() ->features_dec_31

selected_features<-features_dec_31%>% select(moment3, mean, variance, linearity, spikiness, BurstinessFF, maximum)

out<- stray::find_HDoutliers(selected_features, knnsearchtype = "brute" )
#out$outliers
selected_features <- selected_features%>%mutate(Type = out$type)

p <- GGally::ggpairs(selected_features,
                     columns = 1:7,upper = "blank" ,
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

f1<- p 
#+ theme(aspect.ratio = 1 )
print(f1)

## ---- Dec1AnomSensors
#####################################################
###################### Analysis for Dec 1st, 2018
#####################################################
Dec_1_data <- ped_data %>% filter(Date == "2018-12-01")
Dec_1_data%>% ggplot(aes(x=Time,y= Count, group = Sensor)) +
  geom_point(color = "black")+
  geom_line(color = "black") ->p

Dec_1_data%>% select(Sensor, Time, Count) %>%
  spread(key = Sensor, value = Count) -> sensor_data

sensor_data <- sensor_data[,-1]
features_dec_1 <- extract_tsfeatures1(sensor_data) %>% as.data.frame()

selected_features<-features_dec_1%>% 
  select(moment3, mean, variance, linearity, spikiness, 
         BurstinessFF, maximum)

out<- stray::find_HDoutliers(selected_features, knnsearchtype = "brute" )
outlying_sensor<- colnames(sensor_data)[out$outliers]

out_Dec_1 <- Dec_1_data %>% filter(Sensor %in% outlying_sensor )
out_Dec_1 <- data.frame(out_Dec_1)
q2 <- p+ geom_point(data = out_Dec_1, aes(x=Time,y= Count), color= "red")+
  geom_line(data = out_Dec_1, aes(x=Time,y= Count), color= "red") 
#out$outliers

print(q2)

## ---- Dec1AnomSensorsFeatures
############ Calculating time series features for each sensor
Dec_1_data%>% select(Sensor, Time, Count) %>%
  spread(key = Sensor, value = Count) -> sensor_data

sensor_data <- sensor_data[,-1]
extract_tsfeatures1(sensor_data) %>% as.data.frame() ->features_dec_1

selected_features<-features_dec_1%>% 
  select(moment3, mean, variance, linearity, spikiness,
         BurstinessFF, maximum)

out<- stray::find_HDoutliers(selected_features, knnsearchtype = "brute" )
#out$outliers
selected_features <- selected_features%>%mutate(Type = out$type)

p <- GGally::ggpairs(selected_features,
                     columns = 1:7,upper = "blank" ,
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

f2<- p + theme(aspect.ratio = 1 )
## ---- FeaturePlotDec1st
print(f2)


## ---- Dec26AnomSensors
#####################################################
###################### Analysis for Dec 26th, 2018
#####################################################
Dec_1_data <- ped_data %>% filter(Date == "2018-12-26")
Dec_1_data%>% ggplot(aes(x=Time,y= Count, group = Sensor)) +
  geom_point(color = "black")+
  geom_line(color = "black") ->p

Dec_1_data%>% select(Sensor, Time, Count) %>%
  spread(key = Sensor, value = Count) -> sensor_data

sensor_data <- sensor_data[,-1]
features_dec_1 <- extract_tsfeatures1(sensor_data) %>% as.data.frame()

selected_features<-features_dec_1%>% 
  select(moment3, mean, variance, linearity, spikiness, 
         BurstinessFF, maximum)

out<- stray::find_HDoutliers(selected_features, knnsearchtype = "brute" )
outlying_sensor<- colnames(sensor_data)[out$outliers]

out_Dec_1 <- Dec_1_data %>% filter(Sensor %in% outlying_sensor )
out_Dec_1 <- data.frame(out_Dec_1)
q2 <- p+ geom_point(data = out_Dec_1, aes(x=Time,y= Count), color= "red")+
  geom_line(data = out_Dec_1, aes(x=Time,y= Count), color= "red") 
#out$outliers

print(q2)

## ---- Dec1AnomSensorsFeatures
############ Calculating time series features for each sensor
Dec_1_data%>% select(Sensor, Time, Count) %>%
  spread(key = Sensor, value = Count) -> sensor_data

sensor_data <- sensor_data[,-1]
extract_tsfeatures1(sensor_data) %>% as.data.frame() ->features_dec_1

selected_features<-features_dec_1%>% 
  select(moment3, mean, variance, linearity, spikiness,
         BurstinessFF, maximum)

out<- stray::find_HDoutliers(selected_features, knnsearchtype = "brute" )
#out$outliers
selected_features <- selected_features%>%mutate(Type = out$type)

p <- GGally::ggpairs(selected_features,
                     columns = 1:7,upper = "blank" ,
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

f2<- p 
#+ theme(aspect.ratio = 1 )
## ---- FeaturePlotDec1st
print(f2)

## ---- HighDimExample
