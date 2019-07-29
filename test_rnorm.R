library(dplyr)
library(ggplot2)
############################# test1
x1 <- seq(1, 5, length=50)
y1 <- dnorm(x1, mean=3, sd=3)
z1 <- "p1"
df1 <- data.frame(x1,y1,z1)
colnames(df1) <- c("x","y","z")
#
x2 <- seq(3, 7, length=50)
y2 <- dnorm(x2, mean=5, sd=3)
z2 <- "p2"
df2 <- data.frame(x2,y2,z2)
colnames(df2) <- c("x","y","z")
#
x3 <- seq(5, 9, length=50)
y3 <- dnorm(x3, mean=7, sd=3)
z3 <- "p3"
df3 <- data.frame(x3,y3,z3)
colnames(df3) <- c("x","y","z")
#
df <- rbind(df1,df2,df3)
#
ggplot(df,aes(x,y,color = z)) +
  geom_line() +
  theme_bw()
############################# test2
x1 <- seq(0, 100, length=50)
y1 <- dnorm(x1, mean=10, sd=10)
z1 <- "p1"
df1 <- data.frame(x1,y1,z1)
colnames(df1) <- c("x","y","z")
#
x2 <- seq(0, 100, length=50)
y2 <- dnorm(x2, mean=50, sd=10)
z2 <- "p2"
df2 <- data.frame(x2,y2,z2)
colnames(df2) <- c("x","y","z")
#
x3 <- seq(0, 100, length=50)
y3 <- dnorm(x3, mean=70, sd=10)
z3 <- "p3"
df3 <- data.frame(x3,y3,z3)
colnames(df3) <- c("x","y","z")
#
df <- rbind(df1,df2,df3)
#
ggplot(df,aes(x,y,color = z)) +
  geom_line() +
  theme_bw()
#
df_mean <- df %>%
  dplyr::group_by(x)%>%
  dplyr::mutate(m = mean(y)) %>%
  dplyr::ungroup()
#  
ggplot(df_mean,aes(x,y,color = z)) +
  geom_line() +
  geom_line(aes(x,m),color ="black")+
  theme_bw()
#
library(mgcv)
gamm.res <- mgcv::gamm(y ~ s(x), data = df_mean,random=list(z=~1),method ="REML",correlation = corAR1())
df_mean$gamm <- predict(gamm.res$gam)
#
ggplot(df_mean,aes(x,y,color = z)) +
  geom_line() +
  geom_line(aes(x,m),color ="black")+
  geom_line(aes(x,gamm),color ="red")+
  theme_bw()
#
#######################################################
df_test <- NULL
for (i in 1:30){
  x <- seq(0, 100, length=50)
  y <- dnorm(x, mean=runif(1, min = 10, max = 90), sd=10)
  z <- paste0("p",i)
  df <- data.frame(x,y,z)
  df_test <- rbind(df_test,df)
}
#
df_mean <- df_test %>%
  dplyr::group_by(x)%>%
  dplyr::mutate(m = mean(y)) %>%
  dplyr::ungroup()
#
gamm.res <- mgcv::gamm(y ~ s(x), data = df_mean,random=list(z=~1),method ="REML",correlation = corAR1())
df_mean$gamm <- predict(gamm.res$gam)
#
ggplot(df_mean,aes(x,y,color = z)) +
  geom_line() +
  geom_line(aes(x,m),color ="black")+
  geom_line(aes(x,gamm),color ="red")+
  theme_bw()
#######################################################
df_test <- NULL
for (i in 1:300){
  x <- seq(0, 100, length=50)
  y <- dnorm(x, mean=runif(1, min = 10, max = 90), sd=runif(1, min = 1, max = 50))
  z <- paste0("p",i)
  df <- data.frame(x,y,z)
  df_test <- rbind(df_test,df)
}
#
df_mean <- df_test %>%
  dplyr::group_by(x)%>%
  dplyr::mutate(m = mean(y)) %>%
  dplyr::ungroup()
#
gamm.res <- mgcv::gamm(y ~ s(x), data = df_mean,random=list(z=~1),method ="REML",correlation = corAR1())
df_mean$gamm <- predict(gamm.res$gam)
#
ggplot(df_mean,aes(x,y,color = z)) +
  geom_line() +
  geom_line(aes(x,m),color ="black",size = 2)+
  geom_line(aes(x,gamm),color ="red",size = 2)+
  theme_bw() +
  scale_color_discrete(guide=FALSE)
#######################################################
df_test <- NULL
for (i in 1:30){
  x <- seq(0, 100, length=50)
  y <- dnorm(x, mean=runif(1, min = 10, max = 70), sd=runif(1, min = 1, max = 10))
  df1 <- data.frame(x,y)
  x <- seq(101, 200, length=50)
  y <- dnorm(x, mean=runif(1, min = 140, max = 190), sd=runif(1, min = 1, max = 10))  
  df2 <- data.frame(x,y)
  df <- rbind(df1,df2)
  df$z <- paste0("p",i)
  df_test <- rbind(df_test,df)
}
#
df_mean <- df_test %>%
  dplyr::group_by(x)%>%
  dplyr::mutate(m = mean(y)) %>%
  dplyr::ungroup()
#
gamm.res <- mgcv::gamm(y ~ s(x), data = df_mean,random=list(z=~1),method ="REML",correlation = corAR1())
df_mean$gamm <- predict.gam(gamm.res$gam)
#df_mean$gamm <- fitted(gamm.res$lme)
#
ggplot(df_mean,aes(x,y,color = z)) +
  geom_line() +
  geom_line(aes(x,m),color ="black",size = 2)+
  geom_line(aes(x,gamm),color ="red",size = 2)+
  theme_bw() +
  scale_color_discrete(guide=FALSE)

