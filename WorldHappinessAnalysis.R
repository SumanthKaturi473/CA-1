####Loading packages
packages = c('sf', 'tmap', 'tidyverse','plotly','ggthemes','heatmaply','RColorBrewer')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

#Loading Dataset(World-happiness.csv)
happiness<- read.csv("World-happiness.csv")
str(happiness)

# Rename the country column name
names(happiness)[names(happiness) == "ï..Country.name"] <- "Country.name"

# Remove NA values
library(dplyr)

happiness_df <- happiness %>% drop_na(Country.name,year,Life.Ladder,
                Log.GDP.per.capita,Social.support,Healthy.life.expectancy.at.birth,
                Freedom.to.make.life.choices,Generosity,Perceptions.of.corruption,
                Positive.affect,Negative.affect)


# Correlation plot on the happiness data
happiness_corr <- as.data.frame(happiness_df)

heatmaply_cor(
  cor(happiness_corr[, 3:9]),
  xlab = "Features", 
  ylab = "Features",
  colors = colorRampPalette(brewer.pal(3, "Spectral"))(256),
  k_col = 2, 
  k_row = 2
)

# Histogram of Positive affect
hist(happiness_df$Positive.affect, main = "Histogram of Positive affect", xlab = "Positive affect")

# Histogram of Negative affect
hist(happiness_df$Negative.affect, main = "Histogram of Negative affect", xlab = "Negative affect")

# Changes of various factors over the years.
factors<- ggplot(happiness_df, aes(x=year)) + 
  stat_summary(aes(y = Log.GDP.per.capita, 
                   colour= "Log.GDP.per.capita"), 
               geom = "line", 
               fun = "mean", 
               size = 1.5) +
  stat_summary(aes(y = Social.support, 
                   colour= "Social Support"), 
               geom = "line", 
               fun = "mean", 
               size = 1.5) + 
  stat_summary(aes(y = Healthy.life.expectancy.at.birth, 
                   colour= "Health"), 
               geom = "line", 
               fun = "mean", 
               size = 1.5) + 
  stat_summary(aes(y = Freedom.to.make.life.choices, 
                   colour= "Freedom"), 
               geom = "line", 
               fun = "mean", 
               size = 1.5) +
  stat_summary(aes(y = Perceptions.of.corruption, 
                   colour= "Perceptions of Corruption"), 
               geom = "line", 
               fun = "mean", 
               size = 1.5) + 
  stat_summary(aes(y = Generosity, 
                   colour= "Generosity"), 
               geom = "line", 
               fun = "mean", 
               size = 1.5) + 
  labs(x = "Year", 
       y = "Avg. Factor Score", 
       size = 2, 
       colour="") + 
  scale_colour_manual(values=c("steelblue4", 
                               "orange1", 
                               "gold1", 
                               "forestgreen", 
                               "mediumorchid4", 
                               "lightcoral"))

fig_f <- ggplotly(factors)
fig_f


# Generosity Vs Social.support
p6<-ggplot(happiness_df,aes(x=Generosity,y=Social.support,
                            color=Country.name))+geom_point(alpha=0.6)
fig6 <- ggplotly(p6)
fig6

# Log GDP per capita vs Healthy Life Expectancy
p <- ggplot(happiness_df, 
            aes(x = Log.GDP.per.capita, y=Healthy.life.expectancy.at.birth, 
                colour =Country.name ,text = paste("country:", Country.name))) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_colour_brewer(type = "seq", palette = "Spectral") + 
  scale_size(range = c(2, 12)) +
  scale_x_log10()+
  theme_minimal()+
  labs(x = "Log GDP per capita", y = "Life expectancy at birth")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

fig <- ggplotly(p)
fig

res1<- cor.test(happiness_df$Log.GDP.per.capita, happiness_df$Healthy.life.expectancy.at.birth, method = "spearman")
res1

# Log GDP per capita Vs Perceptions.of.corruption
p2<-ggplot(happiness_df,aes(x=Log.GDP.per.capita,y=Perceptions.of.corruption,
        color=Country.name,size=Perceptions.of.corruption))+geom_point(alpha=0.6)
fig2 <- ggplotly(p2)

fig2

res2<- cor.test(happiness_df$Log.GDP.per.capita, happiness_df$Perceptions.of.corruption, method = "spearman")
res2

# Positive.affect Vs Social.support
p3<-ggplot(happiness_df,aes(x=Positive.affect,y=Social.support,
                            color=Country.name,size=Social.support))+geom_point(alpha=0.6)
fig3 <- ggplotly(p3)
fig3

res3<- cor.test(happiness_df$Positive.affect, happiness_df$Social.support, method = "spearman")
res3

# Negative.affect Vs Social.support
p4<-ggplot(happiness_df,aes(x=Negative.affect,y=Social.support,
                            color=Country.name,size=Social.support))+geom_point(alpha=0.6)
fig4 <- ggplotly(p4)
fig4

res4<- cor.test(happiness_df$Negative.affect, happiness_df$Social.support, method = "spearman")
res4

# Positive.affect Vs Freedom.to.make.life.choices
p5<-ggplot(happiness_df,aes(x=Positive.affect,y=Freedom.to.make.life.choices))+geom_point(alpha=0.6)
fig5<-ggplotly(p5)
fig5

res5<- cor.test(happiness_df$Positive.affect, happiness_df$Freedom.to.make.life.choices, method = "spearman")
res5

