library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
library(car)
library(GGally)
library(skimr)
library(patchwork)
library(corrplot)

#import dataset from comm_merge file
eda_df<-read.csv("C:\\Users\\star-\\Data Science\\R stuff\\D.comm\\dcomm_merged.csv")
glimpse(eda_df)

#top five colleges by major
eda_df %>%
  slice_max(order_by = SASCDEG, n = 5) %>%
  select(INSTNM, SASCDEG)
eda_df %>%
  slice_max(order_by = SBASDEG, n = 5) %>%
  select(INSTNM, SBASDEG)
eda_df %>%
  slice_max(order_by = SMASDEG, n = 5) %>%
  select(INSTNM, SMASDEG)
eda_df %>%
  slice_max(order_by = SDOCDEG, n = 5) %>%
  select(INSTNM, SDOCDEG)
summary(eda_df$EFYTOTLT, na.rm= TRUE)
#select only the nessecary variables for the eda
df<- eda_df|>filter(CYACTIVE == 1 & PSEFLAG == 1, !is.na(SASCDEG), 
                    !is.na(SBASDEG),!is.na(SMASDEG), !is.na(SDOCDEG))|>
  select( 
    INSTNM, STABBR, SASCDEG, SBASDEG, SMASDEG, SDOCDEG,
    CHG2AY2,CHG2AY3,CHG3AY2, CHG3AY3, CHG4AY2,
    CHG4AY3, CHG5AY2, CHG5AY3, CHG6AY2, CHG6AY3,
    CHG7AY2, CHG7AY3, CHG8AY2, CHG8AY3, CHG9AY2, CHG9AY3, CYACTIVE, EFYTOTLT
    )
glimpse(df)
#############Visualization###########
#selections every variable in 2023
df2<- eda_df|>filter(CYACTIVE == 1 & PSEFLAG == 1, !is.na(SASCDEG), 
                     !is.na(SBASDEG),!is.na(SMASDEG), !is.na(SDOCDEG))|>
  select( 
    SASCDEG, SBASDEG, SMASDEG, SDOCDEG,CHG2AY2, CHG3AY2,
    CHG4AY2, CHG5AY2, CHG6AY2, CHG7AY2, CHG8AY2, CHG9AY2, EFYTOTLT, INSTNM, STABBR
  )
names(df2)<-c("Associates", "Bachelors", "Masters", "Doctoral",
              "In-State Tuition", "Out-of-State Tutition", "Supply Costs",
              "On-Campus Housing", "On-Campus Expenses", "Off-Campus Housing",
              "Off-Campus Expenses", "Family Expenses", "Student Count","Institution", "State")
summary(df2,na.rm= TRUE)
###basic univariate analysis###
#even with cleaning procedures using IQR the data is not normally distrbuted
clean_df<-remove_outlier(df2, c("Associates", "Bachelors", "Masters", "Doctoral"))
univariate(clean_df, "Associates")
univariate(clean_df, "Bachelors")
univariate(clean_df, "Masters")
univariate(clean_df, "Doctoral")

##ggplot univariate analysis
summary(df2$Associates)
summary(df2$Bachelors)
summary(df2$Masters)
summary(df2$Doctoral)

#stratified boxplot - 4 varialbes put into a factor to use ggplot
visual <- pivot_longer(df2, cols = c("Associates", "Bachelors", "Masters", "Doctoral"),
                       names_to = "Variable",
                       values_to = "Value")
nrow(visual)
visual$Variable<-factor(visual$Variable, levels = c("Associates", "Bachelors", "Masters", "Doctoral"))
p1<-ggplot(visual, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +coord_cartesian(ylim = c(0, 2000))+
  labs(title = "Zoomed in Boxplot for Degree Obtainment",
       x = "Degree Type",
       y = "Number of Students")
p2 <-ggplot(visual, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot()+
  labs(title = "Original Boxplot for Degree Obtainment",
       x = "Degree Type",
       y = "Number of Students")
p1+p2

#stratified barchart
p3<-ggplot(df2, aes(x = Associates)) +
  geom_histogram(bins = 50, fill = "#F8766D",color = "Black")+
  labs(title = "Histogram for Associates' Degree",
       x = "Associates",
       y = "Number of Students")+coord_cartesian(xlim = c(0,5000))
p4<-ggplot(df2, aes(x = Bachelors)) +
  geom_histogram( fill = "#7CAE00",bins = 50,color = "Black")+
  labs(title = "Histogram for Bachelor's Degree",
       x = "Bachelors",
       y = "Number of Students")+coord_cartesian(xlim = c(0,5000))
p5<-ggplot(df2, aes(x = Masters)) +
  geom_histogram(bins = 50, fill = "#00BFC4",color = "Black")+
  labs(title = "Histogram for Masters' Degree",
       x = "Masters",
       y = "Number of Students")+coord_cartesian(xlim = c(0,5000))
p6<-ggplot(df2, aes(x = Doctoral)) +
  geom_histogram(fill = "#C77CFF",color = "Black",bins = 50)+
  labs(title = "Histogram for Doctoral Degree",
       x = "Doctoral",
       y = "Number of Students")+coord_cartesian(xlim = c(0,5000))

(p3+p4)/(p5+p6)

###multivariate analysis
#df2$`In-State Tuition`
s1<-df2|>filter(`Bachelors`>0)|>ggplot(aes(x = Bachelors, y = `In-State Tuition`)) +
  geom_point()+geom_smooth()+
  labs(title = "Scatterplot of In-State Tuition by Bachelors Conferred",
       x = "Bachelors",
       y = "Cost of In-State Tuition")+coord_cartesian(xlim = c(0,15000))
#df2$`Out-of-State Tutition`
s2<-df2|>filter(Bachelors>0)|>ggplot(aes(x = Bachelors, y = `Out-of-State Tutition`)) +
  geom_point()+geom_smooth()+
  labs(title = "Scatterplot of Out of State Tuition by Bachelors Conferred",
       x = "Bachelors",
       y = "Cost of Out-of-State Tuition")+coord_cartesian(xlim = c(0,15000))
s1+s2 #creates plot for instate and outof state tuition
#pair plot for target variables
df2|> select("Associates", "Bachelors", "Masters", "Doctoral")|>
  ggpairs(df2,
          lower = list(continuous = wrap("smooth", 
                                         alpha = 0.5, color = "blue")),
          diag = list(continuous = "densityDiag"),
          upper = list(continuous = "cor"))

# Calculate the correlation matrix
cor_matrix <- cor(df2, use = "complete.obs")
# Plot a visual heatmap of the correlations
corrplot(cor_matrix, 
         method = "color", 
         type = "lower",  
         addCoef.col = "black", # Overlay the actual correlation numbers
         tl.col = "black",
         mar = c(0, 0, 1, 0),
         number.cex = .5)#reduces number size
title(main = "Correlation Plot", 
      line = -7,    # Higher numbers move it further up
      adj = 0.5)

# ggduo plots
library(GGally)
dv_plots <- map(1:4, function(dv_col) {
  # Extract the name of the current DV for the title
  dv_name <- names(df2)[dv_col] 
  
  ggduo(
    data = df2,
    columnsX = 5:12,
    columnsY = dv_col,
    mapping = aes(alpha = 0.1, color = "red")
  ) +
    theme_minimal() +coord_cartesian(ylim = c(0, 5500))+
    labs(title = paste("Predictor Variables vs.", dv_name))
})

# View the plots one by one by changing the index (1 through 4)
dv_plots[[1]]
dv_plots[[2]]
dv_plots[[3]]
dv_plots[[4]]
dv_plots
#write.csv(df2, "modeldf.csv")
#write_xlsx(df2,"modeldf.xlsx")
