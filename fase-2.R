data <- read.csv("F:\\Downloads\\archive\\data.csv", header=TRUE, stringsAsFactors=FALSE)
head(data)

boxplot(data$JP_Sales~data$Genre)

library(dplyr)
group_by(data, Genre) %>%
  summarise(
    count = n(),
    mean = mean(JP_Sales, na.rm = TRUE),
    sd = sd(JP_Sales, na.rm = TRUE),
    median = median(JP_Sales, na.rm = TRUE),
    IQR = IQR(JP_Sales, na.rm = TRUE)
  )

kruskal.test(JP_Sales ~ Genre, data = data)

pairwise.wilcox.test(data$JP_Sales, data$Genre,
                     p.adjust.method = "BH")