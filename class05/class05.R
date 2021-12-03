#' ---
#' title: "Data Visualization"
#' output: github_document
#' ---


# Class 05: Data Visualization

# learning to use ggplot2

# load package
library(ggplot2)

# cars dataset is built in

# beginning of the dataset
head(cars)

# ggplot has: data + aes + geoms
ggplot(data = cars) +
  aes(x = speed, y = dist) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Stopping Distance of Old Cars",
       x = "Speed (MPH)",
       y = "Stopping Distance (ft)")

# can use other packages to make figures such as 'base' R
plot(cars$speed, cars$speed)

# importing RNA-seq data
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

# info about data
nrow(genes)
colnames(genes)
ncol(genes)

# quantity of each value in State column
table(genes$State)

# percentage of genes that are downregulated, unchanging, or upregulated
round(table(genes$State) / nrow(genes) * 100, 2)

# plotting genes data as scatterplot, color coded by State variable, and labeled
p <- ggplot(genes) + 
  aes(x = Condition1, y = Condition2, col = State) + 
  geom_point() + 
  labs(title = 'Gene Expression Changes Upon Drug Treatment',
       x = 'Control (no drug)', 
       y = 'Drug Treatment')
p

# changing colors to red, gray, blue
p + scale_colour_manual(values = c("blue", "gray", "red"))


# gapminder data
library(gapminder)
head(gapminder)

#plotting life expectancy by year
ggplot(gapminder) +
  aes(x = year, y = lifeExp, col = continent) + 
  geom_jitter(width = 0.3, alpha = 0.4) + 
  geom_violin(aes(group = year), alpha = 0.2, draw_quantiles = 0.5)

# makes interactive plot
# library(plotly)
# ggplotly()
