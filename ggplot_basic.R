# R plotting basics
library(gapminder)
library(ggplot2)

head(gapminder)
p<-ggplot(data=gapminder,mapping=aes(x=gdpPercap, lifeExp))
p+geom_point()
