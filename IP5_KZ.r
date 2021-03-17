library(datasets)
library(tidyverse)
library(psych)

df<-read.csv("Github repositories/IP5_KZ/imports-85.csv")

describe(df)

sapply(df, typeof)

make<-table(df$make)
make

fuel_type<-table(df$fuel_type)
fuel_type

aspiration<-table(df$aspiration)
aspiration

num_of_doors<-table(df$num_of_doors)
num_of_doors

body_style<-table(df$body_style)
body_style

drive_wheels<-table(df$drive_wheels)
drive_wheels

engine_location<-table(df$engine_location)
engine_location

engine_type<-table(df$engine_type)
engine_type

num_of_cylinders<-table(df$num_of_cylinders)
num_of_cylinders

fuel_system<-table(df$fuel_system)
fuel_system

numeric<-df[, c("symboling", "wheel_base", "length", "width", "height", "curb_weight", "engine_size", "compression_ratio", "city_mpg", "highway_mpg")]
numeric_cor<-round(cor(numeric),2)
numeric_cor

heatmap(numeric_cor, Colv = NA, Rowv = NA, scale='column')

hist(df$symboling, main='Symboling', xlab='Symboling')
hist(df$wheel_base, main='Wheel Base', xlab='Wheel Base')
hist(df$length, main='Length', xlab='Length')
hist(df$width, main='Width', xlab='Width')
hist(df$height, main='Height', xlab='Height')
hist(df$curb_weight, main='Curb Weight', xlab='Curb Weight')
hist(df$engine_size, main='Engine Size', xlab='Engine Size')
hist(df$compression_ratio, main='Compression Ratio', xlab='Compression Ratio')
hist(df$city_mpg, main='City MPG', xlab='City MPG')
hist(df$highway_mpg, main='Highway MPG', xlab='Highway MPG')

pairs(numeric, pch=19, col='blue', cex=0.5)

barplot(make, main='Make', horiz=TRUE, col='blue')
barplot(fuel_type, main='Fuel Type', horiz=TRUE, col='red')
barplot(aspiration, main='Aspiration', horiz=TRUE, col='green')
barplot(num_of_doors, main='Number of Doors', horiz=TRUE, col='purple')
barplot(body_style, main='Body Style', horiz=TRUE, col='blue')
barplot(drive_wheels, main='Drive Wheels', horiz=TRUE, col='red')
barplot(engine_location, main='Engine Location', horiz=TRUE, col='green')
barplot(enigne_type, main='Engine Type', horiz=TRUE, col='purple')
barplot(num_of_cylinders, main='Number of Cylinders', horiz=TRUE, col='blue')
barplot(fuel_system, main='Fuel System', horiz=TRUE, col='red') 

fit<-lm(city_mpg ~ engine_size, data=df)
plot(city_mpg ~ engine_size, data=df, pch=16, cex=0.5, col='blue')
abline(fit)
