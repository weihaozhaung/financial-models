rm(list=ls());gc()
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(rlang)


data = read.csv(file = "all_stocks_2006-01-01_to_2018-01-01.csv")

data$Date = as.Date(data$Date, format = '%Y-%m-%d')

data$return = c(NA, diff(log(data$Close), 1))
data=data[complete.cases(data),]

t = quantile(data$return, 0.05)
T= as.numeric(data[data$return<=t, 'Date'] - as.Date('2006-01-01'))


pstart = c(mu = 1, C = 1, a = 1)
ppm = ptproc(pts = T, cond.int = hawkes.cond.int, params = pstart)
condition(ppm) = penalty(code = NULL, condition = quote(any(params < 0)))
model = ptproc.fit(ppm, optim.control = list(trace = 2), alpha = 1e+5, hessian = TRUE)


model.sum=summary(model)
fitted.value=model.sum$ppobj
Branch=fitted.value$params[3]/fitted.value$params[2]


# x = as.numeric(data$Date - as.Date('2006-01-01'))
# e = evalCIF(model, xpts = x)
