library(shiny)
library(ggplot2)
library(reshape2)
library(corrplot)
library(RColorBrewer)
library(d3heatmap)

#set maximum file size
options(shiny.maxRequestSize=500*1024^2) #500Mb

#setup a color pallete choice on main dashboard or per tool?
cols<-brewer.pal(9,"Set1")

#test table
test<-data.frame(A=seq(1,10),B=seq(1,40,4),C=seq(1,100,10))