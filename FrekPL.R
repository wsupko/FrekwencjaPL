### Install (if not available already) required libraries ###

if (!'data.table' %in% installed.packages()) {install.packages('data.table')}
if (!'ggplot2' %in% installed.packages()) {install.packages('ggplot2')}
if (!'scales' %in% installed.packages()) {install.packages('scales')}
if (!'rvest' %in% installed.packages()) {install.packages('rvest')}
if (!'stringr' %in% installed.packages()) {install.packages('stringr')}

### Load required libraries ###

library(data.table)
library(RPostgreSQL)
library(ggplot2)
library(scales)
library(boot)
library(rvest)
library(stringr)

#### Database connection #### 


#### Custom variables #### 


    
#### Custom function ####

# cross-join 

CJ.table <- function(X,Y){
    setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]}


#### 1. Data Input ####

#### 1.1 Working data ####

dtTabela <- fread('./data/in/dtTabela.txt')
dtESA <- fread('./data/in/dtESA.txt')

source('scrapData.R')

frekData <- rbind(dtESA, dtL1, dtL2)

#### 2. Data wrangling ####

Tab.Stat <- frekData[, .(Median = median(Frek)), .(Liga, Club = Gosp)]
Tab.Stat <- merge(Tab.Stat, dtTabela, by = c('Liga', 'Club'), all.x = T)

#### 3. Modelling ####

Tab.Stat[,.N, Liga]
#### 4. Viz ####

Plot.Frek <- ggplot(Tab.Stat, aes(x = numPos, y = Median)) +
    geom_point(size = 3) + 
    geom_text(aes(label = Club), size = 3, vjust = -1)+
    annotate('text', x = 4.5, y = 35 * 10^3, label = "atop(bold('ESA Mistrzowska'))", vjust = 0.5, parse = T) +
    annotate('text', x = 12.5, y = 35 * 10^3, label = "atop(bold('ESA Spadkowa'))", vjust = 0.5, parse = T) +
    annotate('text', x = 25.5, y = 35 * 10^3, label = "atop(bold('Nice 1 Liga'))", vjust = 0.5, parse = T) +
    annotate('text', x = 43.5, y = 35 * 10^3, label = "atop(bold('II liga'))", vjust = 0.5, parse = T) +
    scale_y_log10(breaks = c(25 * 10^3, 15 * 10^3, 5 * 10^3, 10^3, 100),
                  labels = c(25 * 10^3, 15 * 10^3, 5 * 10^3, 10^3, 100),
                  limits = c(100, 35 * 10^3)) +
    scale_x_reverse(breaks = c(1, 8, 16, 16+18, 16 + 18 * 2), position = 'top') +
    labs(x = 'Pozycja w Polsce', y = 'Mediana frekwencji')+
    theme_minimal()+
    theme(panel.grid.minor = element_blank())


#### 4. Data Export ####

ggsave('./img/frekPL.png', Plot.Frek, width = 20, height = 8, units = 'in')
