lnk1L <- 'http://www.90minut.pl/liga/0/liga9323.html'
lnk2L <- 'http://www.90minut.pl/liga/0/liga9324.html'

#### 1.2 Scrap data ####

#### 1.2.1 II Liga ####

L2 <- read_html(lnk2L)
dataL2 <- L2 %>% html_nodes('table.main') %>% html_table(fill = T); dtL2 <- rbindlist(dataL2, fill = T)
dtL2 <- dtL2[X4 %like% ':' & X2 != '-', .(Liga = '2L', Gosp = X1, Gosc = X3, DataFrek = X4)]
dtL2[, Frek := gsub('\\)', '', gsub('\\(', '', str_extract(DataFrek, '\\((\\d+)\\)'))) %>% as.numeric()]
dtL2 <- dtL2[!is.na(Frek)]
dtL2[, DataFrek := NULL]

#### 1.2.1 I Liga ####

L1 <- read_html(lnk1L)
dataL1 <- L1 %>% html_nodes('table.main') %>% html_table(fill = T); dtL1 <- rbindlist(dataL1, fill = T)
dtL1 <- dtL1[X4 %like% ':' & X2 != '-', .(Liga = '1L', Gosp = X1, Gosc = X3, DataFrek = X4)]
dtL1[, Frek := gsub('\\)', '', gsub('\\(', '', str_extract(DataFrek, '\\((\\d+)\\)'))) %>% as.numeric()]
dtL1 <- dtL1[!is.na(Frek)]
dtL1[, DataFrek := NULL]