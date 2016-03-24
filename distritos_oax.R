library(readxl)
library(zoo)
library(stringr)

oax_dist <- read_excel("data/oaxaca - distritos2013.xlsx", sheet = 3, skip = 10, col_names = FALSE) %>%
  select(muncode = X0, mun_name = X3)

## create new vector with district name
oax_dist$dist_name = rep(NA, nrow(oax_dist))
oax_dist$dist_name <- ifelse(grepl("Distrito", oax_dist$muncode),  oax_dist$mun_name, NA)
oax_dist$dist_name <- na.locf(oax_dist$dist_name)
oax_dist$dist_name <- str_trim(oax_dist$dist_name)
oax_dist$dist_name <- str_sub(oax_dist$dist_name, 4)
oax_dist$dist_name <- str_trim(oax_dist$dist_name)

## clean municipio names
oax_dist <- filter(oax_dist, !is.na(mun_name))
oax_dist$mun_name <- str_trim(oax_dist$mun_name)

## some municipio names take two or more rows. This only fixes the ones with two rows. 
for(i in 1:nrow(oax_dist)) {
  
  if (is.na(oax_dist$muncode[i])) {
    
  oax_dist$mun_name[i-1] <- paste(oax_dist$mun_name[i-1], oax_dist$mun_name[i], sep = " ")
  
  }
}


#create table with municipality codes 
distritos <- oax_dist  %>%
  filter(!is.na(muncode)) %>%
  mutate(muncode = as.numeric(as.character(muncode)), 
         muncode = (muncode + 20000)) %>%
  slice(1:600)

##create vector with distrito codes
distritos$distrito = rep(NA, nrow(distritos))
distritos$distrito[is.na(distritos$muncode)] <- c(1:30+20000)
distritos$distrito <- na.locf(distritos$distrito)

#filter out NA rows 
distritos <- distritos %>%
  filter(!is.na(muncode)) 

#write.csv(distritos, "distritos_oaxaca.csv")

