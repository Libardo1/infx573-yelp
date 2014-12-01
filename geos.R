install.packages("mapproj")
install.packages("ggmap")
install.packages("DeducerSpatial")

require(maps)
require(ggmap)

df <- read.csv("vegas_aadt.csv")

library(stringr)

## from the example help
# ads <- unique(crime$address)[1:12]
# ads <- paste(ads, ', houston, texas', sep = '')
# ads <- str_trim(ads)
# gc <- geocode(ads)


df$addy <- paste(df$First.Street, df$Second.Street, sep = ' and ')

paid <- df$addy
paid <- paste(paid, ', las vegas, nevada', sep = '')
paid <- str_trim(paid)
gp <- geocode(paid)
