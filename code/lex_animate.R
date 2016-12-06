##https://gist.github.com/walkerke/bb891e729e76c5a68bde719f4b0db516
#install.packages("idbr")
#install.packages("countrycode")
#install.packages("gganimate")
#devtools::install_github("dgrtwo/gganimate")
#install.packages("tweenr")

library(idbr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(countrycode)
library(gganimate)  # https://github.com/dgrtwo/gganimate
library(tweenr)

ctrycode <- countrycode_data

#names(ctrycode)
NorthernAfrica <- subset(ctrycode, (ctrycode$region == "Northern Africa") )
NorthernAfrica <- NorthernAfrica[ ,c("fips104")]

#ctrys <- countrycode(c('South Africa', 'Botswana', 'Lesotho', 'Namibia', 'Zimbabwe', 'Swaziland'), 'country.name', 'fips104')
MiddleEast <- countrycode(c('Jordan', 'Lebanon', 'Syria', 'Israel', 'Iraq', 'Kuwait', 'Oman', 'Qatar', 'Saudi Arabia', 'Yemen',
                            'United Arab Emirates'), 'country.name', 'fips104')

ctrys <- NorthernAfrica

# https://github.com/walkerke/idbr
# http://api.census.gov/data/key_signup.html 
# http://www.census.gov/data/developers/data-sets/international-database.html

#idb_api_key("Your API key here")

idb_api_key("1c42997275e3033bb5dcfcca179c54633dcfdff1")

#idb_concepts()
#idb_variables()

full <- idb5(country = ctrys, year = 1985:2016,  variables = c('E0_F', 'E0_M'), country_name = TRUE)

df2 <- full %>%
  mutate(diff = E0_F - E0_M, ease = 'cubic-in-out') %>%
  select(-FIPS) %>%
  rename(Male = E0_M, Female = E0_F)

dft <- tween_elements(df2, time = 'time', group = 'NAME', ease = 'ease', 
                      nframes = 500)


dft <- dft %>%
  gather(Sex, value, Male, Female, -diff, -.group) 

g <- ggplot() + 
  geom_point(data = dft, aes(x = value, y = .group, color = Sex, frame = .frame),   size = 14) + 
  scale_color_manual(values = c('darkred', 'navy')) + 
  geom_text(data = dft, aes(x = value, y = .group, frame = .frame, label = as.character(round(dft$value, 1))),  color = 'white', fontface = 'bold') +
  geom_text(data = dft, aes(x = 28.5, y = 1.5, frame = .frame, label = round(dft$time, 0)), color = 'black', size = 12) + 
  theme_minimal(base_size = 16) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + 
  labs(y = '',  x = '', caption = 'Data source: US Census Bureau IDB via the idbr R package', 
       title = 'Life expectancy at birth in Northern Africa, 1985-2016')

g1 <- gg_animate(g, interval = 0.05, ani.width = 1000, ani.height = 900, title_frame = FALSE)
g1 %>% gg_animate_save("out/output.gif")

#g1

#gg_animate_save(g1, filename = "out/gganimate.gif", saver = "gif")
#gg_animate_save(g1, filename = "out/gganimate.mp4", saver = "mp4")
#gg_animate(g1, "out/gganimate.swf")
#gg_animate(g1, "out/gganimate.html")

