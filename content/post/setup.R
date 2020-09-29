library(tidyverse)

install.packages("blogdown")

library(blogdown)

blogdown::new_site(
  dir = ".",
  install_hugo = TRUE,
  format = "toml",
  sample = TRUE,
  theme = "yihui/hugo-lithium",
  hostname = "github.com",
  theme_example = TRUE,
  empty_dirs = FALSE,
  to_yaml = TRUE,
  serve = interactive()
)

blogdown::serve_site()

install.packages("opendatatoronto")

library(opendatatoronto)

marriage_license_packages <- opendatatoronto::search_packages("Marriage Licence Statistics")

marriage_license_resources <- marriage_license_packages %>% 
  opendatatoronto::list_package_resources()

marriage_license_statistics <- marriage_license_resources %>%
  opendatatoronto::get_resource()

ggplot(data=marriage_license_statistics) +
  geom_point(mapping = aes(x=TIME_PERIOD, y=MARRIAGE_LICENSES, color=CIVIC_CENTRE)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

select_recent <- filter(marriage_license_statistics, TIME_PERIOD > "2017-12")

ggplot(data=select_recent) +
  geom_point(mapping = aes(x=TIME_PERIOD, y=MARRIAGE_LICENSES, color=CIVIC_CENTRE)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

agg_marriage_license = aggregate(x=marriage_license_statistics$MARRIAGE_LICENSES, 
                                 by=list(marriage_license_statistics$TIME_PERIOD), 
                                 FUN=sum)

agg_marriage_license = rename(agg_marriage_license, TIME_PERIOD=Group.1, TOTAL_MARRIAGE_LICENSES=x)

ggplot(data=agg_marriage_license, aes(x=TIME_PERIOD, y=TOTAL_MARRIAGE_LICENSES, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

agg_marriage_license$month <- substr(agg_marriage_license$TIME_PERIOD, 6, 7)
agg_marriage_license$year <- substr(agg_marriage_license$TIME_PERIOD, 0, 4)

agg_marriage_license_by_month = aggregate(x=agg_marriage_license$TOTAL_MARRIAGE_LICENSES, 
                                 by=list(agg_marriage_license$month), 
                                 FUN=mean)

agg_marriage_license_by_month = rename(agg_marriage_license_by_month, month=Group.1, AVG_MARRIAGE_LICENSES=x)

ggplot(data=agg_marriage_license_by_month, aes(x=month, y=AVG_MARRIAGE_LICENSES, group = 1)) +
  geom_line()

agg_marriage_license_by_year = aggregate(x=agg_marriage_license$TOTAL_MARRIAGE_LICENSES, 
                                          by=list(agg_marriage_license$year), 
                                          FUN=mean)

agg_marriage_license_by_year = rename(agg_marriage_license_by_year, year=Group.1, AVG_MARRIAGE_LICENSES=x)

ggplot(data=agg_marriage_license_by_year, aes(x=year, y=AVG_MARRIAGE_LICENSES, group = 1)) +
  geom_line()




