
library(tidyverse)
library(data.table)
library(scales)
library(spatstat)
library(stringr)
library(zoo)
library(margins)
library(lme4)
library(purrr)
library(broom)
library(lubridate)
library(seasonal)
library(ggfortify)
library(seasonalview)

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(ggthemes)
library(ggrepel)
library(directlabels)
library(RColorBrewer)
library(wesanderson)
library(viridis)
library(fastDummies)
library(pracma)

library(fredr)
fredr_set_key("92dc6162fdfbda560283f539f4f7667f")
library(ipumsr)
library(openxlsx)
library(Rcpp)
library(fastmap)
library(plotly)
# devtools::install_github("mibarnes/ggTHP")
# library(ggTHP)
# brookings_view_palette("THP_Categorical_2")

library(openxlsx)
library(Rcpp)
setwd("C:/Users/sestep/Documents/jolts/data")


path <- "C:/Users/sestep/Documents/jolts/data/"



###############################################
####### Job Openings and Turnover (JT) ########
###############################################


download.file(url = "https://download.bls.gov/pub/time.series/jt/jt.data.1.AllItems", 
              destfile = paste(path, "JOLTS/JOLTS_AllItems.csv", sep=""))

download.file(url = "https://download.bls.gov/pub/time.series/jt/jt.data.0.Current", 
              destfile = paste(path, "JOLTS/JOLTS_Current.csv", sep=""))


download.file(url = "https://download.bls.gov/pub/time.series/jt/jt.series", 
              destfile = paste(path, "JOLTS/JOLTS_Series.csv", sep=""))

download.file(url = "https://download.bls.gov/pub/time.series/jt/jt.dataelement", 
              destfile = paste(path, "JOLTS/JOLTS_DataElements.csv", sep=""))

download.file(url = "https://download.bls.gov/pub/time.series/jt/jt.industry", 
              destfile = paste(path, "JOLTS/JOLTS_Industries.csv", sep=""))

download.file(url = "https://download.bls.gov/pub/time.series/jt/jt.sizeclass", 
              destfile = paste(path, "JOLTS/JOLTS_Sizes.csv", sep=""))

download.file(url = "https://download.bls.gov/pub/time.series/jt/jt.state", 
              destfile = paste(path, "JOLTS/JOLTS_States.csv", sep=""))



jolts.allitems <- read.csv(paste(path, "JOLTS/JOLTS_AllItems.csv", sep=""), header = TRUE, sep = "\t")

jolts.current <- read.csv(paste(path, "JOLTS/JOLTS_Current.csv", sep=""), header = TRUE, sep = "\t")

jolts.series <- read.csv(paste(path, "JOLTS/JOLTS_Series.csv", sep=""), header = TRUE, sep = "\t")

jolts.elements <- read.csv(paste(path, "JOLTS/JOLTS_DataElements.csv", sep=""), header = TRUE, sep = "\t")

jolts.industries <- read.csv(paste(path, "JOLTS/JOLTS_Industries.csv", sep=""), header = TRUE, sep = "\t")

jolts.sizes <- read.csv(paste(path, "JOLTS/JOLTS_Sizes.csv", sep=""), header = TRUE, sep = "\t")

jolts.states <- read.csv(paste(path, "JOLTS/JOLTS_States.csv", sep=""), header = TRUE, sep = "\t")




jolts.df <- jolts.allitems %>%
  left_join(jolts.series, by = c("series_id")) %>%
  left_join(jolts.elements, by = c("dataelement_code")) %>%
  left_join(jolts.industries, by = c("industry_code")) %>%
  left_join(jolts.sizes, by = c("sizeclass_code")) %>%
  left_join(jolts.states, by = c("state_code")) %>%
  select(series_id, year, period, value, seasonal, state_code, state_text, ratelevel_code,
         dataelement_text, dataelement_code, industry_text, industry_code, sizeclass_code, sizeclass_text) %>%
  mutate(month = substr(period, 2, 3)) %>%  
  filter(!month==13) %>%
  mutate(Date = as.Date(with(., paste(year, month, 1, sep="-")), "%Y-%m-%d")) %>% 
  filter(seasonal == "S",
         #year %in% c(2019, 2020, 2021),
         !industry_code %in% c("0","300000","400000","520000","530000","600000","920000","923000","929000")) %>%
  #filter(dataelement_code %in% c("JO")) %>%
  filter(ratelevel_code %in% c("L")) %>%
  filter(state_code %in% c("00")) %>%
  mutate(Series = ifelse(industry_text == "Total private", sizeclass_text, industry_text)) %>%
  mutate(Level = value) %>%
  mutate(Group = case_when(
    Series %in% c("All size classes") ~ "All",
    Series %in% c("1 to 9 employees","10 to 49 employees","50 to 249 employees","250 to 999 employees",
                  "1,000 to 4,999 employees","5,000 or more employees") ~ "Sizes",
    
    Series %in% c("Mining and logging","Construction","Durable goods manufacturing","Nondurable goods manufacturing") ~ "Goods",
    
    Series %in% c("Wholesale trade","Retail trade","Transportation, warehousing, and utilities") ~ "Trade Services",
    
    Series %in% c("Information","Financial activities","Professional and business services","Educational services","Health care and social assistance") ~ "Pro Services",
    
    Series %in% c("Leisure and hospitality","Arts, entertainment, and recreation","Accommodation and food services",
                  "Other services","Government", "Federal") ~ "Other Services",
    
    TRUE ~ "NA"))


jolts.df$dataelement_text <- factor(jolts.df$dataelement_text, 
                                    levels=c("Job openings","Hires","Total separations","Quits","Layoffs and discharges","Other separations"),
                                    labels=c("Job openings","Hires","Total separations","Quits","Layoffs","Other separations"))


jolts.df$Series <- factor(jolts.df$Series, 
                                    levels=c("All size classes",
                                             "1 to 9 employees","10 to 49 employees","50 to 249 employees","250 to 999 employees",
                                             "1,000 to 4,999 employees","5,000 or more employees",
                                             
                                             "Mining and logging","Construction","Durable goods manufacturing","Nondurable goods manufacturing",
                                             
                                             "Wholesale trade","Retail trade","Transportation, warehousing, and utilities",
                                             
                                             "Information","Financial activities","Professional and business services","Educational services","Health care and social assistance",
                                             
                                             "Leisure and hospitality","Arts, entertainment, and recreation","Accommodation and food services",
                                             "Other services","Government", "Federal"),
                                    
                                    labels=c("All",
                                             "1 to 9 employees","10 to 49 employees","50 to 249 employees","250 to 999 employees",
                                             "1,000 to 4,999 employees","5,000+",
                                             
                                             "Mining and logging","Construction","Durables manufacturing","Nondurables manufacturing",
                                             
                                             "Wholesale trade","Retail trade","Transportation, warehousing, and utilities",
                                             
                                             "Information","Financial activities","Professional and business services","Educational services","Health care and social assistance",
                                             
                                             "Leisure and hospitality","Arts, entertainment, and recreation","Accommodation and food services",
                                             "Other services","Government", "Federal"))


jolts.df <- jolts.df %>% filter(!Series %in% c("Federal"))




#=======================================#
#   TRIM Overall Openings and Quits
#=======================================#

jolts.trim <- jolts.df %>%
  mutate(month = as.numeric(as.character(substr(period,2,3)))) %>%
  mutate(Date = as.Date(with(., paste(year, month, 1, sep="-")), "%Y-%m-%d")) %>%
  filter(ratelevel_code %in% c("L")) %>%
  filter(seasonal %in% c("S"))%>% 
  # filter(state_code %in% c("00"))%>%
  # filter(industry_code %in% c("0")) %>%
  filter(sizeclass_code %in% c("00")) %>%
  filter(dataelement_text %in% c("Job openings")) %>%
  select(Date, value, "Measure"="dataelement_text")%>%
  spread(key="Measure", value="value")  %>%
  rename("Openings"="Job openings")



write.csv(jolts.trim, file = "C:/Users/sestep/Documents/jolts/data/JOLTS_levels.csv", row.names=FALSE)


#=======================================#
#   TRIM Openings and Quits by Industry
#=======================================#

jolts.trim <- jolts.df %>%
  mutate(month = as.numeric(as.character(substr(period,2,3)))) %>%
  mutate(Date = as.Date(with(., paste(year, month, 1, sep="-")), "%Y-%m-%d")) %>%
  filter(ratelevel_code %in% c("R")) %>%
  filter(seasonal %in% c("S")) %>%
  filter(state_code %in% c("00")) %>%
  filter(!industry_code %in% c("0")) %>%
  filter(sizeclass_code %in% c("0")) %>%
  filter(Date > as.Date("2020-01-01")) %>%
  filter(dataelement_text %in% c("Total separations"))


jolts.max <- jolts.trim %>%
  group_by(industry_text) %>%
  filter(value == max(value)) %>%
  select(industry_text, industry_code, "Max_Date"=Date, "Max_Val"=value) %>%
  filter(!industry_text %in% c("Durable goods manufacturing","Nondurable goods manufacturing", "Government",
                               "State and local government education","State and local government, excluding education"))  %>%
  mutate(Max_Val2 = ifelse(industry_text=="Wholesale trade" & Max_Date=="2020-04-01", 0, Max_Val)) %>% filter(!Max_Val2==0) %>% 
  select(industry_text, Max_Date, "Max_Val"=Max_Val2)   ### Removes a duplicate max of Wholesale


jolts.combine <- jolts.trim %>%
  left_join(jolts.max, by = c()) %>%
  select(industry_text, Max_Date, Max_Val2)




#=============================================#
#           LOOP - PLOT DATA FIGURES
#=============================================#
setwd("C:/Users/sestep/Documents/jolts/data/")

jolts.trim <- jolts.df %>% filter(Group %in% c("Sizes", "Goods", "Trade Services", "Pro Services", "Other Services"))
GROUP_list = unique(jolts.trim$Group)
GROUP_num = length(unique(jolts.trim$Group))


plot_list = list()
for (i in 1:GROUP_num) {
  p = 
    jolts.df %>%
    #  filter(industry_text %in% c("Total private", GROUP_list[[i]])) %>%
    filter(Group %in% GROUP_list[[i]] | Group=="All") %>%
    ggplot()+
    # add_rec_shade() +
    geom_line(aes(x=Date, y=Rate, group = Series, color = Series, linetype = Series)) +
    scale_x_date(name = "", limits = as.Date(c("2019-01-01","2021-12-31")), date_breaks  ="3 month", date_labels = "%b-%y", expand = c(0,0)) +
    #scale_x_date(name = "", limits = as.Date(c("2001-03-01","2021-12-31")), expand = c(0,0)) +
    #scale_y_continuous("", breaks = seq(0,35,2.5), labels = comma) +
    scale_y_continuous("", labels = percent) +
    labs(title = paste("JOLTS Breakout:\nGroup - ", GROUP_list[[i]],sep="")) +
    
    scale_linetype_manual(values = c(2, 1,1,1,1,1,1)) +
    # THP_theme + #scale_color_brookings("THP_Categorical_2") +
    scale_color_manual(values = c("#6E2585", "#69BE28","#FF6E00","#00ADD0","#006983","#FFB612","red")) +
    
    # THP_theme + 
    facet_wrap(~dataelement_text, ncol = 1, strip.position="right", scales = "free_y") 
  
  plot_list[[i]] = p
}


pdf("plots_JOLTS_Recent.pdf")
for (i in 1:GROUP_num) {
  print(plot_list[[i]])
}
dev.off()










