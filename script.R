install.packages("pacman")
library("pacman")

# packages commonly needed for SWMP data
# checks to see if a package is installed, if not it attempts to install the package and then loads it
pacman::p_load(SWMPr, SWMPrExtension, lubridate, tidyverse)

##Feed in our BA Nut data
barkers <- import_local("./data", "lksbanut", trace = TRUE)

##Feed in our BA Nut data
blatnik<- import_local("./data", "lksblnut", trace = TRUE)

##Feed in our BA Nut data
oliver <- import_local("./data", "lksolnut", trace = TRUE)

##Feed in our BA Nut data
pokegama <- import_local("./data", "lksponut", trace = TRUE)

nut <- bind_rows(lst(barkers, blatnik, oliver, pokegama), .id = "id")

#the qaqc code for below MDL is <-4> [SBL]

#calc total number of above zero no2f readings
#filter out no2 and no3 samples collected that are not rejected -3, suspect 1, or missing
no2_no <- nut %>%
  filter(!grepl('<-3>', f_no2f))  %>%
  filter(!grepl('<1>', f_no2f)) %>%
  filter(!grepl('<-2>', f_no2f)) %>%
  filter(no2f > 0) %>%
  nrow()

#calculate number of values below mdl
mdl_no <- nut %>%
  filter(!grepl('<-3>', f_no2f))  %>%
  filter(!grepl('<1>', f_no2f)) %>%
  filter(!grepl('<-2>', f_no2f)) %>%
  filter(grepl('<-4>', f_no2f)) %>%
  nrow()

#calculate percentage of reading below mdl
percent_mdl <- 100*(mdl_no[1]/no2_no[1])

#48.5% of nitrite samples (not flagged rejected or suspect) are below MDL. 
#how are <mdls distributed among the 4 sites?
#counts by sites
count_by_id <- nut %>%
  filter(!grepl('<-3>', f_no2f))  %>%
  filter(!grepl('<1>', f_no2f)) %>%
  filter(!grepl('<-2>', f_no2f)) %>%
  count(id)

#counts by site below mdl
mdl_count_by_id <- nut %>%
  filter(!grepl('<-3>', f_no2f))  %>%
  filter(!grepl('<1>', f_no2f)) %>%
  filter(!grepl('<-2>', f_no2f)) %>%
  filter(grepl('<-4>', f_no2f)) %>%
  count(id)
colnames(mdl_count_by_id) <- c("id", "below_mdl")

by_id<-merge(count_by_id, mdl_count_by_id)                
by_id %>%
  mutate(percent_below_mdl = (below_mdl/n)*100)

#more samples at barkers are because of diel samples
#now let's look at the values above MDL and how they are distributed by site

gdata <- nut %>%
  filter(!grepl('<-3>', f_no2f))  %>%
  filter(!grepl('<1>', f_no2f)) %>%
  filter(!grepl('<-2>', f_no2f)) %>%
  filter(!grepl('<-4>', f_no2f))

g <- ggplot(gdata, aes(x=id, y=no2f)) + geom_boxplot()

#added n for each site in red
g <- g  + annotate("text", x = 1:length(table(nut$id)),
             aggregate(no2f ~ id, nut, median) [,2],
             label = table(gdata$id),
             col = "red",
             vjust = -.8)+
  labs(y = "nitrite mg/L") +
  theme(axis.title.x = element_blank())

g
#nitrite at barkers and blatnik generally are higher than oliver and pokegama
ggsave("outputs/no2_distribution_bysite.png", plot = g, height = 2, width = 2.5, units = c("in"))


#now let's look at the proportion of NO23 that is NO2
#first filter out suspect, missing, and rejected data for other parameters
nut_qa <-nut %>%
  filter(!grepl('<-3>', f_no3f))  %>%
  filter(!grepl('<1>', f_no3f)) %>%
  filter(!grepl('<-2>', f_no3f)) %>%  
  filter(!grepl('<-3>', f_no23f))  %>%
  filter(!grepl('<1>', f_no23f)) %>%
  filter(!grepl('<-2>', f_no23f))%>%
  filter(!grepl('<-3>', f_no2f))  %>%
  filter(!grepl('<1>', f_no2f)) %>%
  filter(!grepl('<-2>', f_no2f))%>%
#and filter out the below MDL values
  filter(!grepl('<-4>', f_no2f)) %>%
  filter(!grepl('<-4>', f_no3f)) %>%
  filter(!grepl('<-4>', f_no23f)) %>%
  drop_na(no2f) %>%
  drop_na(no3f) %>%
  drop_na(no23f)

ggplot(nut_qa, aes(x=id, y=no2f)) + geom_boxplot()
g2 <- ggplot(nut_qa, aes(x=id, y=no3f)) + geom_boxplot()
ggsave("outputs/no3_distribution_bysite.png", plot = g2)

nut_qa <- nut_qa %>%
  mutate(no2_proportion = no2f/no3f)

no2prop <- mean(nut_qa$no2_proportion)

g3 <- ggplot(nut_qa, aes(x=id, y=no2_proportion)) + geom_boxplot()+
  labs(y = "proportion nitrite to nitrate") +
  theme(axis.title.x = element_blank())
ggsave("outputs/no2_proportion_bysite.png", plot = g3, height = 2, width = 2.5, units = c("in"))


