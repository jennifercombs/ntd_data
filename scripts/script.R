##### 01 load libraries #####
library(tidyverse)

##### 02 load data #####
master <- read_csv("data/February 2020 Adjusted Database Master.csv")
upt <- read_csv("data/February 2020 Adjusted Database UPT 2.csv")

##### 03 create mode-specific tables #####
#### bus ####
master_mb <- master %>% 
  filter(Mode == "MB" & 
           Active == "Active" & 
           `Reporter Type` == "Full Reporter" & 
           `Most Recent Report Year` == 2018) %>% 
  mutate(fares = as.numeric(str_remove(`Average Fares per Trip FY`, "[$]")),
         cost = as.numeric(str_remove(`Average Cost per Trip FY`, "[$]")),
         fares_year = as.numeric(str_replace_all(`Fares FY`, c("\\$" = "", "," = ""))),
         operating_cost = as.numeric(str_replace_all(`Operating Expenses FY`, c("\\$" = "", "," = ""))),
         fares_perc = fares_year/operating_cost)

master_mb[master_mb$Agency == "Bi-State Development Agency of the Missouri-Illinois Metropolitan District, d.b.a.(St. Louis) Metro",]$Agency <- "Metro"

#### commuter rail ####
master_cr <- master %>% 
  filter(Mode == "CR" & 
           Active == "Active" & 
           `Reporter Type` == "Full Reporter" & 
           `Most Recent Report Year` == 2018) %>% 
  mutate(fares = as.numeric(str_remove(`Average Fares per Trip FY`, "[$]")),
         cost = as.numeric(str_remove(`Average Cost per Trip FY`, "[$]")),
         fares_year = as.numeric(str_replace_all(`Fares FY`, c("\\$" = "", "," = ""))),
         operating_cost = as.numeric(str_replace_all(`Operating Expenses FY`, c("\\$" = "", "," = ""))),
         fares_perc = fares_year/operating_cost)

#### light rail ####
master_lr <- master %>% 
  filter(Mode == "LR" & 
           Active == "Active" & 
           `Reporter Type` == "Full Reporter" & 
           `Most Recent Report Year` == 2018) %>% 
  mutate(fares = as.numeric(str_remove(`Average Fares per Trip FY`, "[$]")),
         cost = as.numeric(str_remove(`Average Cost per Trip FY`, "[$]")),
         fares_year = as.numeric(str_replace_all(`Fares FY`, c("\\$" = "", "," = ""))),
         operating_cost = as.numeric(str_replace_all(`Operating Expenses FY`, c("\\$" = "", "," = ""))),
         fares_perc = fares_year/operating_cost)

master_lr[master_lr$Agency == "Bi-State Development Agency of the Missouri-Illinois Metropolitan District, d.b.a.(St. Louis) Metro",]$Agency <- "Metro"
master_lr[master_lr$Agency == "New Jersey Transit Corporation" & master_lr$TOS == "DO",]$Agency <- " New Jersey Transit Corporation"

##### create and save plots #####
#### bus ####
master_mb %>% 
  arrange(-`Unlinked Passenger Trips FY`) %>%
  filter(!is.na(fares_perc)) %>% 
  slice(1:30) %>% 
  mutate(`Agency_City` = paste0(Agency, ", ", `HQ City`)) %>% 
  mutate(`Agency_City` = fct_reorder(`Agency_City`, desc(fares_perc))) %>%
  ggplot() +
  geom_bar(aes(x = `Agency_City`, 
               y = `fares_perc`, 
               fill = TOS), 
           stat = "identity", 
           size = .5,
           width = 0.75)+
  theme(axis.text.x = element_text(angle = 0, hjust=1))+
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey50", linetype = "solid", size = 0.1),
        panel.background = element_blank(),
        text = element_text(family = "SourceSansPro-Light", color = "grey10", lineheight = 0.5),
        plot.title = element_text(family = "SourceSansPro-Regular", size = 20, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.caption = element_text(size = 6, color = "grey50"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())+
  labs(x = NULL,
       y = NULL,
       title = "Bus Farebox Recovery Ratio",
       subtitle = paste0("Fraction of operating expenses which are met by the fares paid by passengers"),
       caption = "Directly Operated: Transportation service provided directly by a transit agency, using their employees to supply the necessary labor to operate the revenue vehicles.\n
       Purchased Transportation: Transportation service provided to a public transit agency or governmental unit from a public or private transportation provider based on a written contract.\n
       Source Data: National Transit Database, 2018\n")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values = c("#f9cfc5", "#ee6c4d"), 
                     labels = c("Directly Operated", "Purchased Transportation"),
                     name = "Type of Service")+
  geom_hline(yintercept = 0, color = "#444444", size = 0.5)+
  coord_flip()

ggsave("output_bus_bar.jpg", height = 8, width = 12, units = "in")

#### commuter rail ####

master_cr %>% 
  filter(!is.na(fares_perc)) %>% 
  arrange(-`Unlinked Passenger Trips FY`) %>% 
  slice(1:30) %>% 
  mutate(`Agency_City` = paste0(Agency, ", ", `HQ City`)) %>% 
  mutate(`Agency_City` = fct_reorder(`Agency_City`, desc(fares_perc))) %>%
  ggplot() +
  geom_bar(aes(x = `Agency_City`, 
               y = `fares_perc`, 
               fill = TOS), 
           stat = "identity",
           size = .5,
           width = 0.75)+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey50", linetype = "solid", size = 0.1),
        panel.background = element_blank(),
        text = element_text(family = "SourceSansPro-Light", color = "grey10", lineheight = 0.5),
        plot.title = element_text(family = "SourceSansPro-Regular", size = 20, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.caption = element_text(size = 6, color = "grey50"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())+
  labs(x = NULL,
       y = NULL,
       title = "Commuter Rail Farebox Recovery Ratio",
       subtitle = paste0("Fraction of operating expenses which are met by the fares paid by passengers"),
       caption = "Directly Operated: Transportation service provided directly by a transit agency, using their employees to supply the necessary labor to operate the revenue vehicles.\n
       Purchased Transportation: Transportation service provided to a public transit agency or governmental unit from a public or private transportation provider based on a written contract.\n
       Source Data: National Transit Database, 2018\n")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values = c("#d7bec8", "#843b62"), 
                    labels = c("Directly Operated", "Purchased Transportation"),
                    name = "Type of Service")+
  geom_hline(yintercept = 0, color = "#444444", size = 0.5)+
  coord_flip()

ggsave("output_cr_bar.jpg", height = 8, width = 12, units = "in")

#### light rail ####

master_lr %>% 
  filter(!is.na(fares_perc)) %>% 
  arrange(-`Unlinked Passenger Trips FY`) %>% 
  slice(1:30) %>% 
  mutate(`Agency_City` = paste0(Agency, ", ", `HQ City`)) %>% 
  mutate(`Agency_City` = fct_reorder(`Agency_City`, desc(fares_perc))) %>%
  ggplot() +
  geom_bar(aes(x = `Agency_City`, 
               y = `fares_perc`, 
               fill = TOS), 
           stat = "identity",
           size = 0.5,
           width = 0.75) +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey50", linetype = "solid", size = 0.1),
        panel.background = element_blank(),
        text = element_text(family = "SourceSansPro-Light", color = "grey10", lineheight = 0.5),
        plot.title = element_text(family = "SourceSansPro-Regular", size = 20, hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        plot.caption = element_text(size = 6, color = "grey50"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())+
  labs(x = NULL,
       y = NULL,
       title = "Light Rail Farebox Recovery Ratio",
       subtitle = paste0("Fraction of operating expenses which are met by the fares paid by passengers"),
       caption = "Directly Operated: Transportation service provided directly by a transit agency, using their employees to supply the necessary labor to operate the revenue vehicles.\n
       Purchased Transportation: Transportation service provided to a public transit agency or governmental unit from a public or private transportation provider based on a written contract.\n
       Source Data: National Transit Database, 2018\n")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values = c("#98b4d4", "#23395D"), 
                    labels = c("Directly Operated", "Purchased Transportation"),
                    name = "Type of Service")+
  geom_hline(yintercept = 0, color = "#444444", size = 0.5)+
  coord_flip()

ggsave("output_lr_bar.jpg", height = 8, width = 12, units = "in")
