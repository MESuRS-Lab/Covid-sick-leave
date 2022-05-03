#### FIGURES ###################################

source("COVID19-sick-leaves_analysis.R")
filepath = "Plots/"
filepath_extras = "Plots_extras/"

### Function that can reposition legend for a given grob p

cols_3 = c('#377eb8', '#ff7f00', '#e41a1c')
cols_4 = c('#756bb1', '#377eb8', '#ff7f00', '#e41a1c')

shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}

##################
### DEMOGRAPHY ###
##################

### Age-groups: proportion of total population, proportion of infections
colours_insee = c("Population (total)" = "#8dd3c7",
                  "Population (employed)" = "#ffffb3",
                  "Cumulative incidence (symptomatic COVID-19)" = "#fb8072",
                  "Cumulative incidence (sick-leave)" = "#bebada")

p_insee_infection = data_insee%>%
  group_by(AgeGroup)%>%
  summarise(N_total = sum(N_total), 
            N_active = sum(N_active))%>%
  mutate(p_active = N_active / N_total)%>%
  left_join(., data_cumulative_base_age)%>%
  ggplot(aes(x = AgeGroup))+
  theme_classic()+
  geom_bar(mapping = aes(y = N_total, fill = "Population (total)"), stat = 'identity', width = 0.95, colour = NA)+
  geom_bar(mapping = aes(y = N_active, fill = "Population (employed)"), stat = 'identity', width = 0.8, colour = NA)+
  geom_bar(mapping = aes(y = N_sick_leave, fill = "Cumulative incidence (sick-leave)"), stat = 'identity', width = 0.65, colour = NA)+
  geom_bar(mapping = aes(y = Symptomatic, fill = "Cumulative incidence (symptomatic COVID-19)"), stat = 'identity', width = 0.5, colour = NA)+
  scale_fill_manual("",values = colours_insee)+
  theme(legend.position = 'bottom',
        axis.text.x = element_text())+
  xlab("Age group")+ylab("")+
  ylab("Million (M)")+
  guides(fill=guide_legend(nrow=3,byrow=TRUE))+
  scale_y_continuous(labels = function(x) gsub("000000$", "M", x))+
  coord_flip()
p_insee_infection
ggsave(p_insee_infection, filename = paste0(filepath, "/plot_n_insee_infection.png"), width = 6, height = 4)


##############################
### SYMPTOMATIC SICK LEAVE ###
##############################
## Plot number of sick leaves due to COVID-19 symptoms by age and/or region

## Set colour palette
nb.colours = length(unique(data_final$AgeGroup))
agegroup.colours = colorRampPalette(wes_palette("Zissou1"))(nb.colours)

## 1a. PREVALENCE OF SYMPTOMATIC SICK LEAVES
## Plot number of symptomaitc sick leaves for all of France
plot_n_sickleavePrevalence_france = ggplot(data_final_france, aes(x = Time, y = N_COVID_leave_active_prevalence, colour = AgeGroup, group = AgeGroup))+
  geom_line(stat = 'identity')+
  theme_bw()+
  ylab('Prevalence of symptomatic COVID-19 sick leaves')+
  scale_colour_manual(values = agegroup.colours)
plot_n_sickleavePrevalence_france

## Plot number of symptomaitc sick leaves by age and region
plot_n_sickleavePrevalence_byregion = ggplot(filter(data_final_cc_base), aes(x = Time, y = N_COVID_leave_active_prevalence, colour = AgeGroup, group = AgeGroup))+
  geom_line(stat = 'identity')+
  theme_bw()+
  ylab('Prevalence of symptomatic COVID-19 sick leaves')+
  facet_wrap(facets = vars(Region), ncol = 3)+
  scale_colour_manual(values = agegroup.colours)+
  theme(legend.direction = 'horizontal',
        legend.position = 'bottom')
plot_n_sickleavePrevalence_byregion

# reorient legend and visualize using grid.draw()
plot_n_sickleavePrevalence_byregion_legendShift = shift_legend(plot_n_sickleavePrevalence_byregion)
grid.draw(plot_n_sickleavePrevalence_byregion_legendShift)

## save plots
ggsave(plot_n_sickleavePrevalence_byregion_legendShift, filename = paste0(filepath_extras, "/plot_n_sickleave_prevalence_byregion.png"), width = 8, height = 8)
ggsave(plot_n_sickleavePrevalence_france, filename = paste0(filepath_extras, "/plot_n_sickleave_prevalence_france.png"), width = 6, height = 6)

## 1a. INCIDENCE OF SYMPTOMATIC SICK LEAVES
plot_n_sickleaveIncidence_france = ggplot(data_final_france, aes(x = Time, y = N_COVID_leave_active_incidence, colour = AgeGroup, group = AgeGroup))+
  geom_line(stat = 'identity')+
  theme_bw()+
  ylab('Daily incidence of symptomatic COVID-19 sick leaves')+
  scale_colour_manual(values = agegroup.colours)
plot_n_sickleaveIncidence_france

## Plot incidence of symptomatic sick leaves by age and region
plot_n_sickleaveIncidence_byregion = ggplot(filter(data_final_cc_base), aes(x = Time, y = N_COVID_leave_active_incidence, colour = AgeGroup, group = AgeGroup))+
  geom_line(stat = 'identity')+
  theme_bw()+
  ylab('Daily incidence of symptomatic COVID-19 sick leaves')+
  facet_wrap(facets = vars(Region), ncol = 3)+
  scale_colour_manual(values = agegroup.colours)+
  theme(legend.direction = 'horizontal',
        legend.position = 'bottom')
plot_n_sickleaveIncidence_byregion

# reorient legend and visualize using grid.draw()
plot_n_sickleaveIncidence_byregion_legendShift = shift_legend(plot_n_sickleaveIncidence_byregion)
grid.draw(plot_n_sickleaveIncidence_byregion_legendShift)

## save plots
ggsave(plot_n_sickleaveIncidence_byregion_legendShift, filename = paste0(filepath_extras, "/plot_n_sickleave_incidence_byregion.png"), width = 8, height = 8)
ggsave(plot_n_sickleaveIncidence_france, filename = paste0(filepath_extras, "/plot_n_sickleave_incidence_france.png"), width = 6, height = 6)



## 2a. DAILY PROBABILITY OF SICK LEAVE (prevalence)
## Plot probability of COVID-related sick leaves for all of France
plot_p_sickleavePrevalence_france = ggplot(data_final_france, aes(x = Time, y = p_COVID_leave_prevalence, colour = AgeGroup, group = AgeGroup))+
  geom_line(stat = 'identity')+
  theme_bw()+
  ylab('Daily probability of being on symptomatic COVID-19 sick leave')+
  scale_colour_manual(values = agegroup.colours)
plot_p_sickleavePrevalence_france

## Plot probability of COVID-related sick leaves by age and region
plot_p_sickleavePrevalence_byregion = ggplot(filter(data_final_cc_base), aes(x = Time, y = p_COVID_leave_prevalence, colour = AgeGroup, group = AgeGroup))+
  geom_line(stat = 'identity')+
  theme_bw()+
  ylab('Daily probability of being on symptomatic COVID-19 sick leave')+
  facet_wrap(facets = vars(Region), ncol = 3)+
  scale_colour_manual(values = agegroup.colours)+
  theme(legend.direction = 'horizontal',
        legend.position = 'bottom')
plot_p_sickleavePrevalence_byregion

plot_p_sickleavePrevalence_byregion_legendShift = shift_legend(plot_p_sickleavePrevalence_byregion)
grid.draw(plot_p_sickleavePrevalence_byregion_legendShift)

## save plots
ggsave(plot_p_sickleavePrevalence_byregion_legendShift, filename = paste0(filepath_extras, "/plot_p_sickleave_prevalence_byregion.png"), width = 8, height = 8)
ggsave(plot_p_sickleavePrevalence_france, filename = paste0(filepath_extras, "/plot_p_sickleave_prevalence_france.png"), width = 6, height = 6)


## 2b. DAILY PROBABILITY OF SICK LEAVE (incidence)
## Plot probability of COVID-related sick leaves for all of France
plot_p_sickleaveIncidence_france = ggplot(data_final_france, aes(x = Time, y = p_COVID_leave_incidence, colour = AgeGroup, group = AgeGroup))+
  geom_line(stat = 'identity')+
  theme_bw()+
  ylab('Daily probability of taking symptomatic COVID-19 sick leave')+
  scale_colour_manual(values = agegroup.colours)
plot_p_sickleaveIncidence_france

## Plot probability of COVID-related sick leaves by Region
plot_p_sickleaveIncidence_byregion = ggplot(filter(data_final_cc_base), aes(x = Time, y = p_COVID_leave_incidence, colour = AgeGroup, group = AgeGroup))+
  geom_line(stat = 'identity')+
  theme_bw()+
  ylab('Daily probability of taking symptomatic COVID-19 sick leave')+
  facet_wrap(facets = vars(Region), ncol = 3)+
  scale_colour_manual(values = agegroup.colours)+
  theme(legend.direction = 'horizontal',
        legend.position = 'bottom')
plot_p_sickleaveIncidence_byregion

plot_p_sickleaveIncidence_byregion_legendShift = shift_legend(plot_p_sickleaveIncidence_byregion)
grid.draw(plot_p_sickleaveIncidence_byregion_legendShift)

## save plots
ggsave(plot_p_sickleaveIncidence_byregion_legendShift, filename = paste0(filepath_extras, "/plot_p_sickleave_incidence_byregion.png"), width = 8, height = 8)
ggsave(plot_p_sickleaveIncidence_france, filename = paste0(filepath_extras, "/plot_p_sickleave_incidence_france.png"), width = 6, height = 6)



## 3. Compare predicted sick leave to number of symptomatic COVID-19 cases
plot_prop_active_cases_causing_sick_leave = ggplot(filter(data_final_cc_base,Time == "2020-04-10"), aes(x = AgeGroup, y = N_COVID_leave_active_prevalence/Prevalence, colour = AgeGroup))+
  geom_point(stat = 'identity')+
  theme_bw()+
  ylab('Proportion of symptomatic cases resulting in sick leave')+
  facet_wrap(facets = vars(Region), ncol = 3)+
  scale_colour_manual(values = agegroup.colours)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.direction = 'horizontal',
        legend.position = 'bottom')
plot_prop_active_cases_causing_sick_leave


plot_prop_active_cases_causing_sick_leave_legendShift = shift_legend(plot_prop_active_cases_causing_sick_leave)
grid.draw(plot_prop_active_cases_causing_sick_leave_legendShift)

## save plots
ggsave(plot_prop_active_cases_causing_sick_leave_legendShift, filename = paste0(filepath_extras, "/plot_prop_active_cases_causing_sick_leave.png"), width = 8, height = 8)


### 4. Compare peak prevalence (infection and sick leave)
p_max_prevalence_age_region = ggplot(data_compare_peaks, aes(x = AgeGroup, y = Prevalence_max,fill = AgeGroup))+
  geom_bar(stat = 'identity', colour = 'black')+
  theme_bw()+
  ylab('Maximum prevalence of symptomatic COVID (first wave)')+
  xlab('Age')+
  facet_wrap(facets = vars(Region), ncol = 3)+
  scale_fill_manual(values = agegroup.colours)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_max_prevalence_age_region


p_max_sick_leave_age_region = ggplot(data_compare_peaks, aes(x = AgeGroup, y = N_COVID_leave_active_max, fill = AgeGroup))+
  geom_bar(stat = 'identity', colour = 'black')+
  theme_bw()+
  ylab('Maximum prevalence of sick leave for symptomatic COVID (first wave)')+
  xlab('Age')+
  facet_wrap(facets = vars(Region), ncol = 3)+
  scale_fill_manual(values = agegroup.colours)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_max_sick_leave_age_region


## Put these plots side-by-side
max_y = max(data_compare_peaks$Prevalence_max)

p_compare_max_prevalence_and_sick_leave = ggarrange(p_max_prevalence_age_region+ylim(0,max_y), p_max_sick_leave_age_region+ylim(0,max_y), ncol = 2, 
                                                    common.legend = T, legend = 'right', labels = c('A', 'B'))
p_compare_max_prevalence_and_sick_leave

## save plots
ggsave(p_compare_max_prevalence_and_sick_leave, filename = paste0(filepath_extras, "/plot_compare_peak_prevalence_with_sick_leave.png"),width = 14, height = 8)



### 5. Cumulative predictions over the first wave 
# max y value for same vertical scale
max_y_covid = max(data_cumulative_base$Symptomatic)
max_y_sl = max(data_cumulative_base$N_COVID_leave_active_incidence)

### cumulative symptomatic COVID-19 cases
p_firstwave_incidence_age_region = ggplot(data_cumulative_base, aes(x = AgeGroup, y = Symptomatic, fill = AgeGroup))+
  geom_bar(stat = 'identity', colour = 'black')+
  theme_bw()+
  labs(y = 'Number of symptomatic COVID-19 cases during first wave', x = '', fill = "Age")+
  facet_wrap(facets = vars(Region), ncol = 3)+
  scale_fill_manual(values = agegroup.colours)+
  theme(axis.text.x = element_blank()
        , strip.text = element_text(size = 6.5)) + 
  scale_y_continuous(limits = c(0, max_y_covid), labels = function(x) gsub("000$", "k", x)) 
p_firstwave_incidence_age_region

### cumulative symptomatic sick-leaves
p_firstwave_sick_leave_incidence_age_region = ggplot(data_cumulative_base, aes(x = AgeGroup, y = N_COVID_leave_active_incidence, fill = AgeGroup))+
  geom_bar(stat = 'identity', colour = 'black')+
  theme_bw()+
  labs(y = 'Number of symptomatic COVID-19 sick leaves during first wave', x = '', fill = "Age")+
  facet_wrap(facets = vars(Region), ncol = 3)+
  scale_fill_manual(values = agegroup.colours)+
  theme(axis.text.x = element_blank()
        , strip.text = element_text(size = 6.5)) + 
  scale_y_continuous(limits = c(0, max_y_sl), labels = function(x) gsub("000$", "k", x)) 
p_firstwave_sick_leave_incidence_age_region



## save plots
ggsave(plot = arrangeGrob(grobs = list(p_firstwave_incidence_age_region + guides(fill = F)
                                       , p_firstwave_sick_leave_incidence_age_region)
                          , nrow = 1, widths = list(13.5, 16.5))
       , filename = file.path(filepath_extras, "plot_compare_firstwave_incidence_with_sick_leave_French.png"), units = "cm", width = 25, height = 15)


## 6. What proportion of COVID cases and COVID absences are from each region? 
##    How do these differ?
data_compare_firstwave_proportions = data_cumulative_base%>%
  group_by(Region)%>%
  dplyr::summarise(N_COVID_leave_incidence_active_firstwave = sum(N_COVID_leave_active_incidence),
                   Incidence_firstwave = sum(Symptomatic))

data_compare_firstwave_proportions_d <- data_compare_firstwave_proportions %>% 
  dplyr::rename(SickLeave = N_COVID_leave_incidence_active_firstwave
                , Cases = Incidence_firstwave) %>% 
  pivot_longer(-Region, names_to = "measure", values_to = "Number") %>% 
  group_by(measure) %>% 
  mutate(Proportion = Number/sum(Number)) %>% 
  pivot_wider(id_cols = "Region", names_from = "measure", values_from = c("Number", "Proportion"))

###################################################
### COMPARE SYMPTOMATIC AND CONTACT SICK LEAVES ###
###################################################

# first, how many contacts did different age groups have over the pandemic?
p_contacts = data_final_cc_base%>%
  filter(Time == "2020-03-10")%>%
  dplyr::select(Region, AgeGroup, contacts_hh, contacts_familprol, contacts_travail, contacts_total)%>%
  pivot_longer(-c(Region, AgeGroup), names_to = "contact", values_to = "value")%>%
  mutate(contact = factor(contact,
                          levels = c("contacts_hh", "contacts_familprol", "contacts_travail", "contacts_total"),
                          labels = c("household", "family visit", "workplace", "all")))%>%
  filter(contact != "all")%>%
  mutate(Region_abbrev = case_when(
    Region %in% c("Hauts-de-France") ~ "HDF",
    Region %in% c("Grand-Est") ~ "GES",
    Region %in% c("PACA") ~ "PAC",
    Region %in% c("Auvergne-Rhone-Alpes") ~ "ARA",
    Region %in% c("Nouvelle-Aquitaine") ~ "NAQ",
    Region %in% c("Occitanie") ~ "OCC",
    Region %in% c("Bourgogne-Franche-Comte") ~ "BFC",
    Region %in% c("Centre-Val de Loire") ~ "CVL",
    Region %in% c("Pays de la Loire") ~ "PDL",
    Region %in% c("Bretagne") ~ "BRE",
    Region %in% c("Normandie") ~ "NOR",
    Region %in% c("Corse") ~ "COR",
    Region %in% c("Ile-de-France") ~ "IDF"))%>%
  ggplot(aes(x = AgeGroup, y = value, fill = contact))+
  geom_bar(stat = "identity")+
  theme_bw()+
  facet_wrap(facets = vars(Region_abbrev), ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  scale_fill_manual(name = "type of contact", values = c("#7fc97f", "#beaed4", "#fdc086"))+
  ylab("Average daily number of contacts")+
  xlab("Age group")
p_contacts
ggsave(p_contacts, filename = paste0(filepath_extras, "/plot_n_contacts.png"), width = 6, height = 8)


# plot number of sick leaves for cc vs. symptomatic over time for all France and all age groups:
p_n_sickleave_symp_cc_france_by_age = data_final_cc_base%>%
  group_by(Time, AgeGroup)%>%
  dplyr::summarise(`Symptomatic` = sum(N_COVID_leave_active_incidence),
                   `Contact` = sum(N_absence_cc_total),
                   `All` = sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total))%>%
  pivot_longer(-c(Time, AgeGroup), values_to = "count", names_to = "measure")%>%
  mutate(measure = factor(measure, levels = c('Symptomatic', 'Contact', 'All')))%>%
  ggplot(aes(x = Time, y = count, colour = AgeGroup))+
  geom_line()+
  ylab("Daily sick leave incidence")+
  theme_bw()+
  scale_colour_manual(values = agegroup.colours, name = "Age group")+
  facet_grid(cols = vars(measure))
p_n_sickleave_symp_cc_france_by_age
## save plots
ggsave(p_n_sickleave_symp_cc_france_by_age, filename = paste0(filepath_extras, "/plot_n_sickleave_symp_cc_france_by_age.png"), width = 12, height = 8)

# plot number of cc vs. symptomatic over time for all regions:
p_n_sickleave_symp_cc_by_region = data_final_cc_base%>%
  group_by(Time, Region)%>%
  dplyr::summarise(`Symptomatic` = sum(N_COVID_leave_active_incidence),
                   `Contact` = sum(N_absence_cc_total),
                   `All` = sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total))%>%
  pivot_longer(-c(Time, Region), values_to = "count", names_to = "measure")%>%
  mutate(measure = factor(measure, levels = c('Symptomatic', 'Contact', 'All')))%>%
  ggplot(aes(x = Time, y = count, colour = measure))+
  geom_line()+
  labs(y = "Daily sick leave incidence", colour = "Reason for sick leave")+
  theme_bw()+
  #scale_colour_manual(values = agegroup.colours, name = "Age group")+
  facet_wrap(facets = vars(Region), ncol = 3)+
  theme(legend.position = 'bottom',
        legend.direction =  'vertical')+ 
  scale_y_continuous(labels = function(x) gsub("000$", "k", x)) 
p_n_sickleave_symp_cc_by_region

# shift legend
p_n_sickleave_symp_cc_by_region_legendShift = shift_legend(p_n_sickleave_symp_cc_by_region)
grid.draw(p_n_sickleave_symp_cc_by_region_legendShift)
## save plots
ggsave(p_n_sickleave_symp_cc_by_region_legendShift, filename = paste0(filepath_extras, "/plot_n_sickleave_symp_cc_by_region.png"), width = 8, height = 8)


# plot number of cc stratifying by different types of contact for all regions:
p_n_sickleave_ccType_by_region = data_final_cc_base%>%
  group_by(Time, Region)%>%
  dplyr::summarise(`Home` = sum(N_absence_cc_hh),
                   `Work` = sum(N_absence_cc_travail),
                   `Caregiving` = sum(N_absence_cc_familprol))%>%
  pivot_longer(-c(Time, Region), values_to = "count", names_to = "measure")%>%
  ggplot(aes(x = Time, y = count, colour = measure))+
  geom_line()+
  labs(y = "Daily sick leave incidence (for contact with a symptomatic case)", colour = "Type of contact")+
  theme_bw()+
  #scale_colour_manual(values = agegroup.colours, name = "Age group")+
  facet_wrap(facets = vars(Region), ncol = 3)+
  theme(legend.position = 'bottom',
        legend.direction =  'vertical')+ 
  scale_y_continuous(labels = function(x) gsub("000$", "k", x)) 
p_n_sickleave_ccType_by_region

# shift legend
p_n_sickleave_ccType_by_region_legendShift = shift_legend(p_n_sickleave_ccType_by_region)
grid.draw(p_n_sickleave_ccType_by_region_legendShift)
## save plots
ggsave(p_n_sickleave_ccType_by_region_legendShift, filename = paste0(filepath_extras, "/plot_n_sickleave_ccType_by_region.png"), width = 8, height = 8)


# plot max cc vs. max symptomatic for all France and all age groups:
p_max_sickleave_cc_symp_france_by_age = data_final_cc_base%>%
  group_by(AgeGroup)%>%
  dplyr::summarise(`Symptomatic` = max(N_COVID_leave_active_prevalence),
                   `Contact` = max(N_absence_cc_total)*duration_sickleave,
                   `All` = max(N_COVID_leave_active_prevalence) + max(N_absence_cc_total)*duration_sickleave)%>%
  pivot_longer(-c(AgeGroup), values_to = "count", names_to = "measure")%>%
  mutate(measure = factor(measure, levels = c('Symptomatic', 'Contact', 'All')))%>%
  ggplot(aes(x = AgeGroup, y = count, fill = measure))+
  geom_bar(stat = 'identity', colour = 'black')+
  ylab("Peak number of active sick leaves during first wave")+
  theme_bw()+
  scale_fill_manual(values = cols_4[c(3,2,1)])+
  facet_grid(cols = vars(measure))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+ 
  scale_y_continuous(labels = function(x) gsub("000$", "k", x)) 
p_max_sickleave_cc_symp_france_by_age
## save plots
ggsave(p_max_sickleave_cc_symp_france_by_age, filename = paste0(filepath_extras, "/plot_max_sickleave_cc_symp_france_by_age.png"), width = 10, height = 6)


# total sick leaves over first wave
p_total_sickleave_cc_symp_france_by_age = data_final_cc_base%>%
  group_by(AgeGroup)%>%
  dplyr::summarise(`Symptomatic` = sum(N_COVID_leave_active_incidence),
                   `Contact` = sum(N_absence_cc_total),
                   `All` = sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total))%>%
  pivot_longer(-c(AgeGroup), values_to = "count", names_to = "measure")%>%
  mutate(measure = factor(measure, levels = c('Symptomatic', 'Contact', 'All')))%>%
  ggplot(aes(x = AgeGroup, y = count, fill = measure))+
  geom_bar(stat = 'identity', colour = 'black')+
  xlab('Age group')+
  ylab('Cumulative sick leave incidence')+
  theme_bw()+
  scale_fill_manual(values = cols_4[c(3,2,1)])+
  facet_grid(cols = vars(measure))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+ 
  scale_y_continuous(labels = function(x) gsub("000$", "k", x)) 
p_total_sickleave_cc_symp_france_by_age

p_total_sickleave_cc_symp_france_by_age2 = data_final_cc_base%>%
  group_by(AgeGroup)%>%
  dplyr::summarise(`Symptomatic COVID-19` = sum(N_COVID_leave_active_incidence),
                   `Contact (all)` = sum(N_absence_cc_total),
                   `Total` = sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total))%>%
  pivot_longer(-c(AgeGroup), values_to = "count", names_to = "measure")%>%
  mutate(measure = factor(measure, 
                          levels = c('Symptomatic COVID-19', 'Contact (all)', 'Total'),
                          labels = c('Symptomatic', 'Contact', 'All')))%>%
  ggplot(aes(y = AgeGroup, x = count, fill = measure))+
  geom_bar(stat = 'identity', colour = 'black')+
  ylab('Age group')+
  xlab('Cumulative sick leave incidence')+
  theme_bw()+
  scale_fill_manual(values = cols_4[c(3,2,1)])+
  facet_grid(rows = vars(measure))+
  theme(legend.position = "none")+ 
  scale_x_continuous(labels = function(x) gsub("000$", "k", x)) 
p_total_sickleave_cc_symp_france_by_age2

## save plots
ggsave(p_total_sickleave_cc_symp_france_by_age, filename = paste0(filepath_extras, "/plot_total_sickleave_cc_symp_france_by_age.png"), width = 8, height = 4)

# total sick leaves over first wave AND by region
p_total_sickleave_cc_symp_france_by_age_region = data_final_cc_base%>%
  group_by(AgeGroup, Region)%>%
  dplyr::summarise(`Symptomatic` = sum(N_COVID_leave_active_incidence),
                   `Contact` = sum(N_absence_cc_total),
                   `All` = sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total))%>%
  pivot_longer(-c(AgeGroup, Region), values_to = "count", names_to = "measure")%>%
  mutate(measure = factor(measure, levels = c('Symptomatic', 'Contact', 'All')))%>%
  mutate(Region_abbrev = case_when(
    Region %in% c("Hauts-de-France") ~ "HDF",
    Region %in% c("Grand-Est") ~ "GES",
    Region %in% c("PACA") ~ "PAC",
    Region %in% c("Auvergne-Rhone-Alpes") ~ "ARA",
    Region %in% c("Nouvelle-Aquitaine") ~ "NAQ",
    Region %in% c("Occitanie") ~ "OCC",
    Region %in% c("Bourgogne-Franche-Comte") ~ "BFC",
    Region %in% c("Centre-Val de Loire") ~ "CVL",
    Region %in% c("Pays de la Loire") ~ "PDL",
    Region %in% c("Bretagne") ~ "BRE",
    Region %in% c("Normandie") ~ "NOR",
    Region %in% c("Corse") ~ "COR",
    Region %in% c("Ile-de-France") ~ "IDF"))%>%
  ggplot(aes(x = AgeGroup, y = count, fill = measure))+
  geom_bar(stat = 'identity', colour = 'black')+
  ylab("Cumulative sick leave incidence")+
  xlab('Age group')+
  theme_bw()+
  scale_fill_manual(values = cols_4[c(3,2,1)])+
  facet_grid(cols = vars(measure), rows = vars(Region_abbrev))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+ 
  scale_y_continuous(labels = function(x) gsub("000$", "k", x)) 
p_total_sickleave_cc_symp_france_by_age_region

ggsave(p_total_sickleave_cc_symp_france_by_age_region, filename = paste0(filepath, "/plot_total_sickleave_cc_symp_france_by_age_region.png"), 
       width = 7, height = 10)


# total sick leaves over first wave by age ***as proportion of working population***
data_insee_national_active = data_insee%>%group_by(AgeGroup)%>%summarise(N_active_national = sum(N_active))

p_total_sickleave_propActive_cc_symp_france_by_age = data_final_cc_base%>%
  merge(., data_insee_national_active)%>%
  group_by(AgeGroup)%>%
  dplyr::summarise(`Symptomatic` = sum(N_COVID_leave_active_incidence)/max(N_active_national),
                   `Contact` = sum(N_absence_cc_total)/max(N_active_national),
                   `All` = (sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total))/max(N_active_national))%>%
  pivot_longer(-c(AgeGroup), values_to = "count", names_to = "measure")%>%
  mutate(measure = factor(measure, levels = c('Symptomatic', 'Contact', 'All')))%>%
  ggplot(aes(x = AgeGroup, y = count, fill = measure))+
  geom_bar(stat = 'identity', colour = 'black')+
  ylab("Proportion of working population\nthat took sick leave")+
  xlab('Age group')+
  theme_bw()+
  scale_fill_manual(values = cols_4[c(3,2,1)])+
  facet_grid(cols = vars(measure))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
p_total_sickleave_propActive_cc_symp_france_by_age



# GRID: combined total number of sick leaves, and proportion of working population taking sick leave

p_sickleave_by_age_total_vs_proportion_working_population = ggarrange(
  p_total_sickleave_cc_symp_france_by_age,
  p_total_sickleave_propActive_cc_symp_france_by_age,
  nrow = 2,
  common.legend = T,
  legend = 'right',
  labels = c('A', 'B'))
p_sickleave_by_age_total_vs_proportion_working_population
## save plots
ggsave(p_sickleave_by_age_total_vs_proportion_working_population,
       filename = paste0(filepath_extras, "/plot_total_vs_proportion_sickleave_by_age.png"), width = 10, height = 7)


# total sick leaves over first wave by age AND REGION ***as proportion of working population***
p_total_sickleave_propActive_cc_symp_france_by_age_region = data_final_cc_base%>%
  group_by(AgeGroup, Region)%>%
  dplyr::summarise(`Symptomatic COVID-19` = sum(N_COVID_leave_active_incidence)/max(N_active),
                   `Contact (all)` = sum(N_absence_cc_total)/max(N_active),
                   `Total` = (sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total))/max(N_active))%>%
  pivot_longer(-c(AgeGroup, Region), values_to = "count", names_to = "measure")%>%
  mutate(measure = factor(measure, 
                          levels = c('Symptomatic COVID-19', 'Contact (all)', 'Total'),
                          labels = c('Symptomatic', 'Contact', 'All')))%>%
  mutate(Region_abbrev = case_when(
    Region %in% c("Hauts-de-France") ~ "HDF",
    Region %in% c("Grand-Est") ~ "GES",
    Region %in% c("PACA") ~ "PAC",
    Region %in% c("Auvergne-Rhone-Alpes") ~ "ARA",
    Region %in% c("Nouvelle-Aquitaine") ~ "NAQ",
    Region %in% c("Occitanie") ~ "OCC",
    Region %in% c("Bourgogne-Franche-Comte") ~ "BFC",
    Region %in% c("Centre-Val de Loire") ~ "CVL",
    Region %in% c("Pays de la Loire") ~ "PDL",
    Region %in% c("Bretagne") ~ "BRE",
    Region %in% c("Normandie") ~ "NOR",
    Region %in% c("Corse") ~ "COR",
    Region %in% c("Ile-de-France") ~ "IDF"))%>%
  ggplot(aes(x = AgeGroup, y = count*100, fill = measure))+
  geom_bar(stat = 'identity', colour = 'black')+
  ylab("Sick leave attack rate (% of working population)")+ xlab("Age group")+
  theme_bw()+
  scale_fill_manual(values = cols_4[c(3,2,1)])+
  facet_grid(cols = vars(measure), rows = vars(Region_abbrev))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

p_total_sickleave_propActive_cc_symp_france_by_age_region

## save plots
ggsave(p_total_sickleave_propActive_cc_symp_france_by_age_region, filename = paste0(filepath, "/plot_total_sickleave_propActive_cc_symp_france_by_age_region.png"), 
       width = 7, height = 10)




# total cases over first wave
p_total_cases_france_by_age = data_final_cc_base%>%
  group_by(AgeGroup)%>%
  dplyr::summarise(`Symptomatic COVID-19` = sum(Symptomatic))%>%
  pivot_longer(-c(AgeGroup), values_to = "count", names_to = "measure")%>%
  ggplot(aes(x = AgeGroup, y = count, fill = AgeGroup))+
  geom_bar(stat = 'identity', colour = 'black')+
  ylab("Cumulative incidence of symptomatic COVID-19 over first wave")+
  theme_bw()+
  scale_fill_manual(values = agegroup.colours, name = "Age group")+
  facet_grid(cols = vars(measure))+ 
  scale_y_continuous(labels = function(x) gsub("000$", "k", x)) +
  theme(legend.position = "none")
p_total_cases_france_by_age

## save plots
ggsave(p_total_cases_france_by_age, filename = paste0(filepath_extras, "/plot_total_cases_france_by_age.png"), width = 7, height = 5)



# total cases over first wave as proportion of TOTAL population, by age and region
data_insee_national_total = data_insee%>%group_by(AgeGroup)%>%summarise(N_total_national = sum(N_total))

p_total_cases_propTotal_france_by_age = data_final_cc_base%>%
  merge(.,data_insee_national_total)%>%
  group_by(AgeGroup)%>%
  dplyr::summarise(`Symptomatic COVID-19` = sum(Symptomatic)/max(N_total_national))%>%
  pivot_longer(-c(AgeGroup), values_to = "count", names_to = "measure")%>%
  ggplot(aes(x = AgeGroup, y = count, fill = AgeGroup))+
  geom_bar(stat = 'identity', colour = 'black')+
  ylab("Proportion of French population\nwith symptomtaic COVID-19 over first wave")+
  theme_bw()+
  scale_fill_manual(values = agegroup.colours, name = "Age group")+
  facet_wrap(facets = vars(measure), ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')
p_total_cases_propTotal_france_by_age

## save plots
ggsave(p_total_cases_propTotal_france_by_age, filename = paste0(filepath_extras, "/plot_total_cases_propTotal_france_by_age.png"), width = 7, height = 5)



# total cases over first wave as proportion of total population, by age and region
p_total_cases_propTotal_france_by_age_region = data_final_cc_base%>%
  group_by(AgeGroup, Region)%>%
  dplyr::summarise(`Symptomatic COVID-19` = sum(Symptomatic)/max(N_total))%>%
  pivot_longer(-c(AgeGroup, Region), values_to = "count", names_to = "measure")%>%
  ggplot(aes(x = AgeGroup, y = count, fill = AgeGroup))+
  geom_bar(stat = 'identity', colour = 'black')+
  ylab("Proportion of French population with symptomtaic COVID-19 over first wave")+
  theme_bw()+
  scale_fill_manual(values = agegroup.colours, name = "Age group")+
  facet_wrap(facets = vars(Region), ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')
p_total_cases_propTotal_france_by_age_region

# shift legend
p_total_cases_propTotal_france_by_age_region_legendShift = shift_legend(p_total_cases_propTotal_france_by_age_region)
grid.draw(p_total_cases_propTotal_france_by_age_region_legendShift)

## save plots
ggsave(p_total_cases_propTotal_france_by_age_region_legendShift, filename = paste0(filepath_extras, "/plot_total_cases_propTotal_france_by_age_region.png"), width = 8, height = 10)




### FIGURE 1 ###

# A: Prevalence of COVID, sick-leave and arrêt maladie
data_prevalence_all = data_final_cc_base%>%
  dplyr::select(Region, AgeGroup, Time, Prevalence, N_COVID_leave_active_prevalence, N_absence_cc_total)%>%
  group_by(Time)%>%
  summarise(Prevalence = sum(Prevalence),
            Leave_symptoms = sum(N_COVID_leave_active_prevalence),
            Leave_contact = sum(N_absence_cc_total)*duration_sickleave,
            Leave_all = sum(N_COVID_leave_active_prevalence) + sum(N_absence_cc_total)*duration_sickleave)%>%
  pivot_longer(-Time, names_to = 'measure', values_to = 'value')


data_prevalence_all_region = data_final_cc_base%>%
  dplyr::select(Region, AgeGroup, Time, Prevalence, N_COVID_leave_active_prevalence, N_absence_cc_total)%>%
  group_by(Time,Region)%>%
  summarise(Prevalence = sum(Prevalence),
            Leave_symptoms = sum(N_COVID_leave_active_prevalence),
            Leave_contact = sum(N_absence_cc_total)*duration_sickleave,
            Leave_all = sum(N_COVID_leave_active_prevalence) + sum(N_absence_cc_total)*duration_sickleave)%>%
  pivot_longer(-c(Time,Region), names_to = 'measure', values_to = 'value')

p_prevalence_cases_and_sickleave = ggplot(data_prevalence_all, aes(x = Time, y = value, colour = measure, linetype = measure, size = measure))+
  geom_vline(xintercept = as.Date("2020-03-11"), colour = 'red', alpha = 0.2, size = 1.5)+
  annotate("text", x = as.Date("2020-03-09"), y = max(data_prevalence_all$value), label = "WHO declares pandemic", 
           angle = 90, hjust = 1, colour = "red", size = 2.5)+
  # annotate("rect", xmin = as.Date("2020-03-17"), xmax = as.Date("2020-05-11"), ymin = -Inf, ymax = Inf,
  #          fill = 'black', alpha = 0.1)+
  geom_vline(xintercept = as.Date("2020-03-17"), colour = 'black', alpha = 0.2, size = 1.5)+
  annotate("text", x = as.Date("2020-03-15"), y = max(data_prevalence_all$value), label = "national lockdown instated",
           angle = 90, hjust = 1, colour = "black", size = 2.5)+
  geom_vline(xintercept = as.Date("2020-05-11"), colour = 'black', alpha = 0.2, size = 1.5)+
  annotate("text", x = as.Date("2020-05-09"), y = max(data_prevalence_all$value), label = "national lockdown lifted",
           angle = 90, hjust = 1, colour = "black", size = 2.5)+
  geom_line()+
  theme_classic()+
  ylab('Prevalence')+xlab('2020')+
  scale_colour_manual("", values = cols_4, 
                      labels = c('Sick leave (all)', 'Sick leave (contact)', 'Sick leave (symptomatic)', 'Symptomatic COVID-19'))+ 
  scale_linetype_manual("", values = c(1, 1, 1, 2),
                        labels = c('Sick leave (all)', 'Sick leave (contact)', 'Sick leave (symptomatic)', 'Symptomatic COVID-19'))+
  scale_size_manual("", values = c(1,0.5,0.5,0.5),
                    labels = c('Sick leave (all)', 'Sick leave (contact)', 'Sick leave (symptomatic)', 'Symptomatic COVID-19'))+
  scale_y_continuous(labels = function(x) gsub("000$", "k", x))+
  scale_x_date(breaks = as.Date(c("2020-04-01", "2020-05-01", "2020-06-01")), labels = c("1 Apr", "1 May", "1 Jun"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))
p_prevalence_cases_and_sickleave

## save plots
ggsave(p_prevalence_cases_and_sickleave,filename = paste0(filepath_extras, "/plot_preavlence_over_time_cases_and_sickleaves.png"), width = 8, height = 3)


# B: Regional breakdown as proportion of all leaves
total_COVID = sum(data_final_cc_base$Symptomatic)
total_am_COVID = sum(data_final_cc_base$N_COVID_leave_active_incidence)
total_am_contact = sum(data_final_cc_base$N_absence_cc_total)
total_am = total_am_COVID+total_am_contact

data_prevalence_proportions = data_final_cc_base%>%
  dplyr::select(Region, Time, Symptomatic, N_COVID_leave_active_incidence, N_absence_cc_total)%>%
  group_by(Region)%>%
  summarise(Prop_Prevalence = sum(Symptomatic)/total_COVID,
            Prop_Leave_symptoms = sum(N_COVID_leave_active_incidence)/total_am_COVID,
            Prop_Leave_contact = sum(N_absence_cc_total)/total_am_contact,
            Prop_Leave = (sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total))/total_am)%>%
  pivot_longer(-Region, names_to = 'measure', values_to = 'proportion')

p_proportion_cases_absences_firstwave2 = data_prevalence_proportions %>% 
  filter(measure != "Prop_Leave")%>%
  arrange(measure) %>% 
  mutate(Region = factor(Region,
                         levels = c("Auvergne-Rhone-Alpes",
                                    "Bourgogne-Franche-Comte",
                                    "Bretagne",
                                    "Centre-Val de Loire",
                                    "Corse",
                                    "Grand-Est",
                                    "Hauts-de-France",
                                    "Ile-de-France",
                                    "Normandie",
                                    "Nouvelle-Aquitaine",
                                    "Occitanie",
                                    "PACA",
                                    "Pays de la Loire"),
                         labels = c("Auvergne-Rhône-Alpes\n(ARA)",
                                    "Bourgogne-Franche-Comté\n(BFC)",
                                    "Bretagne\n(BRE)",
                                    "Centre-Val de Loire\n(CVL)",
                                    "Corse\n(COR)",
                                    "Grand-Est\n(GES)",
                                    "Hauts-de-France\n(HDF)",
                                    "Île-de-France\n(IDF)",
                                    "Normandie\n(NOR)",
                                    "Nouvelle-Aquitaine\n(NAQ)",
                                    "Occitanie\n(OCC)",
                                    "Provence-Alpes-Côte d'Azur\n(PAC)",
                                    "Pays de la Loire\n(PDL)")))%>%
  ggplot(aes(x = Region, y = proportion, fill = measure))+
  geom_bar(stat = 'identity', position = position_dodge(), colour = "black")+
  theme_classic()+
  labs(y = 'Proportion of total', x = '', fill = "Outcome")+
  scale_fill_manual(values = cols_3, labels = c('Sick leave (contact)', 'Sick leave (symptomatic)', 'Symptomatic COVID-19'))+
  coord_flip() + 
  theme(legend.position = c(0.7, 0.2),
        legend.background = element_rect(colour = 'white', fill = 'white', linetype='solid'),
        legend.title = element_blank())
p_proportion_cases_absences_firstwave2

p_proportion_cases_absences_firstwave3 = data_prevalence_proportions %>% 
  filter(measure != "Prop_Leave")%>%
  arrange(measure) %>% 
  ggplot(aes(x = Region, y = proportion, fill = measure))+
  geom_bar(stat = 'identity', position = position_dodge(), colour = 'black')+
  theme_bw()+
  labs(y = 'Proportion of total', x = '', fill = "Outcome")+
  scale_fill_manual(values = cols_3, labels = c('Sick leave (contact)', 'Sick leave (symptomatic COVID-19)', 'Symptomatic COVID-19'))+
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
p_proportion_cases_absences_firstwave3

## save plots
ggsave(p_proportion_cases_absences_firstwave2,filename = paste0(filepath_extras, "/plot_proportion_cases_absences_firstwave_French2.png"), width = 8, height = 6)




### PREVALENCE SUMMARY ###

### Max prevalence, symptomatic COVID in working age population
data_prevalence_all%>%filter(measure == 'Prevalence')%>%arrange(-value)

### Max prevalence, sick leave (all)
data_prevalence_all%>%
  pivot_wider(names_from = 'measure', values_from = 'value')%>%
  group_by(Time)%>%
  mutate(Prevalence_leave_total = Leave_symptoms + Leave_contact)%>%arrange(-Prevalence_leave_total)


### Max prevalence, sick leave (symptomatic)
data_prevalence_all%>%filter(measure == 'Leave_symptoms')%>%arrange(-value)

### Proportion of sick leaves to total cases
data_prevalence_all%>%
  pivot_wider(names_from = 'measure', values_from = 'value')%>%
  mutate(prop_leave_symptoms = Leave_symptoms/Prevalence,
         prop_leave_contact = Leave_contact / Prevalence)

### INCIDENCE SUMMARY ###
data_incidence_all = data_final_cc_base%>%
  dplyr::select(Time, Region, AgeGroup, Symptomatic, N_COVID_leave_active_incidence,N_absence_cc_total)%>%
  group_by(Time)%>%
  summarise(incidence_COVID = sum(Symptomatic),
            incidence_leave_symptoms = sum(N_COVID_leave_active_incidence),
            incidence_leave_contact = sum(N_absence_cc_total))

# by region and time
data_incidence_all_region = data_final_cc_base%>%
  dplyr::select(Time, Region, AgeGroup, Symptomatic, N_COVID_leave_active_incidence,N_absence_cc_total)%>%
  group_by(Time, Region)%>%
  summarise(incidence_covid = sum(Symptomatic),
            incidence_leave = sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total),
            incidence_leave_symptoms = sum(N_COVID_leave_active_incidence),
            incidence_leave_contact = sum(N_absence_cc_total))

# by region cumulatively
data_incidence_all_region_cumul = data_final_cc_base%>%
  dplyr::select(Time, Region, AgeGroup, Symptomatic, N_COVID_leave_active_incidence,N_absence_cc_total)%>%
  group_by(Region)%>%
  summarise(incidence_covid = sum(Symptomatic),
            incidence_leave = sum(N_COVID_leave_active_incidence) + sum(N_absence_cc_total),
            incidence_leave_symptoms = sum(N_COVID_leave_active_incidence),
            incidence_leave_contact = sum(N_absence_cc_total))%>%
  # fix Region order (PACA last)
  mutate(Region = factor(Region,
                         levels = c("Auvergne-Rhone-Alpes",
                                    "Bourgogne-Franche-Comte",
                                    "Bretagne",
                                    "Centre-Val de Loire",
                                    "Corse",
                                    "Grand-Est",
                                    "Hauts-de-France",
                                    "Ile-de-France",
                                    "Normandie",
                                    "Nouvelle-Aquitaine",
                                    "Occitanie",
                                    "Pays de la Loire",
                                    "PACA")),
         Region_accents = factor(Region,
                                 levels = c("Auvergne-Rhone-Alpes",
                                            "Bourgogne-Franche-Comte",
                                            "Bretagne",
                                            "Centre-Val de Loire",
                                            "Corse",
                                            "Grand-Est",
                                            "Hauts-de-France",
                                            "Ile-de-France",
                                            "Normandie",
                                            "Nouvelle-Aquitaine",
                                            "Occitanie",
                                            "Pays de la Loire",
                                            "PACA"),
                                 labels = data_insee_active$Region))

# sum total population size across all ages
data_insee_active_summed = data_insee_active
data_insee_active_summed$N_active = rowSums(data_insee_active[,2:ncol(data_insee_active_summed)])
data_insee_active_summed = data_insee_active_summed%>%
  mutate(Region_accents = Region)%>%
  dplyr::select(Region_accents,N_active)

data_incidence_all_region_cumul_insee = left_join(
  data_incidence_all_region_cumul,
  data_insee_active_summed)%>%
  mutate(attack_rate = incidence_leave/N_active)%>%
  dplyr::select(c("Region", "attack_rate"))

# divide by region population size


# cumulative incidence (COVID among working age adults)
sum(data_incidence_all$incidence_COVID)

# cumulative incidence (leave symptoms)
sum(data_incidence_all$incidence_leave_symptoms)

# cumulative incidence (leave contact)
sum(data_incidence_all$incidence_leave_contact)

# cumulative incidence (leave total)
sum(data_incidence_all$incidence_leave_symptoms) + sum(data_incidence_all$incidence_leave_contact)

# SL incidence (25% reporting)
sum(data_final_cc_25$N_COVID_leave_active_incidence) + sum(data_final_cc_25$N_absence_cc_total)

# SL incidence (50% reporting)
sum(data_final_cc_50$N_COVID_leave_active_incidence) + sum(data_final_cc_50$N_absence_cc_total)

# SL incidence (75% reporting)
sum(data_final_cc_75$N_COVID_leave_active_incidence) + sum(data_final_cc_75$N_absence_cc_total)

# SL incidence (100% reporting)
sum(data_final_cc_100$N_COVID_leave_active_incidence) + sum(data_final_cc_100$N_absence_cc_total)


# peak incidence by region
df_max_prev = data.frame()
for(region_i in levels(data_prevalence_all_region$Region)){
  
  df_i = data_prevalence_all_region%>%
    filter(Region == region_i,
           measure == "Leave_all")
  
  df_j = data_prevalence_all_region%>%
    filter(Region == region_i,
           measure == "Prevalence")
  
  max_leave = max(df_i$value)
  max_leave_date = df_i$Time[which(df_i$value == max_leave)]
  
  max_prev = max(df_j$value)
  max_prev_date = df_j$Time[which(df_j$value == max_prev)]
  
  df_max_prev_i = data.frame(region = region_i,
                             max = max_leave, 
                             max_date = max_leave_date,
                             indicator = "sick leave")
  
  df_max_prev_j = data.frame(region = region_i,
                             max = max_prev, 
                             max_date = max_prev_date,
                             indicator = "symptomatic COVID-19")
  
  df_max_prev = rbind(df_max_prev, df_max_prev_i, df_max_prev_j)
}

df_max_prev_abbrev = df_max_prev%>%
  mutate(Region_abbrev = case_when(
    region %in% c("Hauts-de-France") ~ "HDF",
    region %in% c("Grand-Est") ~ "GES",
    region %in% c("PACA") ~ "PAC",
    region %in% c("Auvergne-Rhone-Alpes") ~ "ARA",
    region %in% c("Nouvelle-Aquitaine") ~ "NAQ",
    region %in% c("Occitanie") ~ "OCC",
    region %in% c("Bourgogne-Franche-Comte") ~ "BFC",
    region %in% c("Centre-Val de Loire") ~ "CVL",
    region %in% c("Pays de la Loire") ~ "PDL",
    region %in% c("Bretagne") ~ "BRE",
    region %in% c("Normandie") ~ "NOR",
    region %in% c("Corse") ~ "COR",
    region %in% c("Ile-de-France") ~ "IDF"))


p_peak_leave_regions_dates = ggplot(df_max_prev_abbrev%>%filter(indicator == "sick leave"),
                               aes(x = max_date, y = max, colour = region, label = Region_abbrev))+
  geom_point()+
  theme_bw()+
  geom_text_repel(size = 3, colour = 'black')+
  xlab("Date of peak (year 2020)")+ylab("Maximum sick leave prevalence")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = function(x) gsub("000$", "k", x))
p_peak_leave_regions_dates

p_peaks_regions_dates = ggplot(df_max_prev_abbrev%>%filter(indicator == "sick leave"),
                               aes(x = max_date, y = max, colour = region, label = Region_abbrev, shape = indicator))+
  geom_point()+
  theme_bw()+
  xlab("Date of peak prevalence")+ylab("Peak prevalence")+
  theme(legend.position = 'bottom')+
  scale_y_continuous(labels = function(x) gsub("000$", "k", x))+
  geom_point(data = df_max_prev_abbrev%>%filter(indicator == "symptomatic COVID-19"), mapping = aes())+
  geom_line(data = df_max_prev_abbrev, mapping = aes(group = region), alpha = 0.25)+
  geom_text_repel(size = 2)+
  guides(colour = "none")+
  scale_shape_manual("", values = c(1,2))
p_peaks_regions_dates
ggsave(p_peaks_regions_dates,filename = paste0(filepath, "/peaks_regions_dates.png"), width = 5, height = 4)



# peak prevalence by region


### MAP
library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(viridis)

### SUMMARY FIGURE
france <- ne_states(geounit = "france", returnclass = "sf")%>%
  mutate(Region = case_when(
    region %in% c("Nord-Pas-de-Calais", "Picardie") ~ "Hauts-de-France",
    region %in% c("Champagne-Ardenne", "Lorraine", "Alsace") ~ "Grand-Est",
    region %in% c("Provence-Alpes-Côte-d'Azur") ~ "PACA",
    region %in% c("Rhône-Alpes", "Auvergne") ~ "Auvergne-Rhone-Alpes",
    region %in% c("Aquitaine", "Poitou-Charentes", "Limousin") ~ "Nouvelle-Aquitaine",
    region %in% c("Midi-Pyrénées", "Languedoc-Roussillon") ~ "Occitanie",
    region %in% c("Franche-Comté", "Bourgogne") ~ "Bourgogne-Franche-Comte",
    region %in% c("Centre") ~ "Centre-Val de Loire",
    region %in% c("Pays de la Loire") ~ "Pays de la Loire",
    region %in% c("Bretagne") ~ "Bretagne",
    region %in% c("Basse-Normandie", "Haute-Normandie") ~ "Normandie",
    region %in% c("Corse") ~ "Corse",
    region %in% c("Île-de-France") ~ "Ile-de-France"))%>%
  mutate(Region_abbrev = case_when(
    region %in% c("Nord-Pas-de-Calais", "Picardie") ~ "HDF",
    region %in% c("Champagne-Ardenne", "Lorraine", "Alsace") ~ "GES",
    region %in% c("Provence-Alpes-Côte-d'Azur") ~ "PAC",
    region %in% c("Rhône-Alpes", "Auvergne") ~ "ARA",
    region %in% c("Aquitaine", "Poitou-Charentes", "Limousin") ~ "NAQ",
    region %in% c("Midi-Pyrénées", "Languedoc-Roussillon") ~ "OCC",
    region %in% c("Franche-Comté", "Bourgogne") ~ "BFC",
    region %in% c("Centre") ~ "CVL",
    region %in% c("Pays de la Loire") ~ "PDL",
    region %in% c("Bretagne") ~ "BRE",
    region %in% c("Basse-Normandie", "Haute-Normandie") ~ "NOR",
    region %in% c("Corse") ~ "COR",
    region %in% c("Île-de-France") ~ "IDF"))

### blank plot of france with corrected regions
# france %>% 
#   group_by(Region) %>%
#   summarise() %>% 
#   ggplot() +
#   geom_sf() +
#   theme_void()

### Combine with cumumalative sick-leave incidence
france_covid = left_join(france, data_cumulative_base_region)

cols_gradient = rev(c('#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd'))

p_france_sickleaves = france_covid %>% 
  group_by(Region, N_sick_leave, Region_abbrev) %>%
  summarise() %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = N_sick_leave)) +
  geom_sf_text(mapping = aes(label = Region_abbrev))+
  theme_void()+
  scale_fill_gradientn("Cumulative\nsick leave\nincidence", colors = cols_gradient, labels = function(x) gsub("000$", "k", x))


france_covid2 = left_join(france,data_incidence_all_region_cumul_insee)

# p_france_sickleaves2 = 
p_france_sickleaves2 = france_covid2 %>% 
  group_by(Region, attack_rate, Region_abbrev) %>%
  summarise() %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = attack_rate*100)) +
  geom_sf_text(mapping = aes(label = Region_abbrev))+
  theme_void()+
  scale_fill_gradientn("Sick leave\nattack rate\n(% of working\npopulation)", colors = cols_gradient)


### combine plot

p_grid1 = plot_grid(p_prevalence_cases_and_sickleave+theme(legend.position = 'bottom'),#+theme(legend.position = c(0.7, 0.8)), 
                    p_france_sickleaves2+theme(legend.position = 'right'), ncol = 2,
                    labels = c('A', 'B'))
p_grid2 = plot_grid(p_proportion_cases_absences_firstwave2+theme(legend.position = c(0.7, 0.85)), 
                    p_total_sickleave_cc_symp_france_by_age2, 
                    ncol = 2, labels = c('C', 'D'),
                    rel_widths = c(1.2,1))
p_grid = plot_grid(p_grid1, p_grid2, nrow = 2, rel_heights = c(2.2,3))
p_grid

ggsave(p_grid,filename = paste0(filepath, "/figure1.pdf"), width = 10, height = 9)
ggsave(p_grid,filename = paste0(filepath, "/figure1.png"), width = 10, height = 9)

