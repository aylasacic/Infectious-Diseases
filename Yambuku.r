#------------------------------------------------------------------------------#-
  title: "Yambuku and Sierra Leone Ebola outbreaks analysis"
  authors: "Ajla Šačić, Hayat Jan & Nataly Ojeda Mosquera"
#------------------------------------------------------------------------------#
#-------------------------------'Packages Used'--------------------------------#
  
library(tidyverse)
library(EpiCurve)
library(incidence)
library(outbreaks)
library(lubridate)
library(ggthemes)
library(EpiEstim)
library(cowplot)
library(readr)
  
#------------------------------------------------------------------------------#
#------------------------------'Loading Data...'-------------------------------#

# We load the data that has been converted to a csv file before
yambuku.data<-read_csv("Yambuku.csv")

# Sierra Leone data is contained in the outbreaks package under the name
# ebola_sierra_leone_2014 and we will thus not load it in, but use it as such

#------------------------------------------------------------------------------#
#--------------------------------'Cleaning Data'-------------------------------#

# We omit all rows which have NA answers in them
yambuku.data<-na.omit(yambuku.data)
    
# Checking the class of the date column -> character
class(yambuku.data$disease_onset)

# We must first convert the dates into proper formats and into Date class
# before making the graphs

# First we need to convert characters to time objects
yambuku.data$disease_onset <- strptime(as.character(yambuku.data$disease_onset),
                                       "%d/%m/%Y")

# Then we convert the disease_onset column into the proper format and Date class
yambuku.data$disease_onset <- as.Date(yambuku.data$disease_onset, format =
                                        "%Y-%m-%d")

# We do the same procedure for the other date columns, disease_ended
yambuku.data$disease_ended <- strptime(as.character(yambuku.data$disease_ended),
                                       "%d/%m/%Y")
yambuku.data$disease_ended <- as.Date(yambuku.data$disease_ended, format =
                                     "%Y-%m-%d")

# Checking the class of the date column -> Date
class(yambuku.data$disease_onset)

#------------------------------------------------------------------------------#
#---------------------------'Creating variables'-------------------------------#

# We create a variable to hold the month of the disease 
yambuku.data$month<-month(yambuku.data$disease_onset)

#------------------------------------------------------------------------------#
#----------------------'Plotting epicurves and barplots'-----------------------#
  
#----------------------------------'Yambuku'-----------------------------------#
#-----------------------------'GENERAL EPICURVE'-------------------------------#

# Now we can plot the epicurve using the incidence function
# Since the Yambuku outbreak was quite short, the interval is put to "day""
yambuku.epicurve <- incidence(yambuku.data$disease_onset, 
                                                   interval = "day")

# Plotting the epicurve using the plot() function
yambuku.plotted.epicurve <- plot(yambuku.epicurve, border = "black")+
                            guides(fill = "none")+
                            ggtitle("Yambuku epicurve")+
                            xlab("Date")+
                            theme_minimal()+
                            scale_fill_discrete(type = "#6eb5ff")+
                            scale_x_date(breaks = "week", 
                                         date_labels = "%d-%b")+
                            geom_vline(xintercept = as.Date("1976-10-06"), 
                                       col = 'red', size = 1.2)

# Plot without labels that will be used to compare the general progression of
# the Yambuku and Sierra Leone outbreaks
yambuku.plotted.epicurve.nolabs <- plot(yambuku.epicurve, border = "black")+
                                   guides(fill = "none")+
                                   ggtitle("Yambuku epicurve")+
                                   theme_minimal()+
                                   scale_fill_discrete(type = "#6eb5ff")+
                                   scale_x_date(labels = NULL, breaks = NULL)+
                                   labs(x="")+
                                   ylab("Incidence")+
                                   geom_vline(xintercept = 
                                                as.Date("1976-10-06"), 
                                              col = 'red', size = 1.2)

yambuku.plotted.epicurve + theme(plot.title = 
                                   element_text(color = "black", size = 14, 
                                                face = "bold", hjust = 0.5),
                                 axis.title.x = 
                                   element_text(color = "black", size = 10, 
                                                face = "bold", hjust = 0.5),
                                 axis.title.y = 
                                   element_text(color = "black", size = 10, 
                                                face = "bold")) 
ggdraw(add_sub(yambuku.plotted.epicurve.nolabs,
"*Red line designates when the intervention plan was introduced (Oct. 6th)", 
               size = 10, x = 0.27, y = 0.25, color = 'red')) +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold",
                                  hjust = 0.5),
        axis.title.x = element_text(color = "black", size = 10, face = "bold",
                                    hjust = 0.5),
        axis.title.y =  element_text(color = "black", size = 10,face = "bold"))

#----------------------------'FIRST MONTH EPICURVE'----------------------------#

# First we count the cases based on the onset of the disease and form rows where
# each row designates a date and contains the number of cases in the column next
# to the date
yambuku.cases.cumulative <- yambuku.data %>% 
                            dplyr::select(disease_onset) %>% 
                            group_by(disease_onset) %>% 
                            mutate(I=n()) %>%
                            arrange(disease_onset) %>%
                            filter(row_number()==1 & !is.na(disease_onset)) %>%
                            rename(dates = disease_onset)

# We delete the columns after the first month (the look-up was done manually)
yambuku.cases.cumulative <- yambuku.cases.cumulative[c(1:25),]

# We plot the curve for the first month
first.month.yambuku <- ggplot(yambuku.cases.cumulative, aes(x = dates, y = I))+    
                       geom_bar(stat = "identity", fill ="#6eb5ff", 
                                color ="black", width=0.9) +
                       theme(axis.text.x = element_text(angle = 90, size = 10))+
                       ylab("Incidence") + xlab("Dates") +
                       scale_y_continuous(limits=c(0, 30),breaks = seq(0,30, 
                                                                       by=10))+
                       ggtitle("First month of outbreak, Yambuku")+
                       theme_minimal()+
                       theme(axis.text.x = element_text(angle = 45, hjust = 1),
                        plot.title = element_text(color = "black", size = 14, 
                                                  face = "bold", hjust = 0.5),
                        axis.title.x = element_text(color = "black", size = 10, 
                                                    face = "bold", hjust = 0.5),
                        axis.title.y = element_text(color = "black", size = 10,
                                                    face = "bold"))

#------------------------------'GENDER EPICURVE'-------------------------------#
  
# We can also plot the Epicurve to separate genders
yambuku.epicurve.gender <- incidence(yambuku.data$disease_onset, 
                                     interval="day",
                                     groups=yambuku.data$sex)

yambuku.epicurve.gender.plotted <- plot(yambuku.epicurve.gender, 
                                        border="black")+
                                   guides(fill=guide_legend(title="Groups"))+
                                   ylab("Incidence")+
                                   xlab("Date")+
                                   theme_minimal()+
                                   ggtitle("Yambuku epicurve ")+
                                   scale_x_date(date_breaks = "week", 
                                                date_labels ="%d-%b")+
                                   scale_fill_manual(values = 
                                                       c("male" = "#6eb5ff",
                                                         "female" = "#FF7F50"))

#------------------------------'GENDER BARPLOT'--------------------------------#
  
# plotting the female and male cases in the data set
yambuku.cases.gender <- yambuku.data %>% 
                        ggplot()+
                        geom_bar(mapping=aes(x = sex, fill = sex),
                                 stat = "count",
                                 position = "dodge")+
                        theme(axis.text.x = element_text(angle = 0))+
                        theme_minimal()+
                        scale_fill_manual(values = c("male" = "#6eb5ff",
                                                     "female" = "#FF7F50"))+
                        labs(fill='Sex')+
                        xlab("Sex") + ylab("Number of cases")+
                        theme(axis.title.x =  element_text(color = "black", 
                                                           size = 10,
                                                           face = "bold", 
                                                           hjust = 0.5),
                              axis.title.y = element_text(color = "black", 
                                                          size = 10,
                                                          face = "bold"))

#-------------------------------'Sierra Leone'---------------------------------#
#-----------------------------'GENERAL EPICURVE'-------------------------------#

# We can compare the Yambuku outbreak, the first recorded Ebola outbreak in
# history, to the 2014 Sierra Leone outbreak.
# Sierra Leone data -> ebola_sierraleone_2014 is in outbreaks package
# We repeat the same procedure as with Yambuku data, but since Sierra Leone
# outbreak was a much larger outbreak that spanned more than a few months
# we need to scale the intervals properly, i.e. to a week.
  
sierra.leone.epicurve <-incidence(ebola_sierraleone_2014$date_of_onset, 
                                  interval="week")

# Plotting the epicurve using the plot() function
sierra.leone.plotted.epicurve <- plot(sierra.leone.epicurve, border="black")+
                                 guides(fill = "none")+
                                 xlab("Date")+
                                 theme_minimal()+
                                 scale_fill_discrete(type = "#bee5b0")+
                                 ggtitle("Sierra Leone epicurve")+
                                 scale_x_date(breaks = "month",
                                   date_labels = "%b-%Y")+
                                 theme(axis.text.x = 
                                         element_text(angle = 90, hjust = 1))+
                                 geom_vline(xintercept = as.Date("2014-10-17"), 
                                           col = 'red', size = 15.5, 
                                           alpha = 0.4)

sierra.leone.plotted.epicurve + theme(plot.title = 
                                   element_text(color = "black", size = 14, 
                                                face = "bold", hjust = 0.5),
                                 axis.title.x = 
                                   element_text(color = "black", size = 10, 
                                                face = "bold", hjust = 0.5),
                                 axis.title.y = 
                                   element_text(color = "black", size = 10, 
                                                face = "bold")) 

sl.plotted.epicurve.nolabs <- plot(sierra.leone.epicurve, border = "black")+
                              guides(fill = "none")+
                              ggtitle("Sierra Leone epicurve")+
                              theme_minimal()+
                              scale_fill_discrete(type = "#bee5b0")+
                              scale_x_date(labels = NULL, breaks = NULL)+
                              labs(x="")+
                              ylab("Incidence")+
                              geom_vline(xintercept = as.Date("2014-10-17"), 
                                         col = 'red', size = 15, alpha = 0.4)

ggdraw(add_sub(sl.plotted.epicurve.nolabs, 
"*Red area designates when the intervention plan was introduced (Oct-Nov)", 
size = 10, x = 0.27, y = 0.25, color = 'red')) +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold",
                                                 hjust = 0.5),
        axis.title.x = element_text(color = "black", size = 10, face = "bold",
                                                 hjust = 0.5),
        axis.title.y =  element_text(color = "black", size = 10,face = "bold")) 

#----------------------------'FIRST MONTH EPICURVE'----------------------------#

# First we count the cases based on the onset of the disease and form rows where
# each row designates a date and contains the number of cases in the column next
# to the date
sl.cases.cumulative <- ebola_sierraleone_2014 %>% 
                      dplyr::select(date_of_onset) %>% 
                      group_by(date_of_onset) %>% 
                      mutate(I=n()) %>%
                      arrange(date_of_onset) %>%
                      filter(row_number()==1 & !is.na(date_of_onset)) %>%
                      rename(dates = date_of_onset)

# We delete the columns after the first month (the look-up was done manually)
sl.cases.cumulative <- sl.cases.cumulative[c(1:29),]

# We plot the curve for the first month
first.month.sl <- ggplot(sl.cases.cumulative, aes(x = dates, 
                                                    y = I)) +    
                  geom_bar(stat = "identity", fill ="#6eb5ff", color ="black", 
                           width=0.9) +
                  theme(axis.text.x = element_text(angle = 90, size = 10))+
                  ylab("Incidence") + xlab("Dates") +
                  scale_y_continuous(limits=c(0, 30),
                                     breaks = seq(0,30, by=10)) +
                  ggtitle("First month of outbreak, Sierra Leone")+
                  theme_minimal()+
                  theme(axis.text.x = 
                          element_text(angle = 45, hjust = 1),
                        plot.title = element_text(color = "black", size = 14, 
                                                  face = "bold", hjust = 0.5),
                        axis.title.x = element_text(color = "black", size = 10, 
                                                    face = "bold", hjust = 0.5),
                        axis.title.y = element_text(color = "black", size = 10,
                                                    face = "bold"))

#------------------------------'GENDER EPICURVE'-------------------------------#
# As with Yambuku, we can plot the epicurve which divides the genders
# Unlike Yambuku, there are many rows where the gender is labeled as NA,
# especially at the height of the epidemic. We can use that occurrence to 
# show how the medical staff in Sierra Leone could not properly document
# the details of the cases because of the scale of the epidemic
  
salone.epicurve.gender <- incidence(ebola_sierraleone_2014$date_of_onset, 
                                    interval = "week",
                                    groups = ebola_sierraleone_2014$sex)

plot(salone.epicurve.gender, border="black")+
                guides(fill=guide_legend(title="Groups"))+
                ylab("Number of cases")+
                xlab("Date")+
                theme_minimal()+
                ggtitle("Sierra Leone Gender Epicurve ")+
                scale_x_date(date_breaks = "month", date_labels ="%b-%Y")+
                scale_fill_manual(values = c("M" = "#6eb5ff",
                                             "F" = "#FF7F50",
                                             "NA" = "grey"))+
                theme(axis.text.x = element_text(angle = 90, hjust = 1),
                      plot.title = element_text(color = "black", size = 14, 
                                                face = "bold", hjust = 0.5),
                      axis.title.x = element_text(color = "black", size = 10, 
                                                  face = "bold", hjust = 0.5),
                      axis.title.y = element_text(color = "black", size = 10, 
                                                  face = "bold"))

#------------------------------'GENDER BARPLOT'--------------------------------#

# Plotting the female and male cases in the Sierra Leone data set
salone.cases.gender <- ebola_sierraleone_2014 %>% 
                       ggplot()+
                       geom_bar(mapping=aes(x = sex, fill = sex),
                                 stat = "count",
                                 position = "dodge")+
                       theme(axis.text.x = element_text(angle = 0))+
                       theme_minimal()+
                       scale_fill_manual(values = c("M" = "#6eb5ff",
                                                     "F" = "#FF7F50",
                                                     "NA" = "grey"),
                                          na.value="grey")+
                       labs(fill='Sex')+
                       xlab("Sex") + ylab("Number of cases")+
                       theme(axis.title.x = element_text(color = "black", 
                                                          size = 10, 
                                                          face = "bold", 
                                                          hjust = 0.5),
                              axis.title.y = element_text(color = "black", 
                                                          size = 10,
                                                          face = "bold")) 
  
#-----------------------------'Combining epicurves'----------------------------#

# To point at certain periods of action during the outbreaks, we can plot
# the two epicurves together using plot_grid() function from cowplot library
combined.epicurves<-cowplot::plot_grid(yambuku.plotted.epicurve.nolabs, 
                                       sl.plotted.epicurve.nolabs,
                                       ncol = 1, 
                                       rel_heights = c(1,2),
                                       align = 'v', 
                                       axis = 'lr', 
                                       labels = c("A","B"), 
                                       label_size = 15)

#------------------------------------------------------------------------------#
#----------------------------'Case-fatality rate'------------------------------#
  
# We can check the case-fatality rate for the Yambuku outbreak by dividing the
# number of those who passed away with the total number of infected people
  
cfr <- yambuku.data %>% 
       mutate(cfr = sum(status == "died",na.rm = TRUE)/n())

cfr$cfr[1] # -> cfr for Yambuku
# cfr of Sierra Leone (2014) outbreak was about 0.28

# We can plot the cfr per week of the pandemic
cfr.cumulative <- yambuku.data %>% 
                  # mutate() adds new variables and preserves existing ones
                  # we add onset that scales the date down according to the week
                  mutate(onset = floor_date(disease_onset, "week")) %>% 
                  count(onset, status) %>% 
                  group_by(onset) %>% 
                  mutate(cases = sum(n)) %>% 
                  filter(status == "died") %>% 
                  ungroup %>%
                  arrange(onset) %>%
                  mutate(cfr = cumsum(n)/cumsum(cases))

cfr.plot.yambuku <- ggplot(data = cfr.cumulative, aes(x = onset, y = cfr))+
                    geom_line()+
                    geom_point()+
                    theme_minimal()+
                    ggtitle("Yambuku Case-fatality Rate")+
                    ylab("Case-fatality rate (cfr)") + xlab("Date")+
                    theme(plot.title = element_text(color = "black", size = 14, 
                                                    face = "bold", hjust = 0.5),
                    axis.title.x = element_text(color = "black", size = 10, 
                                 face = "bold", hjust = 0.5),
                    axis.title.y = element_text(color = "black", size = 10, 
                                 face = "bold"))

#------------------------------------------------------------------------------#
#-----------------------'Yambuku gender ratio incidence'-----------------------#
  
# We can visualize the incidence based on the gender ratio. As we are dividing
# the sum of infected women by the sum of infected men, when the black line in
# the graph is above the dashed blue line, it means that more women were 
# infected than men in that specific week. 
# As the line is mostly above the dashed line, it can be concluded that more 
# women were usually infected by men, which relates to the location of the
# outbreak which was in a hospital where a lot of women worked as nurses
  
yambuku.gender.ratio.inc <- yambuku.data %>%   
                            mutate(week_onset = floor_date(disease_onset,
                                                           "week")) %>% 
                            filter(!is.na(sex)) %>%
                            group_by(week_onset) %>% 
                            mutate(sexratio = 
                                     sum(sex=="female")/sum(sex=="male")) %>%
                            arrange(week_onset) %>%
                            filter(row_number()==1 & !is.na(week_onset))

ggplot(yambuku.gender.ratio.inc, aes(x=week_onset, y=sexratio)) + 
        geom_line()+
        geom_hline(yintercept=1, linetype='dashed', col = '#6eb5ff')+
        geom_point()+ ylim(0,10)+
        theme_minimal()+
        ggtitle("Yambuku Sex Ratio Incidence")+
        ylab("Sex ratio") + xlab("Date")+
        theme(plot.title = element_text(color = "black", size = 14, 
                                        face = "bold", hjust = 0.5),
              axis.title.x =  element_text(color = "black", size = 10, 
                             face = "bold", hjust = 0.5),
              axis.title.y = element_text(color = "black", 
                                          size = 10,
                                          face = "bold"))

#------------------------------------------------------------------------------#
#---------------------'Sierra Leone gender ratio incidence'--------------------#
  
salone.gender.ratio.inc <- ebola_sierraleone_2014 %>%  
                           filter(!is.na(sex)) %>%
                           mutate(month_onset = 
                                    floor_date(date_of_onset, "month")) %>% 
                           group_by(month_onset) %>% 
                           mutate(sexratio = sum(sex=="F")/sum(sex=="M")) %>%
                           arrange(month_onset) %>%
                           filter(row_number()==1 & !is.na(month_onset))

ggplot(salone.gender.ratio.inc, aes(x=month_onset, y=sexratio)) + 
        geom_hline(yintercept=1, linetype='dashed', col = '#6eb5ff')+
        geom_line()+
        geom_point()+ylim(0,5)+
        ggtitle("Sierra Leone Sex Ratio Incidence")+
        theme_minimal() + ylab("Sex ratio") + xlab("Date")+
        theme(plot.title = element_text(color = "black", size = 14, 
                                        face = "bold", hjust = 0.5),
              axis.title.x = element_text(color = "black", size = 10, 
                             face = "bold", hjust = 0.5),
              axis.title.y = element_text(color = "black", 
                                          size = 10,
                                          face = "bold"))
                            
#------------------------------------------------------------------------------#
#----------------------'Estimating Reproductive Number'------------------------#
  
# To calculate the reproductive number, we need to first extract the number of 
# cases for each day which is done underneath by summing the cases for each 
# extracted disease_onset day
cumulative.disease.onset <- yambuku.data %>% 
                            dplyr::select(disease_onset) %>% 
                            group_by(disease_onset) %>% 
                            mutate(I=n()) %>%
                            arrange(disease_onset) %>%
                            filter(row_number()==1 & !is.na(disease_onset)) %>%
                            rename(dates = disease_onset)

all.dates <- data.frame(dates=seq(as.Date("1976-08-25"), 
                                 as.Date("1976-10-24"), by = "days"))

cumulative.disease.onset <- merge(cumulative.disease.onset, 
                                  all.dates, by.x = 'dates',
                                  by.y = 'dates', all.x = T, all.y = T)

cumulative.disease.onset$I[is.na(cumulative.disease.onset$I)] <- 0

# Now we can calculate the reproductive number using the estimate_R() function
res_parametric_si <- estimate_R(cumulative.disease.onset, 
                                method = "parametric_si",
                                config = make_config(list(
                                  mean_si = 15, # mean serial interval
                                  std_si = 10))) # standard serial interval 

# Reproductive number can be plotted
r.plotted <- plot(res_parametric_si, "R", 
                  options_R = list(col = "#FF7F50", transp = 0.2, xlim = NULL, 
                                                           ylim = NULL))+
             xlab("Date") + ylab("Reproductive number (R)")+
             theme_minimal()+
             theme(plot.title = 
                     element_text(color = "black", size = 14, face = "bold", 
                                  hjust = 0.5),
                   axis.title.x = element_text(color = "black", size = 10, 
                                               face = "bold", hjust = 0.5),
                   axis.title.y = element_text(color = "black",  size = 10, 
                                               face = "bold")) 

#-------------------------------'Sierra Leone'---------------------------------#
 
# Since the Yambuku data contains the type of transmission, calculating the
# reproductive number might not be as representative as possible because the
# most frequent type of transmission was from a syringe and not from a person to
# person. Hence, we will calculate the reproductive number for Sierra Leone
# outbreak as well. The procedure is exactly the same as with Yambuku
  
cumulative.disease.onset.sl <- ebola_sierraleone_2014 %>% 
                               dplyr::select(date_of_onset) %>% 
                               group_by(date_of_onset) %>% 
                               mutate(I=n()) %>%
                               arrange(date_of_onset) %>%
                               filter(row_number()==1 
                                      & !is.na(date_of_onset)) %>%
                               rename(dates = date_of_onset) 

all.dates.sl <- data.frame(dates = seq(as.Date("2014-05-18"), 
                                 as.Date("2015-09-12"), by = "days"))

cumulative.disease.onset.sl <- merge(cumulative.disease.onset.sl, 
                                     all.dates.sl, by.x = 'dates', 
                                     by.y='dates', all.x=T, all.y=T)

cumulative.disease.onset.sl$I[is.na(cumulative.disease.onset.sl$I)] <- 0

res_parametric_si.sl <- estimate_R(cumulative.disease.onset.sl, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 15, 
                                  std_si = 10)))

plot(res_parametric_si.sl, "R",
      options_R = list(col = "#FF7F50", transp = 0.2, xlim = NULL, 
                      ylim = NULL))+
      xlab("Date") + ylab("Reproductive number (R)")+
      theme_minimal()+
      theme(plot.title = 
              element_text(color = "black", size = 14, face = "bold", 
                           hjust = 0.5),
            axis.title.x = element_text(color = "black", size = 10, 
                                        face = "bold", hjust = 0.5),
            axis.title.y = element_text(color = "black",  size = 10, 
                                        face = "bold"))

#------------------------------------------------------------------------------#
#----------------------'Sierra Leone Chiefdom Incidence'-----------------------#
  
# First we calculate the number of cases for each chiefdom to produce a table
# where each line contains the name of the chiefdom and the number of cases.
# The cases are both confirmed and suspected
cumulative.chiefdom <- ebola_sierraleone_2014 %>% 
                            dplyr::select(chiefdom) %>% 
                            group_by(chiefdom) %>% 
                            mutate(I=n()) %>%
                            arrange(chiefdom) %>%
                            filter(row_number()==1 & !is.na(chiefdom)) %>%
                            rename(places = chiefdom)
  
# We sort the data in descending order 
sorted.data.chiefdom <- cumulative.chiefdom[order(-cumulative.chiefdom$I,
                                                  cumulative.chiefdom$places),]

# We remove the first two rows as they do not designate chiefdoms
sorted.data.chiefdom <- sorted.data.chiefdom[-c(1,2),]

# Since there are over 140 chiefdoms, we can focus on the first 10, so we remove
# those under the 10th row
first10places.chiefdom <- sorted.data.chiefdom[-c(11:140),]

# We can now plot a bar plot in descending order as well to show the different
# number of cases for the 10 most affected chiefdoms
plot.chiefdom <- ggplot(first10places.chiefdom, aes(x = reorder(places, -I), 
                                   y = I)) +    
                 geom_bar(stat = "identity", fill ="#6eb5ff", color ="black", 
                          width=0.9) +
                 theme(axis.text.x = element_text(angle = 90, size = 10))+
                 ylab("Incidence") + xlab("Chiefdom") +
                 scale_y_continuous(limits=c(0, 900),
                                    breaks = seq(0,900, by=100)) +
                 ggtitle("First 10 Most Affected Chiefdoms, Sierra Leone")+
                 theme_minimal()+
                 theme(axis.text.x = 
                         element_text(angle = 45, hjust = 1),
                       plot.title = element_text(color = "black", size = 14, 
                                                 face = "bold", hjust = 0.5),
                       axis.title.x = element_text(color = "black", size = 10, 
                                                   face = "bold", hjust = 0.5),
                       axis.title.y = element_text(color = "black", size = 10,
                                                   face = "bold"))
#------------------------------------------------------------------------------#