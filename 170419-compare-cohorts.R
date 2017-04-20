################################################################################
#
# Supercentenarians 2017-04-19
# Check the cohort effect 1870-s VS 1880-s (USA)
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#
################################################################################

# In order to get the script working, set the working directory to the one
# in which you have just unziped the reproducibility archive
setwd('')

# The code is written and tested on a PC-win7-x64
# R version 3.3.3

# load required packages
# if needed, first install the listed below packages
library(tidyverse) # version 1.1.1
library(HMDHFDplus) # version 1.1.8
library(RColorBrewer) # version 1.1-2
require(ggthemes) # version 3.4.0
library(extrafont) # version 0.17
myfont <- "Roboto Condensed" 
library(cowplot) # version 0.7.0



################################################################################
# CHeck the cohort effect 1870-s VS 1880-s USA

# read HMD data directly into R
# please note! the arguments "ik_user_hmd" and "ik_pass_hmd" are my login credidantials
# at the website of Human Mortality Database, which are stored locally at my computer. 
# In order to access the data, you need to create an account at
# http://www.mortality.org/
# and provide your own credidantials to the `readHMDweb()` function
usa <- readHMDweb(CNTRY = 'USA', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

usa_check <- usa %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(usa_check$period) <- c('1870-s', '1880-s')

gg_usa <- ggplot(usa_check, aes(age,mx))+
        geom_point(aes(color=period),size=3)+
        geom_path(aes(group=period,color=period),size=1)+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('Age')+
        ylab('Cohort mortality rate')+
        theme_few(base_family = myfont, base_size = 20)+
        theme(legend.position = 'none',
              aspect.ratio = 1)+
        annotate('text', x=c(100,105), y=c(.5,.35), hjust=0,vjust=1, size=8,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)], family = myfont)+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=10, label='Unites States',
                 family = myfont)


################################################################################
# The  additional check of the statistical significance
# of the difference between cohort born in 1870-s and 1880-s
usa_sign <- usa %>% transmute(year=Year, AGE=Age, MX=Female) %>%
        filter(year %in% 1870:1889, AGE %in% 100:110) %>%
        mutate(COH = cut_number(year, 2))
levels(usa_sign$COH) <- c('1870-s', '1880-s')

# glance at the rates
ggplot(usa_sign, aes(AGE, MX, group=COH,color=COH))+
        geom_jitter()+
        stat_smooth(method = 'lm', se = F)

# linear model to chech the significance
summary(lm(data = usa_sign, formula = MX ~ AGE + COH))

# RESULT: the dummy for birth decade (COH) is highly significant






################################################################################
# CHeck the cohort effect 1870-s VS 1880-s JPN

jpn <- readHMDweb(CNTRY = 'JPN', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

jpn_check <- jpn %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(jpn_check$period) <- c('1870-s', '1880-s')

gg_jpn <- ggplot(jpn_check, aes(age,mx))+
        geom_point(aes(color=period),size=2)+
        geom_path(aes(group=period,color=period))+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('')+
        ylab('')+
        theme_few(base_family = myfont, base_size = 14)+
        theme(legend.position = 'none',
              aspect.ratio = 1)+
        annotate('text', x=c(105,98), y=c(.45,.65), hjust=0,vjust=1, size=5,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)],
                 family = myfont)+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=7, label='Japan',
                 family = myfont)






################################################################################
# CHeck the cohort effect 1870-s VS 1880-s FRA

fra <- readHMDweb(CNTRY = 'FRATNP', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

fra_check <- fra %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(fra_check$period) <- c('1870-s', '1880-s')

gg_fra <- ggplot(fra_check, aes(age,mx))+
        geom_point(aes(color=period),size=2)+
        geom_path(aes(group=period,color=period))+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('')+
        ylab('')+
        theme_few(base_family = myfont, base_size = 14)+
        theme(legend.position = 'none',
              aspect.ratio = 1)+
        annotate('text', x=c(105,98), y=c(.45,.65), hjust=0,vjust=1, size=5,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)],
                 family = myfont)+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=7, label='France',
                 family = myfont)




################################################################################
# CHeck the cohort effect 1870-s VS 1880-s SWE

swe <- readHMDweb(CNTRY = 'SWE', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

swe_check <- swe %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(swe_check$period) <- c('1870-s', '1880-s')

gg_swe <- ggplot(swe_check, aes(age,mx))+
        geom_point(aes(color=period),size=2)+
        geom_path(aes(group=period,color=period))+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('')+
        ylab('')+
        theme_few(base_family = myfont, base_size = 14)+
        theme(legend.position = 'none',
              aspect.ratio = 1)+
        annotate('text', x=c(105,98), y=c(.45,.65), hjust=0,vjust=1, size=5,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)],
                 family = myfont)+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=7, label='Sweden',
                 family = myfont)



################################################################################
# CHeck the cohort effect 1870-s VS 1880-s GBR

gbr <- readHMDweb(CNTRY = 'GBR_NP', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

gbr_check <- gbr %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(gbr_check$period) <- c('1870-s', '1880-s')

gg_gbr <- ggplot(gbr_check, aes(age,mx))+
        geom_point(aes(color=period),size=2)+
        geom_path(aes(group=period,color=period))+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('')+
        ylab('')+
        theme_few(base_family = myfont, base_size = 14)+
        theme(legend.position = 'none',
              aspect.ratio = 1)+
        annotate('text', x=c(105,98), y=c(.45,.65), hjust=0,vjust=1, size=5,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)],
                 family = myfont)+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=7, label='Great Britain',
                 family = myfont)



################################################################################
# joined plot

four <- plot_grid(gg_fra, gg_gbr, gg_jpn, gg_swe, ncol = 2, align = "hv")

final_h <- plot_grid(gg_usa, four, ncol = 2, rel_widths = c(.5, .5), rel_heights = c(.5, .5))
final_v <- plot_grid(gg_usa, four, ncol = 1, rel_widths = c(.5, .5), rel_heights = c(.5, .5))

ggsave('compare-cohorts-horizontal.png', final_h, width = 15, height = 8, type="cairo-png")
ggsave('compare-cohorts-vertical.png', final_v, width = 8, height = 15, type="cairo-png")

# That's it .)
