source("1_SetupImport.R")


##############################################################################################
#| FIGURE 4.1: "Proportion agreeing that the 'Government should reduce income differences' by year, 2002-2024" 

all_countries <- unique(df_ess$cntry)

select_countries <- c("IE", "PT", "NL", "GB")

df_ess$cntry_rlvl <- factor(df_ess$cntry, levels = c(all_countries[!all_countries %in% select_countries], select_countries))


fig_eutime <- 
    df_ess |>
        mutate(Country = factor(case_match(cntry,
            "IE" ~ "Ireland",
            "PT" ~ "Portugal",
            "NL" ~ "The Netherlands",
            "GB" ~ "United Kingdom",
            .default = "Other countries"),
            levels = c("Other countries", "Portugal", "Ireland", "United Kingdom", "The Netherlands"))) |> 
        group_by(Year = start_year, Country, cntry_rlvl)  |> 
        summarise(
            mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100,
            se = (sd(gincdif_agree, na.rm = T)/sqrt(length((gincdif_agree))))*100) |> 
        ggplot(aes(x = Year, y = mean, group = cntry_rlvl, color = Country)) +
        geom_line(linewidth = 0.8)+
        geom_dl(aes(label = Country), method = list('last.points', cex = 1, hjust = -0.1)) +
        scale_color_manual(values = c("grey90", "red",  "#396065", "blue", "#f26a4c"))+
        scale_x_continuous(breaks = seq(2002,2024, 2), limits = c(2002, 2028)) +
        labs(
            x = "Year", y = "Government should reduce income differences (%)"
        ) +
        theme_classic() +
        theme(
            legend.position = 'none',
            axis.text.y = element_text(size = 11),
            axis.text.x= element_text(size = 11, angle = 45, vjust = 1, hjust = 1),
        ) 

save_display(fig_eutime, w =7, h=5)

##############################################################################################
#| FIGURE 4.2: "Proportion agreeing that the 'Government should reduce income differences' by social class and year, Ireland, 2002-2024" 

# df_ess |> 
#     filter(cntry == 'IE' & class5  == 5) |> 
#     group_by(essround) |> 
#     summarise(mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T))


fig_time_class <- 
    df_ess |>
        filter(cntry == 'IE' & !is.na(class5)) |> 
        group_by(Year = start_year, class = as_factor(class5))  |> 
        summarise(
            mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100,
            se = (sd(gincdif_agree, na.rm = T)/sqrt(length((gincdif_agree))))*100) |> 
        ggplot(aes(x = Year, y = mean, group = class, color = class)) +
        geom_pointrange(aes(y=mean,  ymin = mean - 1.96*se, ymax = mean + 1.96*se), size = 0.2, colour = 'grey95') +
        geom_line(aes(linetype = class), linewidth = 0.8)+
        #geom_point(size = 1) +
        geom_dl(aes(label = class), method = list('last.points', cex = 0.9, hjust = -0.1)) +
        scale_color_manual(values = c("#396065", "darkblue", "#1fbec6", "#f26a4c", "maroon"))+
        scale_x_continuous(breaks = seq(2002,2024, 2), limits = c(2002, 2032)) +
        labs(
            x = "Year", y = "Government should reduce income differences (%)"
        ) +
        theme_classic() +
        theme(
            legend.position = 'none',
            axis.text.y = element_text(size = 11),
            axis.text.x= element_text(size = 11, angle = 45, vjust = 1, hjust = 1)
        ) 

save_display(fig_time_class, w =7, h=5)

##############################################################################################
#| FIGURE 4.3: "Monthly unemployment rate and moving average (3 months) of support for redistribution, Ireland, 2002-2023" 

df_cso <- read_csv("data/MUM01.20250522T130504.csv")

df_cso$ess_month = as.Date(paste0(df_cso$Month, " 01"), format = "%Y %B %d")

tb_unemp  <- 
    df_cso |> 
        select(ess_month, VALUE) |> 
        rename(cso_unemp = VALUE) |> 
        left_join(
            df_ess |> 
                filter(cntry == "IE") |> 
                #mutate(ess_week = strftime(as.Date(ess_day), format("%Y%V"))) |> 
                group_by(ess_month) |> 
                summarise(
                    n= n(),
                    mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*10,
                    se = sd(gincdif_agree, na.rm = T)/sqrt(length((gincdif_agree)))*10)
        )

fig_unemp <- 
    tb_unemp |> 
        filter(ess_month > "2002-01-01") |> 
        ggplot(aes(x = ess_month)) +
        geom_line(aes(y = cso_unemp)) +
        geom_point(aes(y=mean, size =n),  color = 'black', fill = 'grey80', alpha = 0.2, shape = 21) +
        #geom_pointrange(aes(y=mean,  ymin = mean - 1.96*se, ymax = mean + 1.96*se), size = 0.2, colour = 'grey70') +
        tidyquant::geom_ma(aes(y = mean), n = 4, linewidth = 0.8, colour = '#f26a4c')+
        scale_y_continuous(
            name = "Unemployment Rate",
            sec.axis = sec_axis(transform =~.*10,name="Support for redistribution")
        ) +
        labs(x = "Month") +
        theme_classic() +
        theme(
            axis.text.y = element_text(size = 12),
            plot.caption = element_text(hjust = 0),
            legend.position = 'none',
            axis.title.y.right = element_text(color = "#f26a4c")
        ) 

save_display(fig_unemp, w =7, h=5)

##############################################################################################
#| FIGURE 4.4: "Scatter plot for 'reduce income differences' by age and ESS starting year, Ireland"

fig_age_round <- 
    df_ess |> 
        filter(cntry == 'IE') |> 
        group_by(Round = start_year, agea) |> 
        summarise(mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100) |> 
        ggplot(aes(x = agea, y = mean)) +
        geom_point(size = 0.5) +
        geom_smooth(method = 'lm', color = "#f26a4c") +
        #ylim(0.25,1) +
        facet_wrap(~Round, nrow=3) + 
        labs(
            x = "Age", 
            y = "Government should reduce income differences (%)"
        )+
        theme_classic() 

save_display(fig_age_round, w=6, h=6)

##############################################################################################
#| FIGURE 4.5: "Support for redistribution over time by pseudo-cohort and gender, Ireland"

fig_cohort <- 
    df_ess |> 
        filter(false_cohort %in% c("1941-1960", "1961-1980", "1981-") & cntry == 'IE') |> 
        group_by(start_year, false_cohort, gender = as_factor(gndr)) |>
        summarise(mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100) |> 
        ggplot(aes(x = start_year, y = mean, colour = gender, fill = gender)) +
        geom_line()+
        geom_point() +
        facet_wrap(~false_cohort, ncol = 1)+
        scale_color_manual(values = c("#f26a4c", "#1F355E"))+
        scale_x_continuous(breaks = seq(2002,2024, 2), limits = c(2002, 2025)) +
        labs(   
                color = "Gender", fill = "Gender", 
                x = "Year", y = "Government should reduce income differences (%)"
            ) +
        theme_classic() +
        theme(
            legend.position = 'bottom',
            legend.text = element_text(size = 13),
            strip.text = element_text(size = 13),
            axis.text.y = element_text(size = 11),
            axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust = 1)
        ) 

save_display(fig_cohort, w =5, h=7)

##############################################################################################
#| FIGURE 4.6: "7-day exponential moving average (EMA) for three questions on welfare attitudes, Ireland, 2016-2017"

df_ie_8 <- df_ess |> filter(cntry == 'IE' & essround == 8)

fig_dsp_mean <- 
    df_ie_8 |>
            filter(abs(dsp_daysafter) <= 30) |> 
            group_by(dsp_daysafter) |> 
            summarise(
                redist = weighted.mean(gincdif_inv, na.rm = T),
                lazy = weighted.mean(sblazy_inv, na.rm = T),
                pov = weighted.mean(sbprvpv_inv, na.rm = T))|> 
            pivot_longer(cols = -dsp_daysafter, names_to = 'Statement', values_to = 'value') |> 
            mutate(Statement = case_match(Statement,
                "redist" ~ "Government should reduce income differences",
                "lazy" ~ "Social benefits make people lazy",
                "pov" ~ "Social benefits prevent poverty"
            )) |> 
            ggplot(aes(x = dsp_daysafter, y = value)) +
            tidyquant::geom_ma(n = 5, ma_fun = EMA, color = '#1fbec6', linewidth = 0.8) +
            geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
            facet_wrap(~Statement, ncol = 1) +
            theme_classic() +
            labs(   
                x = "Days after campaign launch", y = "Mean of agreement"
            ) +
            theme_classic() +
            theme(
                axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                strip.text = element_text(size = 13),
                plot.caption = element_text(hjust = 0)
            ) 

save_display(fig_dsp_mean, w = 5, h = 7)


##############################################################################################
# FIGURE 4.7: "Linear regression estimates for period of interview, Ireland, 2016-2017"
df_ie_8$dsp_period <- factor(df_ie_8$dsp_period, levels = c("Before", "After", ""))

# Calculating mid-point of the fieldwork
dsp_midpoint_date <- df_ie_8 |> filter(news == 'Yes') |> 
    summarise(min = min(ess_day), max = max(ess_day)) |> 
    mutate(midpoint = min + (max-min)/2) |> 
    pull(midpoint)

df_ie_8 <- df_ie_8 |> mutate(
    dsp_midpoint_period = case_when(
        essround != 8 ~ NA_character_,
        ess_day <= dsp_midpoint_date ~ "Before",
        ess_day > dsp_midpoint_date ~ "After"),
    dsp_midpoints_dafter = as.numeric(ess_day - dsp_midpoint_date)
    )


# Getting variable for news consumption
df_ie_8 <- 
    df_ie_8 |> 
        mutate(
            news = case_when(
                essround < 8 & tvpol == 0 ~ "No",
                essround < 8 & tvpol > 0 ~ "Yes",
                essround > 7 & nwspol == 0 ~ "No",
                essround > 7 & nwspol > 0 ~ "Yes",
                .default = NA           
                ))

# get_model(
#         df = df_ess |> filter(cntry == "IE" & essround == 8 & abs(dsp_daysafter) <=14 & news == 'Yes'),
#         dv = "dsp_period == 'After'",
#         ivs = c('factor(mnactic)', 'agea', 'as_factor(gndr)', 'as_factor(class5)'),
#         type = 'glm'
#     )  |> tbl_regression(exponentiate = TRUE)

# df_ess |> filter(cntry == "IE" & essround == 8) |> count(news)

# df_ess |> filter(cntry == "IE" & essround == 8) |> group_by(dsp_period) |> count(is.na(gincdif)) |> mutate(p = prop.table(n))
# df_ess |> filter(cntry == "IE" & essround == 8) |> group_by(dsp_period) |> count(is.na(sblazy)) |> mutate(p = prop.table(n))
# df_ess |> filter(cntry == "IE" & essround == 8) |> group_by(dsp_period) |> count(is.na(sbprvpv)) |> mutate(p = prop.table(n))


# df_ess |> 
#     filter(cntry == 'IE' & essround == 8 & news == 'Yes') |> 
#     summarise(
#         full = n(),
#         thirty = sum(abs(dsp_daysafter) <= 15),
#         twenty = sum(abs(dsp_daysafter) <= 10))
library(broom.helpers)


get_coef_period <- function(outcomes, indep_vars, timevar, period){
    map_dfr(
            outcomes,
            function(x){
                map_dfr(
                    period,
                    function (y) {
                        tidy_and_attach(get_model(
                            df = df_ess |> filter(cntry == 'IE' & essround == 8 & news == 'Yes' & abs(get(timevar)) <= y ),
                            dv = x,
                            ivs = indep_vars,
                            type = 'glm')) |> 
                        tidy_add_n() |> 
                        mutate(dv = x, period = y)
                    })
            })
}


dsp_models <- get_coef_period(
    outcomes = c('gincdif_agree', 'sblazy_agree', 'sbprvpv_agree'),
    indep_vars = c('dsp_period', 'as_factor(mnactic)', 'agea', 'as_factor(gndr)'),
    timevar = 'dsp_daysafter',
    period = c(300, 15,10))



fig_dsp_model <- 
    dsp_models |>
        mutate(
            variable = case_match(
                dv, 
                'gincdif_agree' ~ 'Support for\nredistribution', 
                'sblazy_agree' ~ "Social benefits\nmake people lazy",
                'sbprvpv_agree' ~ "Social benefits\nprevent poverty"),
            period = case_match(
                period,
                300 ~ 'Full sample',
                15 ~ '30 days',
                10 ~ '20 days'),
            term = if_else(term == 'dsp_periodAfter', "After camp. launch", term)
        ) |> 
        filter(term %in% c('After camp. launch')) |> 
        ggplot(aes(y = term, fill = period, color = period)) +
        geom_pointrange(aes(x = estimate, xmin = estimate -1.96*std.error, xmax = estimate +1.96*std.error),
                position = position_dodge(width = 0.4)) +
        geom_vline(aes(xintercept = 0), color = '#f26a4c', linetype = 'dashed') +
        facet_wrap(~variable, ncol = 3) +
        theme_classic() +
        labs(x = "Effect on outcome", y = "", color = "Time-window", fill = "Time-window") +
        theme(
            axis.text.y = element_text(size = 10),
            legend.position = 'bottom'
        ) 

save_display(fig_dsp_model, w = 6, h=5)




##############################################################################################
#| FIGURE 4.8: "7-day moving average for support for redistribution within the 30-day period of the public budget announcement, Ireland"

#tbl_merge(map(dsp_models, ~tbl_regression(.x) |> bold_p()), tab_spanner = c("Redistribution", "Lazy", "Poverty"))

df_ess |> filter(cntry == 'IE') |> group_by(essround) |> summarise(weighted.mean(gincdif_agree, w = pspwght, na.rm = T))

ess_serie <- tibble(day = seq(from = as.Date('2002-01-02'), to=as.Date('2023-12-31'), by = 'day'))


budget_days <- tibble(budget_date = as.Date(c(
    "2002-12-06", "2003-12-03", "2004-12-01", "2005-12-07",
    "2006-12-06", "2007-12-05", "2008-10-14", "2009-12-09", "2010-12-07", 
    "2011-12-06", "2012-12-05", "2013-10-15", "2014-10-14", "2015-10-13", 
    "2016-10-11", "2017-10-10", "2018-10-09", "2019-10-08", "2020-10-13",
    "2021-10-12", "2022-09-27", "2023-10-10"), format = "%Y-%m-%d"))



df_ess <- 
    df_ess |> 
        mutate(
            budget_period = if_else(budget_daysafter < 0, "Control", "Treatment")
            )

fig_budget_mean <- 
    df_ess |> 
        filter(!is.na(budget_daysafter) & news == 'Yes' & cntry == 'IE' & abs(budget_daysafter) <= 30 ) |> 
        group_by(start_year, budget_daysafter) |> 
        summarise(
            mean = weighted.mean(gincdif_inv, na.rm = T),
            se = sd(gincdif_inv, na.rm = T)/sqrt(length((gincdif_inv)))) |> 
        ggplot(aes(x = budget_daysafter, y = mean)) +
        tidyquant::geom_ma(n = 7, ma_fun = EMA, color = '#1fbec6', linewidth = 0.6) +
        geom_vline(xintercept = 0, color = '#f26a4c', linetype = 'dashed') +
        facet_wrap(~start_year, ncol = 2) +
        theme_classic()+
        labs(x = "Days after budget announcement", y = "Mean value") +
        theme(
            axis.text.y = element_text(size = 12),
            plot.caption = element_text(hjust = 0)
        ) 

save_display(fig_budget_mean, w = 6, h = 6)

##############################################################################################
#| FIGURE 4.9: "Linear regression estimates for being interviewed after the budget announcement by time window, Ireland"


fig_budget_model <- 
    map_dfr(
        c('scale(gincdif_inv)', 'scale(stfgov)'), #c(2006, 2009, 2011, 2012, 2014, 2023)
        function(x){
            map_dfr(
                c(seq(5,40)),
                function (y) {
                    broom::tidy(get_model(
                        df = df_ess |> filter(cntry == 'IE' & news == 'Yes' & abs(budget_daysafter) < y ),
                        dv = x,
                        ivs = c('budget_period', 'factor(mnactic)', 'agea',  'gndr', 'as_factor(class5)', 'as_factor(essround)'),
                        type = 'lm')) |> mutate(dv = x, bandwidth = y)
                })
        }) |> 
       mutate(variable = if_else(dv == 'scale(gincdif_inv)', 'Support for redistribution', "Satisfaction with government")) |> 
        filter(term == 'budget_periodTreatment') |> 
        ggplot(aes(x = bandwidth)) +
        geom_line(aes(y = estimate)) +
        geom_line(aes(y = estimate +1.96*std.error), linetype = 'dotted') +
        geom_line(aes(y = estimate -1.96*std.error), linetype = 'dotted') +
        geom_hline(aes(yintercept = 0), color = '#f26a4c', linetype = 'dashed') +
        facet_wrap(~variable, ncol = 2) +
        theme_classic() +
        labs(x = "Days before and after the announcement", y = "Estimate") +
        theme(
            axis.text.y = element_text(size = 10)
        ) 

save_display(fig_budget_model, w = 6, h = 5)

##############################################################################################
#| FIGURE 4.10: "Proportion of support for redistribution by social class and personal impact of the pandemic, Ireland, 2021"

fig_covid <- 
    df_ess |> 
        filter(!is.na(class5) & cntry == 'IE' & essround == 10) |> 
        mutate(
            impact = case_when(
                hapljc19 == 1 | hapfoc19 == 1 | hapfuc19 == 1 ~ "Lost job\nUnpaid leave\nForloughed",
                hapirc19 == 1 | hapwrc19 == 1 ~ "Income or hours\nreduced",
                hapnoc19 == 1 ~ "Nothing",
                is.na(hapnoc19) ~ NA_character_,
                .default = "Other/DK\nRefusal"
            )
        ) |> 
        group_by(impact, class = as_factor(class5)) |> 
        summarise(
            mean = weighted.mean(gincdif_agree, w = anweight, na.rm = T)*100,
            total = n(),
            se = sd(gincdif_agree, na.rm = T)/sqrt(length((gincdif_agree)))) |> 
        ggplot(aes(x = impact, y = class))+
        geom_tile(colour = 'grey70', fill = 'white') +
        geom_point(aes(color = mean, size = total), shape = 15) +
        geom_text(aes(label = round(mean)), color = 'grey20', size = 3, hjust = 2) +
        scale_color_distiller(palette = "RdBu", direction=-1) + 
        scale_size(range = c(2, 10), guide = 'none') +
        labs(x = "", y = '', color = 'Support for\nredistribution (%)') +
        theme_minimal() +
        theme(
            legend.position = 'top',
            legend.title = element_text(size = 10),
            axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5),
            axis.text.y = element_text(size = 10),
        ) 

save_display(fig_covid, w = 6, h = 4)