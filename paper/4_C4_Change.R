source("paper/1_SetupImport.R")


##############################################################################################
#| FIGURE 4.1: "Proportion agreeing that the "Government should reduce income differences" by year, 2002-2024"

all_countries <- unique(df_ess$cntry)

select_countries <- c("IE", "PT", "NL", "GB")

df_ess$cntry_rlvl <- factor(df_ess$cntry, levels = c(all_countries[!all_countries %in% select_countries], select_countries))


fig_c4_eutime <-
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
            mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = TRUE)*100,
            se = (sd(gincdif_agree, na.rm = TRUE)/sqrt(length((gincdif_agree))))*100) |>
        ggplot(aes(x = Year, y = mean, group = cntry_rlvl, color = Country)) +
        geom_line(linewidth = 0.8) +
        geom_dl(aes(label = Country), method = list("last.points", cex = 1, hjust = -0.1)) +
        scale_color_manual(values = c("grey90", "red",  "#396065", "blue", "#f26a4c"))+
        scale_x_continuous(breaks = seq(2002,2024, 2), limits = c(2002, 2028)) +
        labs(
            x = "Year", y = "Government should reduce income differences (%)"
        ) +
        theme_classic() +
        theme(
            legend.position = "none",
            axis.text.y = element_text(size = 11),
            axis.text.x = element_text(size = 11, angle = 45, vjust = 1, hjust = 1),
        )

save_output(fig_c4_eutime, w =7, h=5)

##############################################################################################
#| FIGURE 4.2: "Proportion agreeing that the "Government should reduce income differences" by social class and year, Ireland, 2002-2024"

# df_ess |>
#     filter(cntry == "IE" & class5  == 5) |>
#     group_by(essround) |>
#     summarise(mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T))


fig_c4_time_class <-
    df_ess |>
        filter(cntry == "IE" & !is.na(class5)) |>
        group_by(Year = start_year, class = as_factor(class5))  |>
        summarise(
            mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100,
            se = (sd(gincdif_agree, na.rm = T)/sqrt(length((gincdif_agree))))*100) |>
        ggplot(aes(x = Year, y = mean, group = class, color = class)) +
        geom_pointrange(aes(y=mean,  ymin = mean - 1.96*se, ymax = mean + 1.96*se), size = 0.2, colour = "grey95") +
        geom_line(aes(linetype = class), linewidth = 0.8)+
        #geom_point(size = 1) +
        geom_dl(aes(label = class), method = list("last.points", cex = 0.9, hjust = -0.1)) +
        scale_color_manual(values = c("#396065", "darkblue", "#1fbec6", "#f26a4c", "maroon"))+
        scale_x_continuous(breaks = seq(2002,2024, 2), limits = c(2002, 2032)) +
        labs(
            x = "Year", y = "Government should reduce income differences (%)"
        ) +
        theme_classic() +
        theme(
            legend.position = "none",
            axis.text.y = element_text(size = 11),
            axis.text.x= element_text(size = 11, angle = 45, vjust = 1, hjust = 1)
        )

save_output(fig_c4_time_class, w =7, h=5)

##############################################################################################
#| FIGURE 4.3: "Monthly unemployment rate and moving average (3 months) of support for redistribution, Ireland, 2002-2023"

df_cso <- read_csv("data/raw/MUM01.20250522T130504.csv")

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

fig_c4_unemp <-
    tb_unemp |>
        filter(ess_month > "2002-01-01") |>
        ggplot(aes(x = ess_month)) +
        geom_line(aes(y = cso_unemp)) +
        geom_point(aes(y=mean, size =n),  color = "black", fill = "grey80", alpha = 0.2, shape = 21) +
        #geom_pointrange(aes(y=mean,  ymin = mean - 1.96*se, ymax = mean + 1.96*se), size = 0.2, colour = "grey70") +
        tidyquant::geom_ma(aes(y = mean), n = 4, linewidth = 0.8, colour = "#f26a4c")+
        scale_y_continuous(
            name = "Unemployment Rate",
            sec.axis = sec_axis(transform =~.*10,name="Support for redistribution")
        ) +
        labs(x = "Month") +
        theme_classic() +
        theme(
            axis.text.y = element_text(size = 12),
            plot.caption = element_text(hjust = 0),
            legend.position = "none",
            axis.title.y.right = element_text(color = "#f26a4c")
        )

save_output(fig_c4_unemp, w =7, h=5)

##############################################################################################
#| FIGURE 4.4: "Scatter plot for "reduce income differences" by age and ESS starting year, Ireland"

fig_c4_age_round <-
    df_ess |>
        filter(cntry == "IE") |>
        group_by(Round = start_year, agea) |>
        summarise(mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100) |>
        ggplot(aes(x = agea, y = mean)) +
        geom_point(size = 0.5) +
        geom_smooth(method = "lm", color = "#f26a4c") +
        #ylim(0.25,1) +
        facet_wrap(~Round, nrow=3) +
        labs(
            x = "Age",
            y = "Government should reduce income differences (%)"
        )+
        theme_classic()

save_output(fig_c4_age_round, w=6, h=6)

##############################################################################################
#| FIGURE 4.5: "Support for redistribution over time by pseudo-cohort and gender, Ireland"

fig_c4_cohort <-
    df_ess |>
        filter(false_cohort %in% c("1941-1960", "1961-1980", "1981-") & cntry == "IE") |>
        group_by(start_year, false_cohort, gender = as_factor(gndr)) |>
        summarise(mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100) |>
        ggplot(aes(x = start_year, y = mean, colour = gender, fill = gender)) +
        geom_line() +
        geom_point() +
        facet_wrap(~false_cohort, ncol = 1)+
        scale_color_manual(values = c("#f26a4c", "#1F355E")) +
        scale_x_continuous(breaks = seq(2002, 2024, 2), limits = c(2002, 2025)) +
        labs(
                color = "Gender", fill = "Gender",
                x = "Year", y = "Government should reduce income differences (%)"
            ) +
        theme_classic() +
        theme(
            legend.position = "bottom",
            legend.text = element_text(size = 13),
            strip.text = element_text(size = 13),
            axis.text.y = element_text(size = 11),
            axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust = 1)
        )

save_output(fig_c4_cohort, w =5, h=7)

##############################################################################################
#| FIGURE 4.6: "7-day exponential moving average (EMA) for three questions on welfare attitudes, Ireland, 2016-2017"


fig_c4_dsp_mean <-
    df_ess |>
            filter(abs(dsp_daysafter) <= 30) |>
            group_by(dsp_daysafter) |>
            summarise(
                redist = weighted.mean(gincdif_inv, na.rm = TRUE),
                lazy = weighted.mean(sblazy_inv, na.rm = TRUE),
                pov = weighted.mean(sbprvpv_inv, na.rm = TRUE))|>
            pivot_longer(cols = -dsp_daysafter, names_to = "Statement", values_to = "value") |>
            mutate(Statement = case_match(Statement,
                "redist" ~ "Government should reduce income differences",
                "lazy" ~ "Social benefits make people lazy",
                "pov" ~ "Social benefits prevent poverty"
            )) |>
            ggplot(aes(x = dsp_daysafter, y = value)) +
            tidyquant::geom_ma(n = 5, ma_fun = EMA, color = "#1fbec6", linewidth = 0.8) +
            geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
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

save_output(fig_c4_dsp_mean, w = 5, h = 7)


##############################################################################################
# FIGURE 4.7: "Linear regression estimates for period of interview, Ireland, 2016-2017"

# Getting variable for news consumption
df_ess <-
    df_ess |>
        mutate(
            news = case_when(
                essround < 8 & tvpol == 0 ~ "No",
                essround < 8 & tvpol > 0 ~ "Yes",
                essround > 7 & nwspol == 0 ~ "No",
                essround > 7 & nwspol > 0 ~ "Yes",
                .default = NA
                ))

# # Checking balance

# check_balance <- function(df, var_treat, covars, time_window = NULL){

#   m_form <- as.formula(paste0(var_treat, " ~ ", paste(covars, collapse = " + ")))

#   model <- glm(
#     formula = m_form,
#     family = "quasibinomial",
#     data = df |> filter(
#       cntry == "IE",
#       essround == 8,
#       if(!is.null(time_window)){abs(dsp_daysafter) <= time_window} else TRUE
#     )
#   )

#   broom::tidy(model) |>
#     mutate(period = if(!is.null(time_window)){as.character(time_window)} else "full sample")
# }

# map_dfr(
#   c("full sample", 15, 10, 5),
#   ~check_balance(df_ess, "dsp_period == 'After'", vars_indo[vars_indo != "as_factor(essround)"], time_window = .x),
#   .null =
# ) |> arrange(p.value)

get_event_coeff <- function(df, event_date, round, outcome, event_window, covars) {
  
    # Get event data
    df_event <- df |>
        filter(
            cntry == "IE",
            essround == round,
            ess_day != as.Date(event_date),
            news == "Yes",
            if_all(all_of(covars), ~ !is.na(.))
        ) |>
        mutate(
            treated_group = if_else(ess_day > as.Date(event_date), 1, 0, missing = NA),
            treated_days  = as.numeric(ess_day - as.Date(event_date))
        )
    
    # Filter event data for model
    df_clipped <- df_event |> 
        filter(!is.na(get(outcome)) & abs(treated_days) <= event_window)



    # Entropy balancing control/treatment
    wt_event <- weightit(formula = as.formula(paste("treated_group ~", paste(covars, collapse = " + "))),
                       data = df_clipped,
                       estimand = "ATT",
                       method = "ebal")

    # Adjust outcome
    df_clipped  <- df_clipped |> mutate(dv = factor(get(outcome)))

    # Model fitting
    model_event <- ordinal::clm(
        formula = dv ~ treated_group * treated_days,
        weights = wt_event$weights,
        data = df_clipped
    )
    
    print(paste0(event_window, " days: ", nrow(df_clipped)))

    # Return results
    broom::tidy(model_event) |>
        mutate(
            variable = outcome,
            period = paste0(event_window, "d"),
            round = round
        )
}

dsp_launch_date  <-  "2017-04-17"

tb_dsp_model <- 
    map_dfr(c("gincdif_inv",  "sbprvpv_inv"), function(outcome) {
            map_dfr(c(150, 14, 7), function(n_days) {
                get_event_coeff(
                    df = df_ess,
                    event_date = dsp_launch_date,
                    round = 8,
                    outcome = outcome,
                    event_window = n_days,
                    covars = c("gndr", "age_group", "mnactic")
                )
            })
        })
    
fig_c4_dsp_model <-
    tb_dsp_model |>
        filter(term %in% c("treated_group", "treated_group:treated_days")) |>
        mutate(
            variable = case_when(
                str_detect(variable, "sbprvpv") ~ "Social benefits prevent poverty",
                str_detect(variable, "gincdif") ~ "Government should reduce income differences"
            ),
            term = case_match(term, "treated_group" ~ "Treatment group", "treated_group:treated_days" ~ "Treated*Days"),
            period = case_match(period, "7d" ~ "\u00B1 1 week\n (n=266)", "14d" ~ "\u00B1 2 weeks\n (n=676)", "150d" ~ "Full sample\n (n=2,368)")
        ) |>
            ggplot(aes(y = term, color = period)) +
            geom_pointrange(aes(x = estimate, xmin = estimate -1.96*std.error, xmax = estimate +1.96*std.error),
                            position = position_dodge(width = 0.4)) +
            geom_vline(aes(xintercept = 0), color = "#f26a4c", linetype = "dashed") +
            theme_classic() +
            facet_wrap(~variable, ncol = 1) +
            labs(x = "Estimate", y = "", 
                color = "Time from launch: ", fill = "Time from launch: ") +
            theme(
            legend.position = "bottom"
            )

save_output(fig_c4_dsp_model, w = 5.5, h = 5)


# # Checking difference in midpoint in the control group
# dsp_placebo_date <- 
#     df_ess |> 
#     filter(cntry == 'IE' & essround == 8 & ess_day < '2017-04-17') |> 
#     summarise(m = median(ess_day)) |> 
#     pull(m)

# map_dfr(c("gincdif_inv",  "sbprvpv_inv"), function(outcome) {
#             map_dfr(c(150, 14, 7), function(n_days) {
#                 get_event_coeff(
#                     df = df_ess |> filter(ess_day < "2017-04-17"),
#                     event_date = dsp_placebo_date,
#                     round = 8,
#                     outcome = outcome,
#                     event_window = n_days,
#                     covars = c("gndr", "age_group", "mnactic")
#                 )
#             })
#         }) |> 
#         filter(p.value <=0.05) |> 
#         view()


##############################################################################################
#| FIGURE 4.8: "7-day moving average for support for redistribution within the 30-day period of the public budget announcement, Ireland"

#tbl_merge(map(dsp_models, ~tbl_regression(.x) |> bold_p()), tab_spanner = c("Redistribution", "Lazy", "Poverty"))

#df_ess |> filter(cntry == "IE") |> group_by(essround) |> summarise(weighted.mean(gincdif_agree, w = pspwght, na.rm = T))

ess_serie <- tibble(day = seq(from = as.Date("2002-01-02"), to=as.Date("2023-12-31"), by = "day"))


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

fig_c4_budget_mean <-
    df_ess |>
        filter(!is.na(budget_daysafter) & news == "Yes" & cntry == "IE" & abs(budget_daysafter) <= 30 ) |>
        group_by(start_year, budget_daysafter) |>
        summarise(
            mean = weighted.mean(gincdif_inv, na.rm = TRUE),
            se = sd(gincdif_inv, na.rm = TRUE)/sqrt(length((gincdif_inv)))) |>
        ggplot(aes(x = budget_daysafter, y = mean)) +
        tidyquant::geom_ma(n = 7, ma_fun = EMA, color = "#1fbec6", linewidth = 0.6) +
        geom_vline(xintercept = 0, color = "#f26a4c", linetype = "dashed") +
        facet_wrap(~start_year, ncol = 2) +
        theme_classic()+
        labs(x = "Days after budget announcement", y = "Mean value") +
        theme(
            axis.text.y = element_text(size = 12),
            plot.caption = element_text(hjust = 0)
        )

save_output(fig_c4_budget_mean, w = 6, h = 6)

#df_ess |> filter(ess_day == "2023-10-10") |> count(essround)

##############################################################################################
#| FIGURE 4.9: "Linear regression estimates for being interviewed after the budget announcement by time window, Ireland"

fig_c4_budget_model <- 
    map_dfr(
        c(10, 14, 30),
        function(n_days){
            map2_dfr(
                c("2009-12-09", "2011-12-06", "2012-12-05", "2014-10-14", "2023-10-10"),
                c(4, 5, 6, 7, 11),
                ~ get_event_coeff(
                    df = df_ess,
                    event_date = .x,
                    round = .y,
                    outcome = "gincdif_inv",
                    event_window = n_days,
                    covars = c("age_group", "gndr", "mnactic", "class5", "lrscale")
                ) |> mutate(year = substr(.x, 1, 4)))
        }) |>
        filter(term %in% c("treated_group",  "treated_group:treated_days")) |>
            mutate(
                term = case_match(term, "treated_group" ~ "Treatment group", "treated_group:treated_days" ~ "Treated*Days"),
                year = factor(year),
                period = case_match(period,"10d" ~ "\u00B1 10 days", "14d" ~ "\u00B1 2 weeks", "30d" ~ "\u00B1 30 days")
            ) |>
            ggplot(aes(y = term, color = year)) +
            geom_pointrange(aes(x = estimate, xmin = estimate -1.96*std.error, xmax = estimate +1.96*std.error),
                            position = position_dodge(width = 0.4), size = 0.2) +
            geom_vline(aes(xintercept = 0), color = "#f26a4c", linetype = "dashed") +
            theme_classic() +
            facet_wrap(~period) +
            labs(x = "Estimate", y = "", 
                color = "Year: ") +
            theme(
            legend.position = "bottom"
            )

save_output(fig_c4_budget_model, w = 6, h = 4)


fig_c4_budget_model <- 
    map_dfr(
        c(10, 14, 150),
        function(n_days){
            map2_dfr(
                c("2009-12-09", "2011-12-06", "2012-12-05", "2014-10-14", "2023-10-10"),
                c(4, 5, 6, 7, 11),
                ~ get_event_coeff(
                    df = df_ess,
                    event_date = .x,
                    round = .y,
                    outcome = "gincdif_inv",
                    event_window = n_days,
                    covars = c("age_group", "gndr", "mnactic")
                ) |> mutate(year = substr(.x, 1, 4)))
        }) |>
        filter(term %in% c("treated_group",  "treated_group:treated_days")) |>
            mutate(
                term = case_match(term, "treated_group" ~ "Treatment group", "treated_group:treated_days" ~ "Treated*Days"),
                year = factor(year),
                period = case_match(period,"10d" ~ "\u00B1 10 days", "14d" ~ "\u00B1 2 weeks", "150d" ~ "Full sample")
            ) |>
            ggplot(aes(y = term, color = year)) +
            geom_pointrange(aes(x = estimate, xmin = estimate -1.96*std.error, xmax = estimate +1.96*std.error),
                            position = position_dodge(width = 0.4), size = 0.2) +
            geom_vline(aes(xintercept = 0), color = "#f26a4c", linetype = "dashed") +
            theme_classic() +
            facet_wrap(~period) +
            labs(x = "Estimate", y = "", 
                color = "Year: ") +
            theme(
            legend.position = "bottom"
            )

save_output(fig_c4_budget_model, w = 5.5, h = 4)

##############################################################################################
#| FIGURE 4.10: "Proportion of support for redistribution by social class and personal impact of the pandemic, Ireland, 2021"

fig_c4_covid <-
    df_ess |>
        mutate(class_covid = if_else(
            as_factor(class5) %in% c("Higher-grade service class", "Lower-grade service class"),
            "Service class (higher- and lower-grade)",
            as_factor(class5)
        )) |> 
        filter(!is.na(class5) & cntry == "IE" & essround == 10) |>
        mutate(
            impact = case_when(
                hapljc19 == 1 | hapfoc19 == 1 | hapfuc19 == 1 ~ "Lost job/\nUnpaid leave/\nFurloughed",
                hapirc19 == 1 | hapwrc19 == 1 ~ "Income\nor hours\nreduced",
                hapnoc19 == 1 ~ "Nothing",
                is.na(hapnoc19) ~ NA_character_,
                .default = "Other/DK\nRefusal"
            )
        ) |>
        group_by(impact, class = as_factor(class_covid)) |>
        summarise(
            mean = weighted.mean(gincdif_agree, w = anweight, na.rm = T)*100,
            total = n(),
            se = sd(gincdif_agree, na.rm = T)/sqrt(length((gincdif_agree)))) |>
        filter(impact != "Other/DK\nRefusal") |> 
        ggplot(aes(x = impact, y = mean, label = paste0(round(mean), " %"))) +
        geom_bar(stat = "identity", width = 0.6, fill = "#1F355E") +
        facet_wrap(~class, ncol = 2) +
        geom_text(size = 3, color = "white", vjust = 1.2) +
        geom_text(aes(label = paste0("(n=", total, ")")),size = 2, color = "white", vjust = 3.5) +
        # ggplot(aes(x = impact, y = class))+
        # geom_tile(colour = "grey70", fill = "white") +
        # geom_point(aes(color = mean, size = total), shape = 15) +
        # geom_text(aes(label = round(mean)), color = "grey20", size = 3, hjust = 2) +
        # scale_color_distiller(palette = "RdBu", direction=-1) +
        # scale_size(range = c(2, 10), guide = "none") +
        labs(x = "", y = "", color = "Support for\nredistribution (%)") +
        theme_classic() +
        theme(
            legend.position = "top",
            legend.title = element_text(size = 10),
            axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
            axis.text.y = element_text(size = 10),
        )

save_output(fig_c4_covid, w = 6, h = 5)
