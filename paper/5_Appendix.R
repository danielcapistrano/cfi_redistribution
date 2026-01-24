source("paper/1_SetupImport.R")

# Complete output table

# Table A1: "Logistic regression coefficients (odds-ratio) for 'support for redistribution' (all variables), Ireland, 2003-2023"

tbl_extra_full <- 
    get_table(
        model = get_model(
            df = df_ess |> filter(cntry == "IE"),
            dv = 'gincdif_agree',
            ivs = vars_indo,
            type = 'glm'
        ),
        type = 'glm'
    )

save_output(tbl_extra_full)

# Original outcome variable (5-point)
#| Table A2: "OLS regression coefficients for 'support for redistribution' (original variable), Ireland, 2003-2023"

tbl_extra_ols_ie <- 
    get_table(
        model = get_model(
            df = df_ess |> filter(cntry == "IE"),
            dv = 'gincdif_inv',
            ivs = vars_indo,
            type = 'lm'
        ),
        type = 'lm'
    )

save_output(tbl_extra_ols_ie)

# Other countries

#| Table A3: "OLS regression coefficients for 'support for redistribution' (original variable), ESS countries except Ireland, 2003-2023"

tbl_extra_ols_eu <- 
    get_table(
        model = get_model(
            df = df_ess |> filter(cntry != "IE"),
            dv = "gincdif_inv",
            ivs = c(vars_indo, "cntry"),
            type = "lm"
        ),
        type = "lm"
    )

save_output(tbl_extra_ols_eu)

# Household income as predictor

#| Table A4: "Logistic regression coefficients (odds-ratio) for 'support for redistribution' (household income), Ireland, 2003-2023"

tbl_extra_hinc <- 
    get_table(
        model = get_model(
            df = df_ess |> filter(cntry == "IE"),
            dv = 'gincdif_agree',
            ivs = c(vars_indo[vars_indo != "as_factor(hincfel)"], "hinctnta"),
            type = 'glm'
        ),
        type = 'glm'
    )

save_output(tbl_extra_hinc)

# Intepersonal trust as predictor

tbl_extra_trust <- 
    get_table(
        model = get_model(
            df = df_ess |> filter(cntry == "IE"),
            dv = 'gincdif_agree',
            ivs = c(vars_indo, "ppltrst"),
            type = 'glm'
        ),
        type = 'glm'
    )

save_output(tbl_extra_trust)

# Placebo for dsp campaign

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

# Calculating mid-point of the fieldwork
dsp_control_midpoint <- 
    df_ess |> 
    filter(cntry == 'IE' & essround == 8 & ess_day < '2017-04-17') |> 
    summarise(m = median(ess_day)) |> 
    pull(m)

df_ess <- df_ess |> mutate(
    dsp_midpoint_period = case_when(
        essround != 8 ~ NA_character_,
        ess_day <= dsp_control_midpoint ~ "Before",
        ess_day > dsp_control_midpoint ~ "After"),
    dsp_midpoint_dafter = as.numeric(ess_day - dsp_control_midpoint)
    )


tb_dsp_placebo_model <- 
    map_dfr(c("gincdif_inv",  "sbprvpv_inv"), function(outcome) {
            map_dfr(c(150, 14, 7), function(n_days) {
                get_event_coeff(
                    df = df_ess,
                    event_date = dsp_control_midpoint,
                    round = 8,
                    outcome = outcome,
                    event_window = n_days,
                    covars = c("gndr", "age_group", "mnactic")
                )
            })
        })


fig_extra_dsp <-
    tb_dsp_placebo_model |>
        filter(term %in% c("treated_group", "treated_group:treated_days")) |>
        mutate(
            variable = case_when(
                str_detect(variable, "sbprvpv") ~ "Social benefits prevent poverty",
                str_detect(variable, "gincdif") ~ "Government should reduce income differences"
            ),
            term = case_match(term, "treated_group" ~ "Treatment group", "treated_group:treated_days" ~ "Treated*Days"),
            period = case_match(period, "7d" ~ "\u00B1 1 week\n (n=151)", "14d" ~ "\u00B1 2 weeks\n (n=312)", "150d" ~ "Full sample\n (n=2,379)")
        ) |>
            ggplot(aes(y = term, color = period)) +
            geom_pointrange(aes(x = estimate, xmin = estimate -1.96*std.error, xmax = estimate +1.96*std.error),
                            position = position_dodge(width = 0.4)) +
            geom_vline(aes(xintercept = 0), color = "#f26a4c", linetype = "dashed") +
            theme_classic() +
            facet_wrap(~variable, ncol = 1) +
            labs(x = "Estimate", y = "", 
                color = "Time from placebo: ", fill = "Time from placebo: ") +
            theme(
            legend.position = "bottom"
            )
            
            
save_output(fig_extra_dsp, w = 6, h=6)


## Pandemic impact in Ireland, Great Britain and Germany

#| Table A5: "Proportion of support for redistribution by social class and personal impact of the pandemic, Ireland, Great Britain and Germany, 2021"

fig_extra_covid <- 
    df_ess |> 
        filter(!is.na(class5) & cntry %in% c('IE', 'GB', 'DE') & essround == 10) |> 
        mutate(
            impact = case_when(
                hapljc19 == 1 | hapfoc19 == 1 | hapfuc19 == 1 ~ "Lost job\nUnpaid leave\nForloughed",
                hapirc19 == 1 | hapwrc19 == 1 ~ "Income or hours\nreduced",
                hapnoc19 == 1 ~ "Nothing",
                is.na(hapnoc19) ~ NA_character_,
                .default = "Other"
            )
        ) |> 
        group_by(cntry, impact, class = as_factor(class5)) |> 
        summarise(
            mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100,
            total = n(),
            se = sd(gincdif_agree, na.rm = T)/sqrt(length((gincdif_agree)))) |> 
        ggplot(aes(x = impact, y = class, fill = mean))+
        geom_tile() +
        geom_text(aes(label = paste0(round(mean), "%\n(", total, ")")), size = 3) +
        scale_fill_distiller(palette = "RdBu", direction=-1) + 
        #coord_fixed(ratio=0.5) +
        facet_wrap(~cntry, ncol = 1) +
        labs(caption = "Source: European Social Survey, 2021",
            x = "Pandemic impact", y = 'Social Class', fill = 'Percentage') +
        theme(
            legend.position = 'top',
            axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
            axis.text.y = element_text(size = 12),
            plot.caption = element_text(hjust = -0.5)
        ) 


save_output(fig_extra_covid, w = 10, h=10)
