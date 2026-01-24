source("1_SetupImport.R")

# Complete output table

# Table A1: "Logistic regression coefficients (odds-ratio) for 'support for redistribution' (all variables), Ireland, 2003-2023"

get_table(
    model = get_model(
        df = df_ess |> filter(cntry == "IE"),
        dv = 'gincdif_agree',
        ivs = vars_indo,
        type = 'glm'
    ),
    type = 'glm'
)



# Original outcome variable (5-point)
#| Table A2: "OLS regression coefficients for 'support for redistribution' (original variable), Ireland, 2003-2023"

get_table(
    model = get_model(
        df = df_ess |> filter(cntry == "IE"),
        dv = 'gincdif_inv',
        ivs = vars_indo,
        type = 'lm'
    ),
    type = 'lm'
)

get_table(
    model = get_model(
        df = df_ess |> filter(cntry == "IE"),
        dv = 'gincdif_agree',
        ivs = c(vars_indo, "ppltrst"),
        type = 'glm'
    ),
    type = 'glm'
)


# Other countries

#| Table A3: "OLS regression coefficients for 'support for redistribution' (original variable), Ireland, 2003-2023"


get_table(
    model = get_model(
        df = df_ess |> filter(cntry != "IE"),
        dv = 'gincdif_inv',
        ivs = c(vars_indo, "cntry"),
        type = 'lm'
    ),
    type = 'lm'
)

# Household income as predictor

#| Table A4: "Logistic regression coefficients (odds-ratio) for 'support for redistribution' (household income), Ireland, 2003-2023"

get_table(
    model = get_model(
        df = df_ess |> filter(cntry == "IE"),
        dv = 'gincdif_agree',
        ivs = c(vars_indo[vars_indo != "as_factor(hincfel)"], "hinctnta"),
        type = 'glm'
    ),
    type = 'glm'
)


# Intepersonal trus as predictor

get_table(
    model = get_model(
        df = df_ess |> filter(cntry == "IE"),
        dv = 'gincdif_agree',
        ivs = c(vars_indo, "ppltrst"),
        type = 'glm'
    ),
    type = 'glm'
)

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


get_coef_period <- function(outcomes, indep_vars, timevar, period){
  map_dfr(
    outcomes,
    function(x){
      map_dfr(
        period,
        function (y) {
          broom::tidy(get_model(
            df = df_ess |> filter(
              cntry == 'IE' & essround == 8 & ess_day < '2017-04-17' & news == 'Yes' & abs(get(timevar)) <= y
            ),
            dv = x,
            ivs = indep_vars,
            #w = NULL,
            type = 'lm')) |> 
            mutate(dv = x, period = y)
        })
    })
}


dsp_placebo <- get_coef_period(
    outcomes = c('gincdif_inv', 'sblazy', 'sbprvpv'),
    indep_vars = c('dsp_midpoint_period'),# 'as_factor(gndr)', 'as_factor(class5)', 'as_factor(mnactic)'),
    timevar = 'dsp_midpoints_dafter',
    period = c(15,10, 5))


#fig_dsp_placebo <- 
    dsp_placebo |>
        mutate(
            variable = case_match(
                dv, 
                'gincdif_inv' ~ 'Support for redistribution', 
                'sblazy' ~ "Social benefits make people lazy",
                'sbprvpv' ~ "Social benefits prevent poverty"),
            period = case_match(
                period,
                #200 ~ 'Full sample',
                15 ~ '30 days',
                10 ~ '20 days',
                5 ~ '10')) |> 
        filter(term %in% c('dsp_midpoint_periodBefore')) |> 
        ggplot(aes(y = term, fill = period, color = period)) +
        geom_pointrange(aes(x = estimate, xmin = estimate -1.96*std.error, xmax = estimate +1.96*std.error),
                position = position_dodge(width = 0.4)) +
        geom_vline(aes(xintercept = 0), color = '#f26a4c', linetype = 'dashed') +
        facet_wrap(~variable, ncol = 1) +
        theme_classic() +
        labs(x = "Estimated effect of being interviewed\nin the second part of the fieldwork", y = "", color = "Time-window: ", fill = "Time-window: ") +
        theme(
            axis.text.y = element_blank(),
            legend.position = 'bottom'
        ) 

save_display(fig_dsp_placebo, w = 6, h=6)


## Pandemic impact in Ireland, Great Britain and Germany

#| Table A5: "Proportion of support for redistribution by social class and personal impact of the pandemic, Ireland, Great Britain and Germany, 2021"

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

