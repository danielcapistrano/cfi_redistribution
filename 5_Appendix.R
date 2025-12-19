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


