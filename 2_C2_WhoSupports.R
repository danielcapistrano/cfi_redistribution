source("1_SetupImport.R")

##############################################################################################
# FIGURE 2.1 "Proportion who agree that the 'Government should reduce income differences', Europe, 2023"

eumap <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf") |> filter(!name %in% c("Russia", "Faeroe Is."))

fig_eumap <- 
    eumap |> 
        mutate(cntry = case_when(
            name == 'France' ~ "FR",
            name == "Norway" ~ "NO",
            .default = iso_a2)) |> 
        left_join(
            df_ess |>
            filter(essround == 11) |> 
            group_by(cntry) |> 
            summarise(
                mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T),
                se = sd(gincdif_agree, na.rm = T)/sqrt(length((gincdif_agree)))),
            by = "cntry"
        ) |> 
        ggplot() +
        geom_sf(aes(fill = mean*100)) +
        geom_sf_label(aes(label = round(mean*100)), color = 'grey20', alpha = 0, label.size = NA, size = 3.5) +
        scale_fill_distiller(palette = 'RdBu', na.value = "grey85") +
        coord_sf(xlim = c(-25, 40), ylim = c(25, 75), expand = FALSE) +
        theme_void() +
        labs(
            fill = "Proportion",
        ) +
        theme(
            legend.position = "left", 
            legend.title.position = "top",
            legend.direction = "horizontal",
        )  


save_display(fig_eumap, w = 6, h = 5)

##############################################################################################
#FIGURE 2.3 "Support for redistribution in Ireland by sex, 2002-2023"

fig_sex <- 
    df_ess |> 
        filter(cntry == "IE") |> 
        group_by(start_year, Sex = as_factor(gndr), .drop=TRUE) |>
        summarise(
                mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100,
                se = (sd(gincdif_agree, na.rm = T)/sqrt(length((gincdif_agree)))*100)
            ) |> 
            ggplot(aes(x = start_year, y = mean,  ymin = mean - 1.96*se, ymax= mean + 1.96*se, fill = Sex, colour = Sex)) +
            geom_pointrange() +
            geom_line() +
            scale_color_manual(values = c("#f26a4c", "#1F355E"))+
            scale_x_continuous(breaks = seq(2002, 2024, 2)) +
            theme_classic() +
            labs(
                #caption = "Source: European Social Survey",
                y = "Government should reduce income differences (%)",
                x = "Year"
            ) +
            theme(
                legend.position = "bottom", 
                legend.title = element_text(size = 12),
                legend.text = element_text(size = 12),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
            )

save_display(fig_sex, w=6, h=5)

##############################################################################################
#FIGURE 2.4: "Scatter plot of support for redistribution by age and country, 2023"

fig_age_eu <- 
    df_ess |> 
        filter(essround == 11 & cntry %in% c('IE', 'FI', 'IS', 'GB')) |> 
        group_by(Country = cntry_label, agea) |> 
        summarise(mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100) |> 
        ggplot(aes(x = agea, y = mean)) +
        geom_point(size = 1) +
        geom_smooth(method = 'lm', color = "#f26a4c") +
        #ylim(3,5) +
        facet_wrap(~Country) + 
        labs(
            x = "Age", 
            y = "Government should reduce income differences (%)", 
            #caption = "Source: European Social Survey, Round 11"
        )+
        theme_classic() +
        theme(plot.caption = element_text(hjust = 0))

save_display(fig_age_eu, w=6, h=5)

##############################################################################################
#FIGURE 2.5: : "Mean value of support for redistribution by social class, income quintile, and financial situation, Ireland, 2002-2023"


get_ses_mean <- function(mydf, myvar, mygroup, mylabel){
    mydf |> 
        filter(!is.na(get(mygroup))) |> 
        group_by(group = as_factor(get(mygroup))) |> 
        summarise(
            mean = weighted.mean(get(myvar), w = pspwght, na.rm = T)*100,
            se = (sd(get(myvar), na.rm = T)/sqrt(length((get(myvar))))*100)
        ) |> 
        mutate(variable = mylabel)
}

tb_ses_mean <- 
    map2_dfr(
        .x = c("class5", "hinc_quint", "hincfel"),
        .y = c("Social Class", "Income Quintile", "Financial Situation"),
        ~get_ses_mean(df_ess |> filter(cntry == "IE"), "gincdif_agree", .x, .y)
    ) 

fig_class <- 
    tb_ses_mean |> 
        mutate(group = str_remove(group, "on present income")) |> 
        ggplot(aes(x = reorder(group, -mean), y = mean,  ymin = mean - 1.96*se, ymax= mean + 1.96*se)) +
        geom_pointrange(color = 'grey20') +
        geom_point(color = '#1F355E') +
        coord_flip() +
        theme_classic() +
        theme(
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            strip.text.x = element_text(size = 12),
        ) +
        facet_wrap(~variable, nrow = 3, scales = 'free_y') +
        labs(x = '', y = 'Government should reduce income differences (%)')

save_display(fig_class, w = 6, h= 6)

##############################################################################################
#TABLE 2.1 tbl-cap: "Logistic regression estimates (odds-ratio) for 'support for redistribution', Ireland, 2002-2023"

tbl_multiv_agree <- 
    get_table(
        model = get_model(
            df = df_ess |> filter(cntry == "IE"),
            dv = 'gincdif_agree',
            ivs = vars_indo,
            type = 'glm'
        ),
        toprint = vars_indo[!vars_indo %in% vars_omit],
        type = 'glm'
    )


tbl_multiv_strong <- get_table(
        model = get_model(
            df = df_ess |> filter(cntry == "IE"),
            dv = 'gincdif_strong_agree',
            ivs = vars_indo,
            type = 'glm'
        ),
        toprint = vars_indo[!vars_indo %in% vars_omit],
        type = 'glm'
    )

tbl_merge(
    list(tbl_multiv_agree, tbl_multiv_strong), 
    tab_spanner = c("Agree or Strongly agree", "Strongly agree")
)  |> as_gt() |> gtsave('./img/Table2_1.rtf')
