source("paper/1_SetupImport.R")

##############################################################################################
# FIGURE 3.1: "Mean values for deservingness of government support by group and country, 2016"

fig_c3_deservingness <- 
    df_ess |>
        filter(essround == 8 & cntry %in% c(
            "CH", "NL", "GB", "IE", "FR", "DE", "PL", "FI", "ES", "PT", "IS" 
        )) |> 
        group_by(country = cntry_label)   |> 
        summarise(across(c("gvcldcr", "gvslvol", "gvslvue"), list(
            mean = ~ weighted.mean(.x, w = pspwght, na.rm = T),
            se = ~ sd(.x, na.rm = T)/sqrt(length((.x))))
        ))  |> 
        pivot_longer(cols = -country, names_to = c("Group", ".value"), names_sep = "_") |> 
        mutate(Group = ordered(Group, levels = c("gvslvue", "gvcldcr", "gvslvol"))) |> 
        mutate(Group = case_match(Group, "gvcldcr" ~ "Working parents", "gvslvol" ~ "Old", "gvslvue" ~ "Unemployed")) |> 
        ggplot(aes(y = reorder(country, -mean), x = mean, xmin = mean - 1.96*se, xmax = mean + 1.96*se,  group = Group, color = Group)) +
        geom_pointrange(size = 0.4) +
        scale_color_manual(values = c("#f26a4c", "#1fbec6", "#396065"), breaks = c("Unemployed", "Working parents", "Old"))+
        theme_classic() +
        labs(
            y = "Country", 
            x = "Mean"
        ) +
        theme(
            legend.position="bottom",
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.text.y = element_text(size = 12)
        )

save_output(fig_c3_deservingness, h=5, w=5.5)

##############################################################################################
#FIGURE 3.2: "Mean values for deservingness of government support by group, Ireland, 2016"

fig_c3_ie_deserve <-
    purrr::map_dfr(
        c("gndr", "age_group", "hincfel_fct"),
        ~ df_ess |>
            filter(essround == 8 & cntry == "IE" & !is.na(hincfel)) |> 
            mutate(hincfel_fct = factor(str_remove(as_factor(hincfel), " on present income"), levels = c("Very difficult", "Difficult", "Coping", "Living comfortably"))) |> 
            group_by(Demographic = as_factor(get(.x)))   |> 
            summarise(across(c("gvcldcr", "gvslvol", "gvslvue"), list(
                mean = ~ weighted.mean(.x, w = pspwght, na.rm = T),
                se = ~ sd(.x, na.rm = T)/sqrt(length((.x))))
            )) |> 
            mutate(variable = .x)
    ) |> 
    pivot_longer(cols = -c(Demographic, variable), names_to = c("Group", ".value"), names_sep = "_") |> 
    mutate(Group = ordered(Group, levels = c("gvslvue", "gvcldcr", "gvslvol"))) |> 
    mutate(Group = case_match(Group, "gvcldcr" ~ "Working parents", "gvslvol" ~ "Old", "gvslvue" ~ "Unemployed")) |> 
    mutate(variable = case_match(variable, "age_group" ~ "Age group", "hincfel_fct" ~ "Financial situation", "gndr" ~ "Gender")) |> 
    ggplot(aes(y = Demographic, x = mean, xmin = mean - 1.96*se, xmax = mean + 1.96*se,  group = Group, color = Group)) +
    geom_pointrange(size = 0.2) +
    scale_color_manual(values = c("#f26a4c", "#1fbec6", "#396065"), breaks = c("Unemployed", "Working parents", "Old"))+
    facet_wrap(~variable, ncol = 1, scale = "free_y") +
    theme_classic() +
    labs(
        y = "", 
        x = "Mean"
    ) +
    theme(
        legend.position="bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.y = element_text(size = 12)
    )

save_output(fig_c3_ie_deserve, h=6, w=6)

##############################################################################################
#FIGURE 3.3: "Distribution for 'When should immigrants obtain rights to social benefits/services', Ireland, 2016"

fig_c3_immig <- 
    df_ess |> 
        mutate(Country = if_else(cntry == 'IE', 'Ireland', 'Other countries')) |> 
        filter(essround == 8 & !is.na(imsclbn)) |> 
        group_by(Country) |> 
        count(Moment = as_factor(imsclbn), wt =anweight) |> 
        mutate(p = round(prop.table(n)*100)) |>  
        ggplot(aes(x = Moment, y = p, fill = Country)) +
        geom_bar(stat = 'identity', width = 0.5, position = 'dodge') +
        geom_text(aes(label = round(p)), position = position_dodge(width = 0.5), hjust = -0.5, size = 3) +
        scale_fill_manual(values = c( "#1F355E" , "#f26a4c"))+
        scale_x_discrete(labels = c(
            "Immediately on\narrival", 
            "After a year,\nwhether or not have\nworked", 
            "After worked and\npaid taxes at least\na year", 
            "Once they have\nbecome a citizen", 
            "They should never\nget the same rights"
        ))+
        coord_flip() +
        labs(x = "", y = "Proportion") +
        theme_classic() +
        theme(
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            axis.text.y = element_text(size = 11)
        )  

save_output(fig_c3_immig, w = 6, h = 5)

##############################################################################################
#FIGURE 3.4: "Proportion of respondents who agree with statements about social benefits, Ireland, 2009/2016"

fig_c3_benefits <- 
    df_ess |>
        filter(essround %in% c(4, 8) & cntry == "IE") |> 
        group_by(year = start_year)  |> 
        summarise(across(all_of(believe_vars_agree), ~ weighted.mean(.x, w = pspwght, na.rm = T)*100)) |> 
        pivot_longer(cols = -year, names_to = "Question", values_to = "mean") |> 
        mutate(Statement = case_when( 
            str_detect(Question, "sbprvpv") ~ "Prevent poverty",
            str_detect(Question, "sblazy") ~ "Make people lazy", 
            str_detect(Question, "sbeqsoc") ~ "Lead to equal society", 
            str_detect(Question, "sbstrec") ~ "Put strain on economy",
            str_detect(Question, "sbbsntx") ~ "Cost businesses\ntoo much in taxes",
            str_detect(Question, "sblwcoa") ~ "Make people less\n willing to care",
        )) |> 
        ggplot(aes(x = year, y = mean, group = Statement, color = Statement)) +
        geom_point(size = 2) +
        geom_text(aes(label = round(mean)), vjust = -1, size = 2.5)+
        geom_line(linewidth = 1.2) +
        scale_color_brewer(palette = 'Dark2', guide='none') +
        geom_dl(aes(label = Statement), method = list('last.points', cex = 0.9, hjust = -0.1)) +
        scale_x_continuous(breaks = c(2009,2016), limits = c(2008, 2020) ) +
        labs(
            x = "Year", 
            y = 'Proportion who agree that social benefits...'
        ) +
        theme_classic()+
        theme(axis.text.x = element_text(size = 11))

save_output(fig_c3_benefits, w = 6, h = 5)

##############################################################################################
#FIGURE 3.5: "Proportion of agreement that social benefits make people lazy, Europe, 2009/2016"

tb_lazy <- 
    df_ess |>
    filter(essround %in% c(4,8)) |> 
    group_by(start_year, cntry) |> 
    summarise(
        mean = weighted.mean(sblazy_agree, w = pspwght, na.rm = T))  |> 
    arrange(desc(mean))

fig_c3_lazy <- 
    eumap |> 
        mutate(cntry = case_when(
            name == 'France' ~ "FR",
            name == "Norway" ~ "NO",
            .default = iso_a2)) |>
        left_join(tb_lazy, by = "cntry") |>  filter(!is.na(start_year)) |> 
        ggplot() +
        geom_sf(aes(fill = round(mean*100))) +
        geom_sf_label(aes(label = round(mean*100)), alpha = 0, label.size = NA,size = 2, color = 'grey20') +
        scale_fill_distiller(palette = "RdBu", na.value = "grey85") +
        coord_sf(xlim = c(-25, 40), ylim = c(25, 75), expand = FALSE) +
        theme_void() +
        labs(fill = "Proportion") +
        facet_wrap(~start_year) +
        theme(
            legend.position = "top", 
            legend.title = element_text(size = 8),
            legend.title.position = "top",
            legend.text = element_text(size = 7),
            legend.direction = "horizontal"
        )

save_output(fig_c3_lazy, w =5, h=4)

##############################################################################################
#Table 3.1: "Logistic regression estimates for agreement that 'social benefits make people lazy', 2016"

tbl_c3_multiv_beliefs <- 
    tbl_merge(
        tbls = map(
            c("IE", "GB", "PT", "PL"),
            ~tbl_regression(
                get_model(
                    df = df_ess |> filter(cntry == .x & essround == 8),
                    dv = "sblazy_agree",
                    ivs = vars_indo[!vars_indo %in% c("as_factor(essround)")],
                    type = 'glm'
                ),
                include = vars_indo[!vars_indo %in% c(vars_omit, "as_factor(brncntr)")],
                exponentiate = TRUE,
                conf.int = FALSE,
                label = list_labels
                ) |> 
                bold_p(t = 0.05, q = FALSE)  |> 
                remove_abbreviation("OR = Odds Ratio")
            ),
        tab_spanner = c("Ireland", "United Kingdom", "Portugal", "Poland")
        )

save_output(tbl_c3_multiv_beliefs)


# Figure 3.5 Distribution of unfairness 

df_ess <- df_ess |> mutate(btminfr_rev = btminfr * -1)

get_dist_fair <- function(var, label){
    df_ess |>
         filter(cntry == 'IE') |> 
        filter(essround == 9) |> 
        count(values = get(var), wt = pspwght) |> 
        mutate(proportion = prop.table(n)*100, var = label) 
}

# df_ess |>
#         # filter(cntry == 'IE') |> 
#         filter(essround == 9) |> 
#         mutate(bt_unfair = if_else(btminfr < 0, 1, 0)) |> 
#         group_by(cntry) |> 
#         summarise(values = weighted.mean(btminfr, w = pspwght, na.rm = T)) |> 
#         arrange(values)
#         # mutate(proportion = prop.table(n)*100)  |> 
#         # arrange(proportion) |> filter(values == 0)

# get_dist_fair("topinfr", "Top decile")

fig_c3_fair_dist <- 
    get_dist_fair( "btminfr", "Bottom decile") |> 
        bind_rows(get_dist_fair("topinfr", "Top decile")) |> 
        ggplot(aes(x = vals, y = p)) +
        geom_bar(stat = 'identity', width = 0.8) +
        facet_wrap(~var, nrow = 2) +
        labs(x = "<----- Unfairly low       Fair      Unfairly high ----->", y = "Percentage of respondents") +
        theme_classic()

save_output(fig_c3_fair_dist, w = 5, h = 5)


##############################################################################################
# Figure 3.6: "Effect (odds-ratio) of perception of 'unfairness of income' on 'support for redistribution', 2018"

get_fig_c3_fair <- function(outcome){
     map_dfr(
        c("NO", "FI", "FR", "PL", "IE",  "PT", "GB", "ES"),
        ~broom::tidy(
            get_model(
                df = df_ess |> filter(cntry == .x & essround == 9) |> mutate(
                    top_toohigh = topinfr, # if_else(topinfr %in% c(3, 4), 1, 0),
                    bottom_toolow = btminfr * -1, # if_else(btminfr %in% c(-3, -4), 1, 0)
                ),
                dv = outcome,
                ivs = c("top_toohigh", "bottom_toolow", vars_indo[vars_indo != "as_factor(essround)"]),
                type = 'glm'
            ), 
            exponentiate = T) |> 
            mutate(cntry = .x)
        ) |> 
        filter(term %in% c("top_toohigh", "bottom_toolow")) |> 
        select(cntry, term, estimate, `std.error`) |> 
        mutate(Group = case_match(term, "top_toohigh" ~ "Top decile\n'unfairly high'", "bottom_toolow" ~ "Bottom decile\n'unfairly low'")) |> 
        mutate(cntry = countrycode(cntry, "iso2c", "country.name")) |> 
        ggplot(aes(x = reorder(cntry, -estimate), y = estimate, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error,  group = Group, color = Group)) +
        geom_pointrange(position = position_dodge(width = 0.4)) +
        geom_hline(yintercept = 1, color = 'red', linetype = 'dashed') +
        scale_color_manual(values = c("#f26a4c", "#1fbec6", "#396065"))+
        labs(
            x = "", y = "Estimate (OR)", color = ""
        ) +
        theme_classic() +
        theme(
            legend.position = "bottom",
            legend.text = element_text(size = 11),
            axis.text.x = element_text(size = 11, angle = 45, vjust = 1, hjust = 1),
            plot.caption = element_text(hjust = 0)
        ) 
}

fig_c3_fairness <- get_fig_c3_fair("gincdif_agree")

save_output(fig_c3_fairness, w =6, h=5)

# map_dfr(
#         c("NO", "FI", "FR", "PL", "IE",  "PT", "GB", "ES"),
#         ~broom::tidy(
#             get_model(
#                 df = df_ess |> filter(cntry == .x & essround == 9) |> mutate(
#                     top_toohigh = topinfr, # if_else(topinfr %in% c(3, 4), 1, 0),
#                     bottom_toolow = btminfr * -1, # if_else(btminfr %in% c(-3, -4), 1, 0)
#                 ),
#                 dv = "gincdif_agree",
#                 ivs = c("top_toohigh", "bottom_toolow", vars_indo[vars_indo != "as_factor(essround)"]),
#                 type = 'glm'
#             ), 
#             exponentiate = T) |> 
#             mutate(cntry = .x)
#         ) |> 
#         filter(term %in% c("top_toohigh", "bottom_toolow"))
                
##############################################################################################
#FIGURE 3.7: "Relationship between support for redistribution and agreement that there could be 'higher taxes if it means more or better  public services', Europe, 2023/2025"

eubar_tax <- read_csv("./data/raw/eurob_tax.csv")
eubar_wealthy <- read_csv("./data/raw/eurob_wealthy.csv")
eubar_tax <- eubar_tax |> select(-wtotal, -tax_dk)  |> pivot_longer(cols = -cntry, names_to = "option", values_to = "n")

eubar_taxben <- eubar_tax |> group_by(cntry) |> mutate(tax = prop.table(n)*100) |> filter(cntry != "EU27" & option == "tax_morebenef")

fig_c3_taxes <- 
    df_ess |> 
        filter(essround == 11) |>  
        group_by(cntry) |> 
        summarise(
            mean = weighted.mean(gincdif_agree, w = pspwght, na.rm = T)*100, .groups = 'drop') |> 
        left_join(eubar_taxben |> select(cntry, tax)) |> 
        mutate(hl = if_else(cntry == "IE", "Ireland", "Other")) |> 
        ggplot(aes(x = mean, y = tax, label = as_factor(cntry)))+
        geom_point(aes(color = hl), size = 2.5) +
        geom_text(nudge_x = 0.5, nudge_y = 0.5, size = 3.5,  check_overlap = T, color = "grey30") +
        scale_color_manual(values = c("#f26a4c", "#1F355E"))+
        theme_classic() +
            labs(
                x = "Government should reduce income differences (%)",
                y = "Agree with higher taxes if it means more or better public services (%)"
            ) +
            theme(legend.position = "none")  

save_output(fig_c3_taxes, w =6, h=5)
