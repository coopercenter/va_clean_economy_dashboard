#----------------------------------------PLOTTING GEOSPATIAL DATA----------------------------------------------------------
#energy equity figures

#OPTION A--------------------------------------------------------------------------------------------------------------
#energy burden map showing average energy expenditures by county
#va_avg_annual_energy_cost <- ggplot() +
#  geom_sf(data = virginia_outline, fill = NA,color="dimgrey") +
#  geom_sf(data = va_energy_equity_by_county, aes(fill = avg_annual_energy_cost)) +
#  geom_sf(data = va_energy_equity_by_county, aes(fill = number1,text=paste0(county,"\nEnergy Expenditures: $",avg_annual_energy_cost)),alpha=0) +
#  scale_fill_gradientn(name="Average Annual Energy Cost \nin Dollars\n",colors=ceps_pal[1:5]) +
#  coord_sf(xlim = c(-84, -75), ylim = c(36, 40), expand = FALSE)+
#  labs(title = "Average Annual Energy Cost ($) for Counties in Virginia", caption = "Source: U.S. Department of Energy") +
#  theme(panel.background = element_rect(fill = "#F0F0F0"),
#        panel.grid = element_line(color="#F0F0F0"),
#        axis.text = element_blank(),
#        axis.ticks = element_blank(),
#        plot.background=element_rect(fill="#F0F0F0"),
#        plot.caption=element_text(hjust = 0.5,face="italic"),
#        plot.subtitle = element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
#        plot.title =element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
#        legend.title=element_text(size=7),
#        legend.text=element_text(size=7),
#        legend.justification = "center",
#        legend.background = element_rect(fill = "#F0F0F0"),
#        text = element_text(family = "Helvetica",color = "dimgrey"))
#va_avg_annual_energy_cost

#energy burden map showing average energy expenditures as percent of income by county
#va_avg_annual_energy_percent_exp <-  ggplot() +
#  geom_sf(data = virginia_outline, fill = NA,color="dimgrey") +
#  geom_sf(data = va_energy_equity_by_county, aes(fill = avg_energy_burden_as_percent_income)) +
#  geom_sf(data = va_energy_equity_by_county, aes(fill = number2,text=paste0(county,"\nEnergy Expenditures: ",avg_energy_burden_as_percent_income,"%")),alpha=0) +
#  scale_fill_gradientn(name="Average Annual Energy Cost \nas Percentage of Income\n",colors=ceps_pal[1:5]) +
#  coord_sf(xlim = c(-84, -75), ylim = c(36, 40), expand = FALSE) +
#  labs(title = "Average Energy Burden (% income) for Counties in Virginia", caption = "Source: U.S. Department of Energy") +
#  theme(panel.background = element_rect(fill = "#F0F0F0"),
#        panel.grid = element_line(color="#F0F0F0"),
#        axis.text = element_blank(),
#        axis.ticks = element_blank(),
#        plot.background=element_rect(fill="#F0F0F0"),
#        plot.caption=element_text(hjust = 0.5,face="italic"),
#        plot.subtitle = element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
#        plot.title =element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
#        legend.title=element_text(size=7),
#        legend.text=element_text(size=7),
#        legend.justification = "center",
#        legend.background = element_rect(fill = "#F0F0F0"),
#        text = element_text(family = "Helvetica",color = "dimgrey"))
#va_avg_annual_energy_percent_exp

#va_avg_annual_energy_cost_p <- ggplotly(va_avg_annual_energy_cost,tooltip =c("text")) %>%
#  layout(title = list(text=paste0("Average Annual Energy Cost ($) for Counties in Virginia"),titlefont=list(size=15)),
#         xaxis=list(title = paste0("<i>","<sub>","Source: U.S. Department of Energy","<sub>","<i>"),titlefont=list(size=14)))%>%
#  config(displaylogo = FALSE,
#         modeBarButtonsToRemove = c("pan2d","select2d","lasso2d","zoom2d","autoScale2d","resetScale2d","toggleSpikelines"))%>%
#  style(hoveron = "fills")
#va_avg_annual_energy_cost_p

#va_avg_annual_energy_percent_exp_p <- ggplotly(va_avg_annual_energy_percent_exp,tooltip = c("text")) %>%
#  layout(title = list(text=paste0("Average Energy Burden (% income) for Counties in Virginia"),titlefont=list(size=15)),
#         xaxis=list(title = paste0("<i>","<sub>","Source: U.S. Department of Energy","<sub>","<i>"),titlefont=list(size=14)))%>%
#  config(displaylogo = FALSE,
#         modeBarButtonsToRemove = c("pan2d","select2d","lasso2d","zoom2d","autoScale2d","resetScale2d","toggleSpikelines"))%>%
#  style(hoveron = "fills")
#va_avg_annual_energy_percent_exp_p
#-------------------------------------------------------------------------------------
#OR
#OPTION B---------------------------------------------------------------------------
#energy burden map showing average energy expenditures by county
# va_avg_annual_energy_cost <- ggplot() +
#   geom_sf(data = virginia_outline, fill = NA, color = "dimgrey") +
#   geom_sf(data = va_energy_equity_by_county, aes(
#     fill = avg_annual_energy_cost,
#     text = paste0(county, "\nEnergy Expenditures: $", avg_annual_energy_cost)
#   )) +
#   scale_fill_gradientn(name = "Average Annual Energy Cost \nin Dollars\n", colors =
#                          ceps_pal[1:5]) + #setting alpha adds some transparency
#   coord_sf(xlim = c(-84,-75),
#            ylim = c(36, 40),
#            expand = FALSE) +
#   labs(title = "Average Annual Energy Cost ($) for Counties in Virginia", caption = "Source: U.S. Department of Energy") +
#   theme(
#     panel.background = element_rect(fill = "#F0F0F0"),
#     panel.grid = element_line(color = "#F0F0F0"),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     plot.background = element_rect(fill = "#F0F0F0"),
#     plot.caption = element_text(hjust = 0.5, face = "italic"),
#     plot.subtitle = element_text(
#       family = "Helvetica",
#       hjust = 0.5,
#       color = "dimgrey"
#     ),
#     plot.title = element_text(
#       family = "Helvetica",
#       hjust = 0.5,
#       color = "dimgrey"
#     ),
#     legend.title = element_text(size = 7),
#     legend.text = element_text(size = 7),
#     legend.justification = "center",
#     legend.background = element_rect(fill = "#F0F0F0"),
#     text = element_text(family = "Helvetica", color = "dimgrey")
#   )
# va_avg_annual_energy_cost

#energy burden map showing average energy expenditures as percent of income by county
# va_avg_annual_energy_percent_exp <- ggplot() +
#   geom_sf(data = virginia_outline, fill = NA, color = "dimgrey") +
#   geom_sf(data = va_energy_equity_by_county,
#           aes(
#             fill = avg_energy_burden_as_percent_income,
#             text = paste0(
#               county,
#               "\nEnergy Expenditures: ",
#               avg_energy_burden_as_percent_income,
#               "%"
#             )
#           )) +
#   scale_fill_gradientn(name = "Average Annual Energy Cost \nas Percentage of Income\n", colors =
#                          ceps_pal[1:5]) + #setting alpha adds some transparency
#   coord_sf(xlim = c(-84,-75),
#            ylim = c(36, 40),
#            expand = FALSE) +
#   labs(title = "Average Energy Burden (% income) for Counties in Virginia", caption = "Source: U.S. Department of Energy") +
#   theme(
#     panel.background = element_rect(fill = "#F0F0F0"),
#     panel.grid = element_line(color = "#F0F0F0"),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     plot.background = element_rect(fill = "#F0F0F0"),
#     plot.caption = element_text(hjust = 0.5, face = "italic"),
#     plot.subtitle = element_text(
#       family = "Helvetica",
#       hjust = 0.5,
#       color = "dimgrey"
#     ),
#     plot.title = element_text(
#       family = "Helvetica",
#       hjust = 0.5,
#       color = "dimgrey"
#     ),
#     legend.title = element_text(size = 7),
#     legend.text = element_text(size = 7),
#     legend.justification = "center",
#     legend.background = element_rect(fill = "#F0F0F0"),
#     text = element_text(family = "Helvetica", color = "dimgrey")
#   )
# va_avg_annual_energy_percent_exp
# 
# va_avg_annual_energy_cost_p <-
#   ggplotly(va_avg_annual_energy_cost, tooltip = "text") %>%
#   layout(
#     title = list(
#       text = paste0("Average Annual Energy Cost ($) for Counties in Virginia"),
#       titlefont = list(size = 15)
#     ),
#     xaxis = list(
#       title = paste0(
#         "<br>",
#         "<i>",
#         "<sub>",
#         "Source: U.S. Department of Energy",
#         "<sub>",
#         "<i>"
#       ),
#       titlefont = list(size = 14)
#     )
#   ) %>%
#   config(
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = c(
#       "pan2d",
#       "select2d",
#       "lasso2d",
#       "zoom2d",
#       "autoScale2d",
#       "resetScale2d",
#       "toggleSpikelines"
#     )
#   )
# va_avg_annual_energy_cost_p
# 
# va_avg_annual_energy_percent_exp_p <-
#   ggplotly(va_avg_annual_energy_percent_exp, tooltip = "text") %>%
#   layout(
#     title = list(
#       text = paste0("Average Energy Burden (% income) for Counties in Virginia"),
#       titlefont = list(size = 15)
#     ),
#     xaxis = list(
#       title = paste0(
#         "<br>",
#         "<i>",
#         "<sub>",
#         "Source: U.S. Department of Energy",
#         "<sub>",
#         "<i>"
#       ),
#       titlefont = list(size = 14)
#     )
#   ) %>%
#   config(
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = c(
#       "pan2d",
#       "select2d",
#       "lasso2d",
#       "zoom2d",
#       "autoScale2d",
#       "resetScale2d",
#       "toggleSpikelines"
#     )
#   )
# va_avg_annual_energy_percent_exp_p
#----------------------------------------------------------------------------------------
