#' theme_ceps
#'
#' @return
#' @export
theme_ceps <- function() {
  theme(
    # text
    text = element_text(family = "Helvetica",color = "dimgrey"),
    # title
    plot.title =element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
    plot.caption =element_text(family="Helvetica",hjust=0.5,color="dimgrey",face = "italic"),
    plot.subtitle = element_text(family="Helvetica",hjust=0.5,color="dimgrey"),
    # color background
    panel.background = element_rect(fill = "#F0F0F0"),
    plot.background=element_rect(fill="#F0F0F0"),
    # modify grid
    panel.grid = element_line(colour = "white",  size = 0.5),
    # modify axis
    axis.text.y = element_text(color = "dimgrey", face = "italic", family = "Helvetica",angle=30),
    axis.text.x = element_text(color = "dimgrey", face = "italic", family = "Helvetica"),
    axis.title = element_text(color = "dimgrey", family = "Helvetica"),
    axis.ticks = element_line(color = "white"),
    # legend
    legend.justification = "center",
    legend.background = element_rect(fill = "#F0F0F0"),
    legend.text = element_text(family="Helvetica",color = "dimgrey")
  )
}
