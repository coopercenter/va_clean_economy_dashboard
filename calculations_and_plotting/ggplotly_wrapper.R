#' ggplotly_wrapper
#'
#' @param list
#'
#' @return figure
#' @export
ggplotly_wrapper <- function(list){
  #list should be list output from ggplot functions, which includes ggplot figure (figure), x label name (x_label), and data citation (source_description)
  #line_figure defaults to FALSE, but should be set as TRUE if the figure is a line plot to avoid duplicate hover info on plotly figure

  library(plotly)

  figure_p <- ggplotly(list$figure,tooltip="text") %>%
    layout(title = list(text=paste0(list$title_name,"<br>","<sup>",list$subtitle_description,"</sup>"),font=list(size=15,family = "Helvetica")),
           xaxis=list(title = paste0(list$x_label,"<br>","<i>","<sub>",list$source_description,"<sub>","<i>"),titlefont=list(size=14,family = "Helvetica")),
           yaxis=list(title = list$y_label,titlefont=list(size=14,family = "Helvetica")),
           legend = list(x = 100, y = 0.5))%>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","select2d","lasso2d","zoom2d","autoScale2d","resetScale2d"))
  #citation is built into x-axis label rather than as an annotation so that it does not move as plot margins change, which happens with plotly annotations
  #subtitle is built into second line of title

  return(figure_p)
}
