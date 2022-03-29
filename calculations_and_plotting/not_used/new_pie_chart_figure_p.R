#' pie_chart_figure_p
#'
#' @param data_table_list
#' @param merge_variable
#' @param title_name
#' @param character_list
#' @param legend_shown
#' @param source_citation
#'
#' @return figure
#' @export
pie_chart_figure_p <- function(data_table,title_name=NULL,character_list=NULL,legend_shown=FALSE,source_citation=NULL){
  #data_table is one table
  #       *if only one table is included in input list (note that it still must be in list form), this table should be ready to be plotted i.e it should include a variable and value column and an x-value (usually date or year) column
  #       *value may be in GWh or whatever is the unit of what is being plotted, the values need not add to 100% or 1 they can be actual values
  #title_name defaults to NULL but can be set as a character if a title is desired
  #character_list defaults to NULL, but if applicable should be a list of the relevant database data table names as strings, so that their source info can be extracted from metadata table
  #legend_shown defaults to FALSE
  #       *if FALSE, no legend is shown and the name of each category and associated percent is displayed on the pie slice
  #       *if TRUE, legend is shown and only the percent is displaye on the pie slice, this may be a better optio if some slices are very small
  #source_citation defaults to NULL, however if a source can't be pulled from the metadata table, the source should be set to be the citation description
  #       *if needed to be set, should be of form: "Source: U.S. Energy Information Administration" for example
  #eventually when a custom theme is set, we can store the colors from that theme in a character vector called "theme_colors" then include "marker=list(colors=theme_colors)" as argument in plotly function

  library(plotly)
  library(scales) #contains ggplot default palette function

  #building source citation if no source citation is given as an input
  source_description = build_source_list(character_list,source_citation)

  category_count <- length(data_table$variable) #counts number of categories being graphed
  theme_colors <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")[1:category_count] #generates a list of that many colors to be assigned to each color (for now from ggplot default color palette)

  # Create sector labels
  total_sum <- data_table[value>=0,sum(value)]
  pct <- data_table[,round(value/total_sum,3)]
  pct[pct<0.1] <- 0  # Anything less than 10% should be blank
  pct <- paste0(pct*100, "%")
  pct <- gsub("^0%","",pct)

  if (legend_shown==FALSE){
    figure <- plot_ly(data_table,labels=~variable,values=~value,type='pie',hoverinfo="percent+label",marker=list(colors=theme_colors),sort=F,textinfo = "label+percent") %>%
      layout(title=list(text=title_name,x=0,xref='paper',yref='paper',font=list(size=15,family = "Helvetica",color="dimgrey")),
             showlegend=F,
             annotations=list(x=0.5,y=-0.1,text=paste0("<i>","<sub>",source_description,"<sub>","</i>"),showarrow=F,xref='paper',yref='paper',font=list(size=14,family = "Helvetica",color="dimgrey")),
             font = list(family="Helvetica",color="dimgrey"),
             paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")%>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","select2d","lasso2d","hoverCompareCartesian","zoom2d","autoScale2d","resetScale2d"))
  }
  else{
    figure <- plot_ly(data_table,labels=~variable,values=~value,type='pie',hoverinfo="percent+label",marker=list(colors=theme_colors),sort=F,text=pct,textposition = "inside",textinfo = "text") %>%
      layout(title=list(text=title_name,x=0.5,xref='paper',yref='paper',font=list(size=15,family = "Helvetica",color="dimgrey")),
             annotations=list(x=0.5,y=-0.1,text=paste0("<i>","<sub>",source_description,"<sub>","</i>"),showarrow=F,xref='paper',yref='paper',font=list(size=14,family = "Helvetica",color="dimgrey")),
             font = list(family="Helvetica",color="dimgrey"),
             paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0",
             legend = list(x = 100, y = 0.5))%>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","select2d","lasso2d","hoverCompareCartesian","zoom2d","autoScale2d","resetScale2d"))
  }
  return(figure)
}
