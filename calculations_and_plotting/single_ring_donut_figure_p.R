#' single_ring_donut_figure_p
#'
#' @param data_table
#' @param description_of_goal
#' @param top_description
#' @param bottom_description
#' @param hover_info
#' @param colors_list
#' @param character_list
#' @param source_citation
#'
#' @return figure
#' @export
single_ring_donut_figure_p <- function(data_table,description_of_goal,top_description,bottom_description,hover_info,colors_list,character_list=NULL,source_citation=NULL){
  #data_table must contain 2 columns: category & value where category is the appropriate label to appear on the figure and value is the appropriate value to fil the donut
  #       *must be listed in a particular order to be displayed correctly (displayed counterclockwise)
  #       *order must be: category you want to be last followed by the order you want the categories to be in (except last category which has already been listed)
  #description_of_goal = character of description of goal category (ex:Renewable Generation)
  #top_description = character of description of text you want displayed at top of donut
  #bottom_description = character of description of text you want displayed at bottom of donut
  #hover_info = character description of what features you want the hover info to display (ex: label+value)
  #colors_list = character vector of colors in order corresponding to assignment to categories
  #character_list defaults to NULL, but if applicable should be a list of the relevant database data table names as strings, so that their source info can be extracted from metadata table
  #source_citation defaults to NULL, however if a source can't be pulled from the metadata table, the source should be set to be the citation description
  #       *if needed to be set, should be of form: "Source: U.S. Energy Information Administration" for example

  library(plotly)

  #building source citation if no source citation is given as an input
  if(is.null(source_citation)){
    source_list <- NULL

    for(table in character_list){
      source <- metadata[db_table_name==table,data_source_full_name]

      if(is.null(source_list))
      {source_list <- source}
      else
      {source_list <- c(source_list,source)}
    }
    source_list <-as.vector(unique(source_list))

    source_description <- NULL

    for (source in source_list){
      if(is.null(source_description))
      {source_description <- paste("Source:",source)}
      else
      {source_description <- paste0(source_description,", ",source)}
    }
  }
  else
  {source_description <- source_citation} #using input source citation if given

  figure <- plot_ly(textinfo="none",hoverinfo=hover_info) %>%
    add_pie(data = data_table, values = ~value, labels = ~category, sort = F,hole = 0.7,
            domain = list(x = c(0, 1), y = c(0, 1)),
            marker=list(colors=colors_list,
                        line=list(color="white",width=1))) %>%
    layout(title=list(text=top_description,font = list(color="dimgrey",size=11,family = "Helvetica"),x=0.55),showlegend = F,
           font = list(family="Helvetica",color="dimgrey",size=14),
           paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0") %>%
    add_annotations(x=0.5,y=0.5,text=paste0(description_of_goal,"<br>","<sub>",bottom_description),showarrow=F,font = list(color = "dimgrey",size = 14,family = "Helvetica")) %>%
    add_annotations(x=0.5,y=-0.09,text=paste0("<i>","<sub>",source_description,"<sub>","</i>"),showarrow=F,font = list(color = "dimgrey",size = 13,family = "Helvetica"))%>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","select2d","lasso2d","hoverCompareCartesian","zoom2d"))
  figure

  return(figure)
}
