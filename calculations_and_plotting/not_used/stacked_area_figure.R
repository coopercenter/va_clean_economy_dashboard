#' stacked_area_figure
#'
#' @param data_table
#' @param value_unit
#' @param title_name
#' @param character_list
#' @param x_label
#' @param lower_limit
#' @param upper_limit
#' @param return_static
#' @param source_citation
#' @param modifications
#' @param subtitle_description
#'
#' @return figure
#' @export
stacked_area_figure <- function(data_table,y_value_unit,title_name,character_list=NULL,x_label="Year",lower_limit=0,upper_limit=NA,return_static=TRUE,source_citation=NULL,modifications=NULL,subtitle_description=NULL){
  #data_table is a melted data table with "variable" formatted to be legend lables
  #           variable names: [x_value,y_value,fill_variable]
  #value_unit = character description of units of value being plotted
  #title_name = character description of what title of figure should be
  #character_list defaults to NULL, but if applicable should be a list of the relevant database data table names as strings, so that their source info can be extracted from metadata table
  #x_label defaults to "Year" but can be substituted with another character if Year is not appropriate xlabel
  #lower_limit defaults to 0, but can be changed to another numeric value appropriate for the data
  #upper_limit defaults to NA, but can be adjusted if needed
  #return_static defaults to TRUE, in which case a ggplot object will be returned
  #       *if FALSE, a list containing the ggplot figure, the x axis label name, and the citation will be returned
  #source_citation defaults to NULL, however if a source can't be pulled from the metadata table, the source should be set to be the citation description
  #       *if needed to be set, should be of form: "Source: U.S. Energy Information Administration" for example
  #modifications defaults to NULL, in which case nothing would be added to the figure, but can be set if additional modifications are needed
  #       *examples of different modifications which may be necessary are scaling the y-axis or removing the legend
  #subtitle_description defaults to NULL, but can be added if desired

  library(ggplot2)
 
  source_description = build_source_list(character_list,source_citation)
  category_count <- length(unique(data_table$fill_variable))
  ceps_pal <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")

  figure <- ggplot(data_table,aes(x=x_value,y=y_value,fill=fill_variable)) +
    geom_area(aes(group=fill_variable,
                  text=paste0(x_label,": ",x_value,"\n","Variable: ",fill_variable,"\n","Value: ",label_comma(accuracy=1,big.mark = ",")(y_value)))) + 
    ylab(y_value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
    labs(title=title_name,subtitle=subtitle_description,caption=source_description) +
    scale_fill_manual(name=NULL,values=ceps_pal[1:category_count]) +
    theme_ceps()+
    modifications
  figure

  return_list <- list(figure=figure,x_label=x_label,source_description=source_description,title_name=title_name,subtitle_description=subtitle_description,y_label=y_value_unit)

  if(return_static==TRUE)
  {return(figure)}
  else
  {return(return_list)}
}
