#' line_figure
#'
#' @param data_table_list
#' @param merge_variable
#' @param value_unit
#' @param title_name
#' @param character_list
#' @param x_label
#' @param lower_limit
#' @param upper_limit
#' @param return_static
#' @param source_citation
#' @param modifications
#' @param modifications2
#' @param subtitle_description
#' @param future_date
#'
#' @return figure
#' @export
line_figure <- function(data_table,y_value_unit,title_name,character_list=NULL,x_label="Year",lower_limit=0,upper_limit=NA,return_static=TRUE,source_citation=NULL,modifications=NULL,modifications2=NULL,subtitle_description=NULL,future_date=NULL){
  #data_table_list is a list of data tables which should be ready to be merged into one table
  #       *if only one table is included in input list (note that it still must be in list form), this table should be ready to be plotted i.e it should include a variable and value column and an x-value (usually date or year) column
  #merge_variable is a character description of which variable the merge should be performed on (ex:"date","year) if applicable; it should also be the x-axis being graphed
  #value_unit = character description of units of value being plotted
  #title_name = character description of what title of figure should be
  #character_list defaults to NULL, but if applicable should be a list of the relevant database data table names as strings, so that their source info can be extracted from metadata table
  #x_label defaults to "Year" but can be substituted with another character if Year is not appropriate xlabel
  #lower_limit defaults to 0, but can be changed to another numeric value appropriate for the data
  #upper_limit defaults to NA, but can be adjusted if needed
  #return_static defaults to TRUE, in which case a ggplot opbject will be returned
  #       *if FALSE, a list containing the ggplot figure, the x axis label name, and the citation will be returned
  #source_citation defaults to NULL, however if a source can't be pulled from the metadata table, the source should be set to be the citation description
  #       *if needed to be set, should be of form: "Source: U.S. Energy Information Administration" for example
  #modifications defaults to NULL, in which case nothing would be added to the figure, but can be set if additional modifications are needed
  #       *examples of different modifications which may be necessary are scaling the y-axis or removing the legend
  #subtitle_description defaults to NULL, but can be added if desired
  #future_date defaults to NULL and should only be given a value if the plot is showing future and past values, and it is desired that the future values be dashed
  #       *the format of future_date must match the format of whatever the x unit variable is
  #       *ex: if year is on x-axis, future_date = 2021 or if date (in y/m/d format) is on x-axis, future_date = '2021-01-01'
  #       *note also that this only works when there is both historic and future data

  library(ggplot2)

  source_description = build_source_list(character_list,source_citation)

  category_count <- length(unique(data_table$fill_variable))
  ceps_pal <- c("#00A087B2", "#3C5488B2", "#CEA5AC", "#BE7E8A", "#4DBBD5B2", "#91D1C2B2","#D9C6C9","#8491B4B2","#5868AC","#6FB3D9","#56BD96","#99A9E2","#A94F64","#B0DEFA","#99EEBB","#8FD3FE")

  if(is.null(future_date)){
    figure <- ggplot(data_table, aes(x=x_value,y=y_value,color=fill_variable)) +
      geom_line(aes(group=fill_variable,
                    text=paste0(x_label,": ",x_value,"\n","Variable: ",fill_variable,"\n","Value: ",round(y_value,4)))) +
      ylab(y_value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
      labs(title=title_name,subtitle=subtitle_description,caption=source_description) +
      scale_color_manual(name=NULL,values=ceps_pal[1:category_count])+
      theme_ceps()+
      modifications+
      modifications2
    figure
  }
  else{
    figure <- ggplot() +
      geom_line(data=data_table[x_value<future_date],mapping=aes(x=x_value,y=y_value,color=fill_variable,group=fill_variable,
                                           text=paste0(x_label,": ",x_value,"\n","Variable: ",fill_variable,"\n","Value: ",round(y_value,4)))) +
      geom_line(data=data_table[x_value>=future_date],mapping=aes(x=x_value,y=y_value,color=fill_variable,group=fill_variable,
                                           text=paste0(x_label,": ",x_value,"\n","Variable: ",fill_variable,"\n","Value: ",round(y_value,4))),linetype="dashed") +
      ylab(y_value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
      labs(title=title_name,subtitle=subtitle_description,caption=source_description) +
      scale_color_manual(name=NULL,values=ceps_pal[1:category_count])+
      theme_ceps()+
      modifications+
      modifications2
    figure
  }

  return_list <- list(figure=figure,x_label=x_label,source_description=source_description,title_name=title_name,subtitle_description=subtitle_description,y_label=y_value_unit)

  if(return_static==TRUE)
  {return(figure)}
  else
  {return(return_list)}
}
