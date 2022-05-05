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
                    text=paste0(x_label,": ",x_value,"\n","Variable: ",fill_variable,"\n","Value: ",format(y_value,big.mark=",",digits=4)))) +
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
                                                                 text=paste0(x_label,": ",x_value,"\n","Variable: ",fill_variable,"\n","Value: ",format(y_value,big.mark=",",digits=4)))) +
      geom_line(data=data_table[x_value>=future_date],mapping=aes(x=x_value,y=y_value,color=fill_variable,group=fill_variable,
                                                                  text=paste0(x_label,": ",x_value,"\n","Variable: ",fill_variable,"\n","Value: ",format(y_value,big.mark=",",digits=4))),linetype="dashed") +
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
    add_annotations(x=0.5,y=-0.09,text=paste0("<i>","<sub>",source_description,"<sub>","</i>"),showarrow=F,font = list(color = "dimgrey",size = 13,family = "Helvetica"))
  #config(displaylogo = FALSE,
  # modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","select2d","lasso2d","hoverCompareCartesian","zoom2d"))
  figure
  
  return(figure)
}

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
             paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0")
  }
  else{
    figure <- plot_ly(data_table,labels=~variable,values=~value,type='pie',hoverinfo="percent+label",marker=list(colors=theme_colors),sort=F,text=pct,textposition = "inside",textinfo = "text") %>%
      layout(title=list(text=title_name,x=0.5,xref='paper',yref='paper',font=list(size=15,family = "Helvetica",color="dimgrey")),
             annotations=list(x=0.5,y=-0.1,text=paste0("<i>","<sub>",source_description,"<sub>","</i>"),showarrow=F,xref='paper',yref='paper',font=list(size=14,family = "Helvetica",color="dimgrey")),
             font = list(family="Helvetica",color="dimgrey"),
             paper_bgcolor="#F0F0F0", plot_bgcolor="#F0F0F0",
             legend = list(x = 100, y = 0.5))
  }
  return(figure)
}

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

ggplotly_wrapper <- function(list){
  #list should be list output from ggplot functions, which includes ggplot figure (figure), x label name (x_label), and data citation (source_description)
  #line_figure defaults to FALSE, but should be set as TRUE if the figure is a line plot to avoid duplicate hover info on plotly figure
  
  library(plotly)
  
  figure_p <- ggplotly(list$figure,tooltip="text") %>%
    layout(title = list(text=paste0(list$title_name,"<br>","<sup>",list$subtitle_description,"</sup>"),font=list(size=15,family = "Helvetica")),
           xaxis=list(title = paste0(list$x_label,"<br>","<i>","<sub>",list$source_description,"<sub>","<i>"),titlefont=list(size=14,family = "Helvetica")),
           yaxis=list(title = list$y_label,titlefont=list(size=14,family = "Helvetica")),
           legend = list(x = 100, y = 0.5))
  #citation is built into x-axis label rather than as an annotation so that it does not move as plot margins change, which happens with plotly annotations
  #subtitle is built into second line of title
  
  return(figure_p)
}

build_source_list <- function(character_list=NULL,source_citation=NULL){
  #character_list defaults to NULL, but if applicable should be a list of the relevant database data table names as strings, so that their source info can be extracted from metadata table
  #source_citation defaults to NULL, however if a source can't be pulled from the metadata table, the source should be set to be the citation description
  #       *if needed to be set, should be of form: "Source: U.S. Energy Information Administration" for example
  #subtitle_description defaults to NULL, but can be added if desired
  
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
  {source_description <- source_citation}
  
  return(source_description=source_description)
}

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