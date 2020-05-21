#for donut figures: 
donut_figure <- function(data_value,data_year,data_with_true_units,goal_value,goal_year,goal_with_true_units,description_of_goal,light_color,dark_color,end_goal_value=NULL,end_goal_year=NULL,end_goal_with_true_units=NULL,darkest_color=NULL){
  #data_value = numerical value of historical data (as decimal between 0 & 1)
  #note: if the data is not a percent, such as percent renewable generation in 2019, make it a decimal by dividing the data value by the goal value
  #data_year = year data is from as character
  #data_with_true_units = character of description of data in natural units: %, MW, etc
  #goal_value = numerical value of goal (as decimal between 0 & 1)
  #note: if the goal is not a percent: goal_value should = 1 if it is the end goal, goal_value should equal intermediate goal value divided by end goal value if goal featured is not end goal 
  #goal_year = year goal is to be reached as character
  #goal_with_true_units = character of description of goal in natural units: %, MW, etc
  #description_of_goal = character of description of goal category (ex:Renewable Generation)
  #light_color = choice of lighter color, as a character
  #dark_color = choice of darker color, as a character (note: light_color and dark_color should be light and dark versions of same color) 
  #end_goal variations default as NULL, but can used if there is a need for three rings: data, and intermediate goal, and an end goal
  
  require(ggplot2)
  
  #components of each ring are the actual value we want to display, and 1-that value as a filler so proportions of the ring are correct
  #the filler is called "zfiller" because the components of the rings are graphed in alphabetical order
  ring1 = data.frame(category=c("zfiller",paste0(description_of_goal,data_year)),
                     value=c(1-data_value,data_value)) #ring1 data.frame consists of elements that will make up outer ring
  ring2 = data.frame(category=c("zfiller",paste0(description_of_goal,goal_year)),
                     value=c(1-goal_value,goal_value)) #ring2 data.frame consists of elements that will make up inner ring
  
  #now we use the ring data frames to plot the donut figure
  if (is.null(end_goal_value)){
    donut <- ggplot()+
      geom_bar(data=ring1,mapping=aes(x=2.5,y=value,fill=category),stat="identity",color="white")+ #x=2.5 so that ring1 is constructed as the outer ring
      geom_bar(data=ring2,mapping=aes(x=1.5,y=value,fill=category),stat="identity",color="white")+ #x=1.5 so that ring2 is constructed as the inner ring
      coord_polar(theta="y",start=0)+ #polar coordinates are used to achieve donut shape
      scale_fill_manual(name=NULL,
                        breaks=c(paste0(description_of_goal,data_year),paste0(description_of_goal,goal_year),"zfiller"),
                        values=c(dark_color,light_color,"snow2"))+ #scale_fill_manual assigns the dark color to the data, the light color to the goal, and a grey color to the filler values
      theme_void()+ #theme_void() is necessary to remove axis marks and plotlines/background
      xlim(-2, 3)+ #with these x limits, each ring is the same width and there is large enough blank center for a description
      labs(title=paste0(" ",data_with_true_units),subtitle=paste0("in ",data_year),caption=paste0(goal_with_true_units," by ",goal_year))+
      geom_text(aes(x=-2,y=0,label=paste0(description_of_goal)),color=dark_color)+
      theme(plot.title = element_text(hjust=0.5,vjust=0,size=22,color=dark_color),
            plot.subtitle = element_text(hjust=0.5,vjust=0,size=10,color=dark_color),
            plot.caption=element_text(hjust=0.5,vjust=1,size=22,color=light_color),
            legend.position="none")
  }
  else{
    ring3 = data.frame(category=c("zfiller",paste0(description_of_goal,end_goal_year)),
                       value=c(1-end_goal_value,end_goal_value)) #ring3 data.frame consists of elements that will make up innermost ring
    
    donut <- ggplot()+
      geom_bar(data=ring1,mapping=aes(x=2.5,y=value,fill=category),stat="identity",color="white")+
      geom_bar(data=ring2,mapping=aes(x=1.75,y=value,fill=category),stat="identity",color="white")+ 
      geom_bar(data=ring3,mapping=aes(x=1,y=value,fill=category),stat="identity",color="white")+ 
      coord_polar(theta="y",start=0)+
      scale_fill_manual(name=NULL,
                        breaks=c(paste0(description_of_goal,data_year),paste0(description_of_goal,goal_year),paste0(description_of_goal,end_goal_year),"zfiller"),
                        values=c(darkest_color,dark_color,light_color,"snow2"))+ 
      theme_void()+
      xlim(-2, 3)+ 
      labs(title=paste0(" ",data_with_true_units),subtitle=paste0("in ",data_year),caption=paste0(goal_with_true_units," by ",goal_year,"\n",end_goal_with_true_units," by ",end_goal_year))+
      geom_text(aes(x=-2,y=0,label=paste0(description_of_goal)),color=darkest_color,size=3.5)+
      theme(plot.title = element_text(hjust=0.5,vjust=0,size=22,color=darkest_color),
            plot.subtitle = element_text(hjust=0.5,vjust=0,size=10,color=dark_color),
            plot.caption=element_text(hjust=0.5,vjust=1,size=22,color=light_color),
            legend.position="none")
  }
  return(donut)
}

#for timeseries stacked area figures by particular category:
stacked_area_figure <- function(data_table,value_unit,title_name,annual=TRUE,x_label="Year",subtitle_name=NULL){
  #data_table must have three columns: year (or date if monthly data is being plotted where date must be of form "1990-01-01" for example), variable, and value
  #value_unit = character description of units of value being plotted
  #title_name = character description of what title of figure should be
  #annual is a logical variable, which defualts to TRUE, if non-annual data is being plotted, annual should be FALSE
  #x_label defaults to "Year" but can be substituted with another character if Year is not appropriate xlabel
  #subtitle_name defaults to NULL, but can be set equal to a character if a subtitle is desired
  
  require(ggplot2)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  require("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels
  
  variable_elements <- unique(data_table$variable) #selects the unique elements of the variable column
  variable_elements <- as.vector(variable_elements) #saves unique elements as character vector
  
  good_names = gsub("_"," ",variable_elements) #subtitutes "_" from variable name with a space to create legend labels
  good_names = gsub("apco","APCO",good_names) #deals with specific case if "apco" is included in a variable name, APCO will be used in the legend label
  good_names = capitalize(good_names) #capitalizes first word of legend labels
  
  if (annual==TRUE){
    figure <- ggplot(data_table, aes(x=year,y=value,fill=variable)) +
      geom_area() + 
      ylab(value_unit) + xlab(x_label) + ylim(0,NA) +
      labs(title=title_name,subtitle=subtitle_name) +
      scale_fill_discrete(name=NULL,breaks=variable_elements,labels=good_names)
    figure
  }
  else{
    figure <- ggplot(data_table, aes(x=date,y=value,fill=variable)) +
      geom_area() + 
      ylab(value_unit) + xlab(x_label) + ylim(0,NA) +
      labs(title=title_name,subtitle=subtitle_name) +
      scale_fill_discrete(name=NULL,breaks=variable_elements,labels=good_names)
    figure
  }
  return(figure)
}

#for timeseries line figures by particular category:
line_figure <- function(data_table,value_unit,title_name,annual=TRUE,x_label="Year",subtitle_name=NULL){
  #data_table must have three columns: year (or date if monthly/daily data is being plotted where date must be of form "1990-01-01" for example), variable, and value
  #value_unit = character description of units of value being plotted
  #title_name = character description of what title of figure should be
  #annual is a logical variable, which defualts to TRUE, if monthly or daily data is being plotted, annual should be FALSE
  #x_label defaults to "Year" but can be substituted with another character if Year is not appropriate xlabel
  #subtitle_name defaults to NULL, but can be set equal to a character if a subtitle is desired
  
  require(ggplot2)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  require("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels
  
  variable_elements <- unique(data_table$variable) #selects the unique elements of the variable column
  variable_elements <- as.vector(variable_elements) #saves unique elements as character vector
  
  good_names = gsub("_"," ",variable_elements) #subtitutes "_" from variable name with a space to create legend labels
  good_names = gsub("apco","APCO",good_names) #deals with specific case if "apco" is included in a variable name, APCO will be used in the legend label
  good_names = capitalize(good_names) #capitalizes first word of legend labels
  
  if (annual==TRUE){
    figure <- ggplot(data_table, aes(x=year,y=value,color=variable,shape=variable)) +
      geom_line() + 
      geom_point() +
      ylab(value_unit) + xlab(x_label) + ylim(0,NA) +
      labs(title=title_name,subtitle=subtitle_name) +
      scale_color_discrete(name=NULL,breaks=variable_elements,labels=good_names)+
      scale_shape_discrete(name=NULL,breaks=variable_elements,labels=good_names)
    figure
  }
  else{
    figure <- ggplot(data_table, aes(x=date,y=value,color=variable,shape=variable)) +
      geom_line() + 
      geom_point() +
      ylab(value_unit) + xlab(x_label) + ylim(0,NA) +
      labs(title=title_name,subtitle=subtitle_name) +
      scale_color_discrete(name=NULL,breaks=variable_elements,labels=good_names)+
      scale_shape_discrete(name=NULL,breaks=variable_elements,labels=good_names)
    figure
  }
  return(figure)
}

#note: because all these functions return ggplot2 figures, additional different desired plot elements can be added in conjunction with the use of the functions
