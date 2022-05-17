#' build_source_list
#'
#' @param character_list
#' @param source_citation
#'
#' @return source_description
#' @export
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
