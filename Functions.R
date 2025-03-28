
drop_one_value_col <- function(df, prt_val = FALSE){ 
  # browser()
  df_id <- ensym(df)
  if(prt_val){
    msg = paste("Looking for single value columns in data frame: ",as.character(df_id) )
    print(msg)}
  ## takes whole dataframe
  dropc <- NULL
  val <- NULL
  ## test each column for a single value
  for(i in 1:dim(df)[2]){   
    if(dim(distinct(df[,i]))[1] == 1){
      dropc <- c(dropc, i)
      val <- c(val, df[1,i])
    }
  } 
  
  if(prt_val){
    if(is.null(dropc)){
      print("No columns dropped")
      return(df)}else{
        print("Columns dropped:")
        # print(colnames(df)[drop])
        print(unlist(val))
        df <- df[, -1*dropc]
        return(df)
      }
  }
  df <- df[, -1*dropc]
  return(df)
}
