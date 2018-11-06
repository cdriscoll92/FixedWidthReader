
fwf_read <- function(file, widths, header = F, sep = '\t',
                     varnames = NULL, rownames = "NULL"){

  txt <- readLines(file)
  num_cols <- length(widths)
  df <- data.frame(matrix(ncol = num_cols, nrow = length(txt)))

  if(header == T){
    varnames <- strsplit(df[,1], sep)[[1]]
    df <- df[-1,]
  }

  df[,1] <- substr(txt, 1, widths[1])
  for(i in 2:length(widths)){
    this_col <- substr(txt, sum(widths[1:(i-1)])+1, sum(widths[1:i]))
    if(length(grep("\\D", this_col)) == 0){

      df[,i] <- as.numeric(this_col)
    }else{
      df[,i] <- this_col
    }
  }

  df_out <- df

  ## Naming rows/columns
  if(!missing(varnames)){
    if(length(varnames)!= num_cols){
      stop("Variable name and widths lengths do not match")
    }
    colnames(df_out) <- varnames
  }else{
    colnames(df_out) <- paste("x", c(1:num_cols), sep = "")
  }
  if(!missing(rownames)){
    if(length(rownames)!= length(txt)){
      stop("Number of row names does not match number of rows")
    }
    rownames(df_out) <- rownames
  }
  return(df_out)
}





