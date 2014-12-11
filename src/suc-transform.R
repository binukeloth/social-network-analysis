library(data.table)

stringsAsFactors=FALSE


# Transform into from,to rows
# save to file / invoke network analytics


## ----------------- For Subex User Conf ----------------------------------
loadData = function(dataFile, header = TRUE, sep = ',') {
  data.frame(read.csv(file=dataFile, header=header, sep=",", stringsAsFactors=FALSE));
}

prepareData = function (rawData, subex=FALSE)
{  
  to = character();
  from = character();
  comp = character();
  attendee = list();  
  

  for(i in 1:nrow(rawData)) {
    
    for (col in names(rawData)) {
      #       if(subex == TRUE && 
      #            ((rawData[i, "Company"] == 'Subex') ||
      #             (col == 'Subex.Limited')))
      #         continue;
      
      
      
      if(col == "Name") {
        # print(paste0("Processing for ", rawData[i, "Name"]));
        attendee[[tolower(trim(rawData[i, "Name"]))]] = c(lrtrim(rawData[i, "Name"]), rawData[i, "Company"]) ;
      }
      else if (col == "Company" || col == "Department" || col == "Email" || col == "Function"
               || col == "Sub.Function") {        
      }      
      else {      
        
        values = unlist(strsplit(as.character(rawData[[col]][i]), "[,]"));        
        
        for(val in values) {
          if(!is.na(val)) {          
            to = append(to, lrtrim(val));
            from = append(from, lrtrim(rawData[i, "Name"]));
            
            #attendee[[tolower(trim(val))]] = c(lrtrim(val), col);
          }
        }       
      }        
    }    
  }
  
  df = data.frame(from, to, stringsAsFactors=FALSE);
  
  # print(head((names)));  
  #write.csv(attendee, 'D:/WorkSpace/R/SNA/data/all-names.csv');
  write.csv(df, 'D:/WorkSpace/R/SNA/data/sub-sna.csv');  
}
  
lrtrim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim <- function (x) gsub("\\s+", "", x)

## ----------------- For Subex User Conf ----------------------------------

