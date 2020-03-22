# The North_America project contains many test addresses that could be used to
# test the NYC region. Select the test addresses within NY, NJ, PA, or CT.

# expected to be run in pelias_docker/projects/nyc_region
# setwd("~/dev/pelias-docker/projects/nyc-region/")
library(jsonlite)

na_test_dir = "../north-america/test_cases"
output_test_dir = "test_cases"
if(!dir.exists(output_test_dir)) dir.create(output_test_dir)

test_files = list.files(na_test_dir, pattern = ".json$")

states = c("NY", "NJ", "PA", "CT",
           "ny", "nj", "pa", "ct", 
           "New York", "New Jersey", "Pennsylvania", "Connecticut", 
           "new york", "new jersey", "pennsylvania", "connecticut")

filter_json = function(test_json){
  na_search = fromJSON(file.path(na_test_dir, test_json))
  orig_rows = nrow(na_search[["tests"]])

  # check if the row contains one of the states in the nested expected/label object
  check_row = function(r, DF){ 
    expected = any(unlist(sapply(states, function(s) grep(s, DF[r, "expected"][[1]][[1]]))))  
    input = any(unlist(sapply(states, function(s) grep(s, DF[r, "in"][[1]]))))  
    expected|input
  }
  in_region = lapply(1:nrow(na_search[["tests"]]), FUN = check_row, DF = na_search[["tests"]])

  na_search[["tests"]] <- na_search[["tests"]][unlist(in_region), ]
  
  # only write out filtered json if any of the addresses were in the selected states
  if(any(in_region)){
    outfile = file(file.path(output_test_dir, test_json))
    writeLines(toJSON(na_search, pretty = TRUE, auto_unbox = TRUE), outfile)
    close(outfile)  
  }
  
  paste0(test_json, ": ", sum(unlist(in_region)),"/", orig_rows)
}

# run and display filter results
unlist(lapply(test_files, filter_json))
