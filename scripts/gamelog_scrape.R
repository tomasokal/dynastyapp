library(data.table)
library(rvest)
library(xml2)
library(tinytest)

## Test implementation

  ### URL of receiving stats in a given year
  url <- 'https://www.pro-football-reference.com/years/2019/receiving.htm'
  
  ### Extract the nodes of the players in the table of the URL to gain hrefs of players
  nodes <- rvest::read_html(url) |> rvest::html_nodes(xpath = "//table//a")
  
  ### Unlist and create a table with name and href
  nodes_list <- lapply(xml2::xml_attrs(nodes), function(x) data.table::data.table(as.list(x), stringsAsFactors = FALSE))
  nodes_list <- data.table::rbindlist(nodes_list)
  nodes_list <- nodes_list[`V1` %like% "/players/"]
  nodes_list[, string := gsub(".htm", "", `V1`, fixed = TRUE)]
  nodes_list[, url := paste0("https://www.pro-football-reference.com"
                             , string
                             , "/gamelog/advanced/")]
  
  ### Extract the table itself
  players <- data.table::as.data.table(
    rvest::html_table(
      xml2::read_html(url)
      , header = TRUE
      , fill = TRUE
      )
    )
  )
  
  ### Remove the table breaks that are in the table for some reason?!?!
  players <- players[`Rk` != 'Rk', ]
  
  ### Quick testing
  test_rows_nodes <- nodes_list[, .N]
  test_rows_players <- players[, .N]
  
  tinytest::expect_equal(test_rows_nodes, test_rows_players)
  
  ### Lapply through list of URLs to pull list of gamelog data
  scrape_gamelog_advanced <- function(x) {

    temp_url <- as.character(x)
    temp_table <- data.table::data.table(rvest::html_table(xml2::read_html(x))[[1]])
    check <- c(paste0("V", ncol(temp_table)))
    
    temp_table <- temp_table[`V1` != 'Rk', ]
    temp_table <- temp_table[1:nrow(temp_table)-1, ]
    temp_table <- temp_table[, .(year = `V2`
                                 , week = `V5`
                                 , g_count = .N
                                 , rt_count = 11
                                 # , rt_cum
                                 # , ra_count
                                 # , ra_cum
                                 # , yrc_count
                                 # , yrc_cum
                                 # , yra_count
                                 # , yra_cum
                                 # , trc_count
                                 # , trc_cum
                                 # , tra_count
                                 # , tra_cum
                                 # , tt_count
                                 # , tt_cum
    )]
                                 
      
      
      year = Year
                                 )]

    return(temp_table)
    
  }
  
  test_list <- list(c(nodes_list[, 3]))
  woop_woop <- lapply(as.list(nodes_list$url)[1:10], scrape_gamelog_advanced)
