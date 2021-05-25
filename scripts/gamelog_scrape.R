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
    colnames(temp_table) <- paste0("var", seq(1:ncol(temp_table)))
    
    temp_table <- temp_table[var1 != 'Rk', ]
    temp_table <- temp_table[1:nrow(temp_table)-1, ]
    temp_table <- temp_table[, .(year = var2
                                 , week = var5
                                 , g_count = .I
                                 , ret_count = as.numeric(var11)
                                 , rec_count = as.numeric(var12)
                                 , rey_count = as.numeric(var13)
                                 , retd_count = as.numeric(var14)
                                 , rua_count = as.numeric(var27)
                                 , ruy_count = as.numeric(var28)
                                 , rutd_count = as.numeric(var29)
    )]
    temp_table[, ttd_count := sum(retd_count, rutd_count), by = 1:nrow(temp_table)]
    temp_table[, g_cum := cumsum(g_count)]
    temp_table[, ret_cum := cumsum(ret_count)]
    temp_table[, rec_cum := cumsum(rec_count)]
    temp_table[, rey_cum := cumsum(rey_count)]
    temp_table[, retd_cum := cumsum(retd_count)]
    temp_table[, rua_cum := cumsum(rua_count)]
    temp_table[, ruy_cum := cumsum(ruy_count)]
    temp_table[, rutd_cum := cumsum(rutd_count)]
    temp_table[, ttd_cum := cumsum(ttd_count)]

    return(temp_table)
    
  }
  
  woop_woop <- lapply(as.list(nodes_list$url)[1:10], scrape_gamelog_advanced)
