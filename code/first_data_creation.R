library('dplyr')
library('rvest')

data_out2
data3 <- data_out2
data3$categorie <- ''

## find categories for products
url_base <- "https://kolonial.no/sok/produkter/?q="

for (j in seq_along(data3$categorie)) {
  product <- data_out2$item[j]
  
  url_search <- paste0(url_base, stringr::str_replace_all(product, ' ', '+'))
  
  html <- url_search %>% 
    read_html()
  
  search_data <- html %>% 
    html_nodes('.product-list-item')
  
  search_data2 <- search_data %>% 
    html_nodes('a') %>% 
    html_attr('href')
  
  ## get first search result
  url2 <- search_data2[1]
  
  html2 <- paste0('https://kolonial.no', url2) %>% 
    read_html()
  
  product_data <- html2 %>% 
    html_nodes('.breadcrumb') %>%
    html_nodes('li') %>% 
    html_attr('id') %>% 
    stringr::str_replace(., 'breadcrumb-', '')
  product_data
  
  data3[j, ]$categorie <- product_data[2]  
}

data3


