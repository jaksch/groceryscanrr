
# data_out2
data3 <- data_out2
data3$categorie <- ''
data3$categorie2 <- ''

## find categories for products
url_base <- "https://kolonial.no/sok/produkter/?q="

for (j in seq_along(data3$categorie)) {
  not_found_flag <- 0
  product <- data3$item[j]
  product_url_encoded <- urltools::url_encode(product)

  url_search <- paste0(url_base, product_url_encoded)

  html <- url_search %>%
    read_html()

  search_data <- html %>%
    html_nodes('.product-list-item')

  ## check if any search results
  if (length(search_data) == 0) {
    ## if search contains a space search for inviduel words
    if (str_detect(product, ' ')) {
      product_new <- str_split(product, ' ') %>% unlist()
      product_new_encoded <- urltools::url_encode(product_new)

      url_search <- paste0(url_base, product_new_encoded)
    }
    if (str_detect(product, 'BÆREPOSE|BEREPOSE|BAREPOSE|EPOSE')) {
      product_new <- 'kildesorteringsposer'
      url_search <- paste0(url_base, product_new)
    }

    ## try one word at the time
    for (word in seq_along(url_search)) {
      html <- url_search[word] %>%
        read_html()

      search_data <- html %>%
        html_nodes('.product-list-item')

      if (length(search_data) != 0) {break}
    }

    ## if no match are found
    if (length(search_data) == 0) {
      not_found_flag <- 1
      product_new <- 'kildesorteringsposer'
      url_search <- paste0(url_base, product_new)
      html <- url_search %>%
        read_html()

      search_data <- html %>%
        html_nodes('.product-list-item')
    }

  }

  search_data2 <- search_data %>%
    html_nodes('a') %>%
    html_attr('href')

  ## get first search result
  url2 <- search_data2[1]
  if (str_detect(product, 'EGG')) {
    ## first search result for 'EGG' is eggnudles
    url2 <- search_data2[2]
  }

  html2 <- paste0('https://kolonial.no', url2) %>%
    read_html()

  product_data <- html2 %>%
    html_nodes('.breadcrumb') %>%
    html_nodes('li') %>%
    html_attr('id') %>%
    stringr::str_replace(., 'breadcrumb-', '')
  product_data

  data3[j, ]$categorie <- product_data[2]
  data3[j, ]$categorie2 <- product_data[3]

  ## print unknow if search dont return any results
  if (not_found_flag == 1) {
    data3[j, ]$categorie <- 'unknown'
    data3[j, ]$categorie2 <- 'unknown'
  }
}

data3

load('.\\db_data\\grocery_data.RData')
grocery_data <- bind_rows(grocery_data, data3)
save(grocery_data, file = '.\\db_data\\grocery_data.RData')
