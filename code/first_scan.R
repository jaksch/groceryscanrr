library('abbyyR')
library('dplyr')
library('rvest')
library('stringr')

vignette("overview", package = "abbyyR")
vignette("example", package = "abbyyR")

setapp(c("groceryscanrr", "eXsaEMlGowRdv5xJePVc+KI3"))

info <- getAppInfo()

processImage(file_path="C:\\image_test\\img1.jpg", 
             language="Norwegian", profile="textExtraction")

tasks <- listTasks(fromDate = "2016-07-10T00:00:00Z")
url_download <- as.character(finished_list$resultUrl[2])

data <- read_html(url_download) %>% 
  html_text()
data
backup <- data
data2 <- str_split(data, '\\r\\n')
data2

data_out <- tibble::tibble(item = '',
                           check = '',
                           amount = '')
data2 <- unlist(data2)

for (i in seq_along(data2)) {
  data_out[i, ]$item <- str_extract(data2[i], '^[A-Z].*[A-Z]')
  data_out[i, ]$check <- str_extract(data2[i], '[0-9]+[%]')
  data_out[i, ]$amount <- str_extract(data2[i], '[0-9]+[,|.][0-9]+')
}
data_out %>% as.data.frame()

remove_words <- c('løsvekt|LØSVEKT', 'stykk|STYKK', '[0-9]+[a-zA-Z]*', 
                  'skorpefri|SKORPEFRI')

data_out2 <- data_out %>% 
  filter(!is.na(item), !is.na(check), !is.na(amount)) %>% 
  select(-check) %>% 
  mutate(item = stringr::str_replace(item, '[,]|[.]', ''),
         item = stringr::str_replace(item, paste0(remove_words, collapse = '|'), '') %>% str_trim(),
         amount = stringr::str_replace(amount, ',', '.') %>% as.numeric())
data_out2



