library('abbyyR')
library('dplyr')
library('rvest')
library('stringr')
library('Hmisc')

## remove private key from script

## use SQLite instead of .RData files

## make it easy to run script on already procsed images (move DB logic)

## split script into small functions

## create viz

## create shiny dashboard
  ## add upload function http://stackoverflow.com/questions/33526256/dynamically-display-images-from-upload-in-shiny-ui



## setup abbyy to my user
setapp(c("groceryscanrr", "eXsaEMlGowRdv5xJePVc+KI3"))

setwd('C:\\R\\github\\groceryscanrr')
load('.\\db_data\\images_processed.RData')
# images_processed

images_uploaded <- list.files('.\\images')

new_images <- images_uploaded[images_uploaded %nin% images_processed$image]

## for now only processes one new image
processImage(file_path = paste0(".\\images\\", new_images[1]),
             language = "Norwegian", profile = "textExtraction")

images_processed <- bind_rows(images_processed, tibble::tibble(image = new_images[1]))
save(images_processed, file = '.\\db_data\\images_processed.RData')

load('.\\db_data\\tasks_ids.RData')
completed_tasks <- tasks_ids

Sys.sleep(5)

## get completed tasks
tasks <- listTasks()
tasks2 <- tasks %>%
  filter(id %nin% completed_tasks$id)

## add the new task
new_tasks_ids <- tibble::tibble(id = as.character(tasks2$id[1]))
tasks_ids <- bind_rows(tasks_ids, new_tasks_ids)
save(tasks_ids, file = '.\\db_data\\tasks_ids.RData')

## download the image data
url_download <- as.character(tasks2$resultUrl[1])
data <- read_html(url_download) %>%
  html_text()
# data

load('.\\db_data\\images_raw_data.RData')
new_images_raw_data <- tibble::tibble(image = new_images[1], raw_data = data)
images_raw_data <- bind_rows(images_raw_data, new_images_raw_data)
save(images_raw_data, file = '.\\db_data\\images_raw_data.RData')

## complete data about image
complete_image_data <- tibble::tibble(id = as.character(tasks2$id[1]),
                                      image_name = new_images[1],
                                      image_data = data)
save(complete_image_data, file = paste0('.\\db_data\\complete_image_data', as.character(tasks2$id[1]),
                                        '.RData'))

data2 <- str_split(data, '\\r\\n')
# data2

data_out <- tibble::tibble(timestamp = '',
                           item = '',
                           check = '',
                           amount = '')
data2 <- unlist(data2) %>% toupper() %>% str_trim(., side = 'right')

for (i in seq_along(data2)) {
  data_out[i, ]$item <- str_extract(data2[i], '^[A-Z].*[A-Z]')
  data_out[i, ]$check <- str_extract(data2[i], '[0-9]+[%]')
  data_out[i, ]$amount <- str_extract(data2[i], '[0-9]+[,|.|/][0-9]+')
}
# data_out %>% as.data.frame()

test_data <- data_out %>%
  filter(!is.na(item), !is.na(check), !is.na(amount))

# ## move things around when item, check and amount are on different lines in data
# rearrange_data <- data_out %>%
#   filter(!is.na(item) | !is.na(check) | !is.na(amount))
#
# nof_check <- rearrange_data %>% filter(!is.na(check)) %>% nrow()
# nof_item_check <- rearrange_data %>% filter(!is.na(item), !is.na(check)) %>% nrow()
# nof_check_amount <- rearrange_data %>% filter(!is.na(check), !is.na(amount)) %>% nrow()
# nof_item_check_amount <- test_data %>% nrow()
#
# ## when amount
# if (nof_item_check != nof_check_amount) {
#   rearrange_data %>% filter(!is.na(item), !is.na(check))
#   a <- which(!is.na(rearrange_data$item))
#   b <- which(!is.na(rearrange_data$check))
#   c <- which(!is.na(rearrange_data$amount))
#
#   ## case 1: when amount are on different row than item/check
#   d <- intersect(intersect(a,b), c)
#   e <- intersect(a,b)
#
#   ## case 2: when item are on different row than amount/check
#   d <- intersect(intersect(c,b), a)
#   e <- intersect(c,b)
#
#   check_rows <- c(setdiff(d, e), setdiff(e, d))
#   check_rows2 <- rearrange_data[(check_rows-1):(check_rows+1),]
#
#   ## case 1:
#   colapse_rows <- check_rows2 %>% filter((is.na(check) & is.na(item) & !is.na(amount)) |
#                                            (!is.na(check) & !is.na(item) & is.na(amount)))
#   ## case 2:
#   colapse_rows <- check_rows2 %>% filter((is.na(check) & !is.na(item) & is.na(amount)) |
#                                            (!is.na(check) & is.na(item) & !is.na(amount)))
#
#   new_row <- colapse_rows %>%
#     group_by(timestamp) %>%
#     summarise(item = item[!is.na(item)],
#               check = check[!is.na(check)],
#               amount = amount[!is.na(amount)])
# }

remove_words <- c('LØSVEKT', 'STYKK|STK', 'SKORPEFRI', 'FJL')

if (nrow(test_data) == 0) {
  for (i in seq_along(data2)) {
    data_out[i, ]$item <- str_extract(data2[i], '[A-Z].*[A-Z]')
    data_out[i, ]$check <- str_extract(data2[i], '[0-9]+[%]')
    data_out[i, ]$amount <- str_extract(data2[i], '[0-9]+[,|.|/][0-9]+$')
  }
  # data_out %>% as.data.frame()

  data_out2 <- data_out %>%
    filter(!is.na(item), !is.na(amount)) %>%
    mutate(id = new_tasks_ids$id %>% as.character()) %>%
    select(id, timestamp, item, amount) %>%
    mutate(item = stringr::str_replace(item, '[,]|[.]', ''),
           item = stringr::str_replace_all(item, '0', 'O'),
           item = stringr::str_replace(item, '[1-9]+[a-zA-Z]*', ''),
           item = stringr::str_replace(item, '[%]', ''),
           item = stringr::str_replace(item, '[ ][0-9]+[ ]', ' '),
           item = stringr::str_replace(item, paste0(remove_words, collapse = '|'), ''),
           item = stringr::str_trim(item),
           item = stringr::str_replace_all(item, '[ ]{2,}', ' '),
           amount = stringr::str_replace(amount, ',', '.') %>% as.numeric()) %>%
    filter(item != 'BANK', item != 'SUBTOTAL', item != 'X KR')
} else {
  data_out2 <- data_out %>%
    filter(!is.na(item), !is.na(check), !is.na(amount)) %>%
    # bind_rows(new_row) %>%
    mutate(id = new_tasks_ids$id %>% as.character()) %>%
    select(id, timestamp, item, amount) %>%
    mutate(item = stringr::str_replace(item, '[,]|[.]', ''),
           item = stringr::str_replace_all(item, '0', 'O'),
           item = stringr::str_replace(item, '[1-9]+[a-zA-Z]*', ''),
           item = stringr::str_replace(item, '[%]', ''),
           item = stringr::str_replace(item, '[ ][0-9]+[ ]', ' '),
           item = stringr::str_replace(item, paste0(remove_words, collapse = '|'), ''),
           item = stringr::str_trim(item),
           item = stringr::str_replace_all(item, '[ ]{2,}', ' '),
           amount = stringr::str_replace(amount, '[,]|[/]', '.') %>% as.numeric()) %>%
    filter(item != 'BANK', item != 'SUBTOTAL')
}

time_pattern <- '[0-9]{1,2}[.|,| |-][0-9| ]{1,2}[.|,| |-][0-9]{1,4} [0-9]{1,2}[:][0-9]{1,2}'
timestamp <- data2[str_detect(data2, time_pattern)] %>%
  str_extract(., time_pattern) %>%
  lubridate::dmy_hm() %>%
  as.character()

data_out2$timestamp <- timestamp
data_out2

## continues in search_script.R
