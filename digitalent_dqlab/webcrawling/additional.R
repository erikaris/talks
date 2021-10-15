## additional example to explain function

squirrels <- tibble::tribble(
  ~group,    ~name,     ~role,     ~n_squirrels,
  1,      "Sam",    "Observer",   NA,
  1,     "Mara", "Scorekeeper",    8,
  1,    "Jesse",    "Observer",   NA,
  1,      "Tom",    "Observer",   NA,
  2,     "Mike",    "Observer",   NA,
  2,  "Rachael",    "Observer",   NA,
  2,  "Sydekea", "Scorekeeper",   14,
  2, "Gabriela",    "Observer",   NA,
  3,  "Derrick",    "Observer",   NA,
  3,     "Kara", "Scorekeeper",    9,
  3,    "Emily",    "Observer",   NA,
  3, "Danielle",    "Observer",   NA
)

squirrels %>% fill(n_squirrels, .direction = "up")


######### mutate vs transmute ##############

# mutate() vs transmute --------------------------
# mutate() keeps all existing variables
mtcars %>%
  mutate(displ_l = disp / 61.0237)

# transmute keeps only the variables you create
mtcars %>%
  transmute(displ_l = disp / 61.0237)


### function
# function_name <- function(arg_1, arg_2, ...) {
#   Function body
#   return value
# }

### example of function
### menghitung volume balok

p1 <-  3
l1 <- 5
t1 <- 9

vol1 <- p1*l1*t1
vol1

p2 <-  6
l2 <- 8
t2 <- 7

vol2 <- p2*l2*t2
vol2

vol <- function(p, l, t) {
  volume <- p*l*t
  return(volume)
}

vol(4, 2, 15)

get_holidays <- function(year) {
  ## validation script

  ## preprocessing script
}

#==== Piping vs Non Piping ===============

# Tanpa piping, data (yang biasanya berasal dari step sebelumnya) selalu menjadi parameter pertama

str_html <- read_html("https://news.detik.com/indeks")
html_nodes <- html_nodes(str_html, ".pagination__item:nth-last-child(2)")
last_page <- html_text(html_nodes)
last_page

# Dengan piping, data (yang biasanya berasal dari step sebelumnya), yang menjadi parameter pertama, tidak perlu dituliskan
last_page <- read_html("https://news.detik.com/indeks") %>%
  html_nodes(".pagination__item:nth-last-child(2)") %>%
  html_text()
last_page

#===================================
# what if I want to scrape the 'Referensi' of Libur Nasional?
read_html(libur2019_url) %>%
  html_nodes(".libnas-ref") %>%
  html_text()

# kompas terpopuler
news_title <- read_html("https://www.kompas.com/") %>%
                # html_nodes(".most__wrap .most__list .most__link  .most__title") %>%
                html_nodes(".most__title") %>%
                html_text() %>%
                as_tibble()

#========= pagination
# get the total number of pages
last_page <- read_html("https://news.detik.com/indeks") %>%
  html_nodes(".pagination__item:nth-last-child(2)") %>%
  html_text()
last_page

# now scrape the titles
titles <- c()
for(i in 1:last_page) {
  str_html <- read_html(paste0("https://news.detik.com/indeks/", i))
  p_titles <- str_html %>%
    # html_nodes(".list-content__item .media__title a") %>%
    html_nodes(".media__title a") %>%
    html_text()

  titles <- c(titles, p_titles)
}
titles_df <- as.data.frame(titles)
view(titles_df)
