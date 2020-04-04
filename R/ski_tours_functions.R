get_snezak_html <- function() {
  read_html("https://www.snezak.si/razmere/?date=season&startDate=&endDate=&conditionsMin=1&conditionsMax=5&safetyMin=1&safetyMax=5") %>%
    html_nodes(".post-list section")
}

get_attr_of_nodes <- function(html_file, nodes, attr) {
  html_file %>%
    html_nodes(nodes) %>%
    html_attr(attr)
}

get_text_from_nodes <- function(html_file, nodes, trim = FALSE) {
  html_file %>%
    html_nodes(nodes) %>%
    html_text() %>%
    {
      if (trim) str_replace_all(., "[\n\t]", "") else .
    }
}

get_rates <- function(html_node) {
  title <- get_text_from_nodes(html_node, nodes = "span.title")
  rate <- html_nodes(html_node, "img") %>% length()
  set_names(rate, title)
}

extract_ski_tour <- function(ski_tour_section) {
  browser()
  # date & link ----
  link <- get_attr_of_nodes(ski_tour_section, nodes = "a", attr = "href")
  date <- get_attr_of_nodes(ski_tour_section, nodes = "time", attr = "datetime")

  # main ----
  ski_tour_main <- html_nodes(ski_tour_section, css = ".main")

  title <- get_text_from_nodes(ski_tour_main, nodes = "h2")
  author <- get_text_from_nodes(ski_tour_main, nodes = "div.author", trim = TRUE)
  route <- get_text_from_nodes(ski_tour_main, nodes = "div.route", trim = TRUE)

  # rate ----
  ski_tour_rate <- html_nodes(ski_tour_section, css = ".rates div.rate")
  rates <- ski_tour_rate %>%
    map(get_rates) %>%
    unlist()

  ski_condition_rating <- rates["Smučarske razmere"]
  safety_rating <- rates["Občutek varnosti"]

  # joining columns ----
  tibble(date, link, title, author, route, ski_condition_rating, safety_rating)
}

extract_all_ski_tours <- function(ski_tour_section) {
  ski_tour_section %>% 
    map(extract_ski_tour) %>%
    bind_rows() %>%
    # TODO should be done inside extract_ski_tour
    separate(route, c("mountains", "peak", "route"), sep = "\\|", remove = TRUE)
}
