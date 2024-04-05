library(stringr)
library(rvest) 
library(xml2)
library(purrr)

# Extract all divs with following ids

ids <- c("c6496", "c6497", "c6499", "c6500", "c6501", "c6502", "c6503")

program_html <- read_html("program.html") %>% 
  html_nodes("div")
  
slots_html <- program_html[which(html_attr(program_html, "id") %in% ids)]

slots_content <- html_nodes(slots_html,"div")[which(html_attr(html_nodes(slots_html,"div"), "class") == "ce-bodytext")]


# Extract panels 
extract_panels <- function(slot) {

    regex <- "\\b(Di|Mi|Do|Fr)\\b [A-Z] \\d{2} \\| .*?\n\n"
    text <- html_text2(slot)

    panels <- unlist(str_split(text, regex))[-1]
    names(panels) <- str_remove_all(unlist(regmatches(text, gregexpr(regex, text, perl = TRUE))), "\n\n")
    
    
    return(panels)
}

slots <- map(as.list(slots_content), extract_panels)

names(slots) <- str_remove_all(html_text(html_nodes(slots_html, "h2")), "\n\n")


dissect_panel <- function(panel) {
  chair <- str_remove_all(unlist(str_extract_all(panel, "Chair:(.*?)\\n\\n")), "(Chair:\\s|\n\n)")
  discussant <- str_remove_all(unlist(str_extract_all(panel, "Discussant:(.*?)\\n\\n")), "(Discussant:\\s|\n\n)")
  
  talks_zuorndung <- unlist(str_split(substr(panel, nchar(paste0(c(unlist(str_extract_all(panel, "Chair:(.*?)\\n\\n")), unlist(str_extract_all(panel, "Discussant:(.*?)\\n\\n"))), collapse = "")), nchar(panel)), "\n\n"))
  
  section <- str_remove_all(unlist(str_extract_all(str_remove_all(talks_zuorndung[length(talks_zuorndung)-1], "Zuordnung:"), "„.*?“")), "„|“")
  
  talks <- str_split(talks_zuorndung[-c(length(talks_zuorndung), length(talks_zuorndung)-1)], "\n")
  
  talks[[1]] <- talks[[1]][-1]
  
  talks_out <- list()
  
  for (talk in 1:length(talks)) {
    
    presenter <- talks[[talk]][1]
    presenter <- gsub("(,[^,]*),", "\\1;", substr(presenter, 1, nchar(presenter)-1))
    paper <- talks[[talk]][2]
    coauthor <- substr(str_extract(paper, "\\(Ko-Autor\\*in: (.*?)\\)"), 15, nchar(str_extract(paper, "\\(Ko-Autor\\*in: (.*?)\\)"))-1)
    
    title <- substr(paper, 1, nchar(paper) - nchar(paste0("(Ko-Autor*in: ",coauthor, ")")))
    
    talks_out[[paste0("talk_",talk)]] <- list("presenter" = presenter, "title" = title, "coauthor" = coauthor)
    
  }
  
  return(list("chairs" = chair, "discussants" = discussant, "talks" = talks_out, "section" = section))
}
slots_cleaned <- list()

for (i in 1:length(slots)) slots_cleaned[[names(slots)[i]]] <- map(slots[[i]], dissect_panel)

library(dplyr)

panel_to_df <- function(list) {
  talks_list <- list$talks
  
  # Convert talks list to dataframe
  talks_df <- do.call(rbind, lapply(talks_list, data.frame))
  
  # Adding constant columns
  talks_df$chairs <- paste0(list$chairs, collapse = "; ")
  talks_df$discussants <- paste0(list$discussants, collapse = "; ")
  talks_df$section <- paste0(list$section, collapse = "; ")
  
  return(tibble(talks_df))
}

slots_df <- list()
for (i in 1:length(slots_cleaned)) slots_df[[names(slots_cleaned)[i]]] <- map(slots_cleaned[[i]], panel_to_df)

program_df <- bind_rows(map(slots_df, ~rename(bind_rows(.x, .id = "ListName"), panel = ListName)), .id = "ListName") %>% 
  rename(slot = ListName) %>% 
  rowwise() %>% 
  mutate(date = str_extract(unlist(str_split(slot, " \\| "))[2], "\\d{2}.\\d{2}.\\d{4}"),
         time = str_extract(unlist(str_split(slot, " \\| "))[2], "\\d{1,2}:\\d{2}-\\d{2}:\\d{2} Uhr"),
         slot = unlist(str_split(slot, " \\| "))[1],
         panel_title = unlist(str_split(panel, " \\| "))[2],
         panel = str_extract(unlist(str_split(panel, " \\| "))[1], "\\d{2}")) %>% ungroup() %>% 
  select(slot, panel, date:panel_title, chairs:section, title, presenter, coauthor) %>% 
  mutate(presenter = str_split(presenter, "; ")) %>% 
  tidyr::unnest(presenter) %>% 
  rowwise() %>% 
  mutate(presenter_affiliation = unlist(str_split(presenter, ", "))[2],
         presenter = unlist(str_split(presenter, ", "))[1])

write.csv(program_df, "program.csv", row.names = F)

read.csv("program.csv")



