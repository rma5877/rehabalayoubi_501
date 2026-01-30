###############################################################################
# Web Scraping + Google Scholar Tutorial (10 Penn State Faculty: Bellisario + SoDA)
# Date: Sys.Date()
#
# This script:
#   1) Scrapes PSU profile pages for title/email + interests/expertise/research interests
#   2) Pulls Google Scholar citation histories (citations per year)
#   3) Plots citations over time (facet plot, readable with 10 people)
#   4) Visualizes overlap in scraped interests (shared topics)
#   5) Computes median citations per year (observed years only)
#
# Teaching note:
# - Hard-coded sequential workflow
# - No user-defined functions
# - No if/else
###############################################################################

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
# install.packages(c("rvest", "dplyr", "ggplot2", "scholar", "stringr", "tibble", "tidyr"))
library(rvest)
library(dplyr)
library(ggplot2)
library(scholar)
library(stringr)
library(tibble)
library(tidyr)

# -----------------------------------------------------------------------------
# Part 1: Hard-code 10 Penn State faculty (Bellisario + SoDA)
# -----------------------------------------------------------------------------
shen_name <- "Fuyuan Shen"
shen_dept <- "Advertising/Public Relations (Bellisario)"
shen_url  <- "https://bellisario.psu.edu/people/fuyuan-shen"

skurka_name <- "Chris Skurka"
skurka_dept <- "Film Production & Media Studies (Bellisario / IEE)"
skurka_url  <- "https://iee.psu.edu/people/chris-skurka"

shoenberger_name <- "Heather Shoenberger"
shoenberger_dept <- "Advertising/Public Relations (Bellisario)"
shoenberger_url  <- "https://bellisario.psu.edu/people/heather-shoenberger"

cameron_name <- "Daryl Cameron"
cameron_dept <- "Psychology (College of the Liberal Arts)"
cameron_url  <- "https://psych.la.psu.edu/people/cdc49/"

bortree_name <- "Denise Bortree"
bortree_dept <- "Advertising/Public Relations (Bellisario)"
bortree_url  <- "https://bellisario.psu.edu/people/denise-bortree"

oliver_name <- "Mary Beth Oliver"
oliver_dept <- "Film Production & Media Studies (Bellisario)"
oliver_url  <- "https://bellisario.psu.edu/people/mary-beth-oliver"

sundar_name <- "S. Shyam Sundar"
sundar_dept <- "Film Production & Media Studies (Bellisario)"
sundar_url  <- "https://bellisario.psu.edu/people/s-shyam-sundar"

gil_name <- "Homero Gil de Zúñiga"
gil_dept <- "Journalism & Media Studies (Bellisario)"
gil_url  <- "https://bellisario.psu.edu/people/homero-gil-de-zuniga"

mcallister_name <- "Matthew McAllister"
mcallister_dept <- "Film Production & Media Studies (Bellisario)"
mcallister_url  <- "https://bellisario.psu.edu/people/matthew-mcallister"

overton_name <- "Holly Overton"
overton_dept <- "Advertising/Public Relations (Bellisario)"
overton_url  <- "https://bellisario.psu.edu/people/holly-overton"

# -----------------------------------------------------------------------------
# Part 2: Scrape each PSU profile (title/email + interests)
# -----------------------------------------------------------------------------
# NOTE:
# PSU profile pages vary. We try to capture common sections like:
# - "Areas of Interest"
# - "Areas of Expertise"
# - "Research Interests"
# - "Interests"
#
# We also pull full page text (for regex extraction of title/email).

# -----------------------------------------------------------------------------
# Step 1: Scrape Fuyuan Shen
# -----------------------------------------------------------------------------
shen_page <- read_html(shen_url)

shen_text <- shen_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

shen_title <- str_extract(
  shen_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

shen_email <- str_extract(shen_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

shen_interests_nodes <- shen_page %>%
  html_nodes(xpath = paste0(
    # Areas of Interest / Expertise lists
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    # Research Interests (either ul/li or next block)
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    # Interests (generic)
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

shen_interests_raw <- shen_interests_nodes %>%
  html_text(trim = TRUE)

shen_interests <- paste(shen_interests_raw, collapse = "; ")
shen_n_interest_items <- length(shen_interests_raw)

shen_row <- tibble(
  name = shen_name,
  department = shen_dept,
  url = shen_url,
  scraped_title = shen_title,
  scraped_email = shen_email,
  scraped_interests = shen_interests,
  n_interest_items = shen_n_interest_items
)

# -----------------------------------------------------------------------------
# Step 2: Scrape Chris Skurka
# -----------------------------------------------------------------------------
skurka_page <- read_html(skurka_url)

skurka_text <- skurka_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

skurka_title <- str_extract(
  skurka_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

skurka_email <- str_extract(skurka_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

skurka_interests_nodes <- skurka_page %>%
  html_nodes(xpath = paste0(
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

skurka_interests_raw <- skurka_interests_nodes %>%
  html_text(trim = TRUE)

skurka_interests <- paste(skurka_interests_raw, collapse = "; ")
skurka_n_interest_items <- length(skurka_interests_raw)

skurka_row <- tibble(
  name = skurka_name,
  department = skurka_dept,
  url = skurka_url,
  scraped_title = skurka_title,
  scraped_email = skurka_email,
  scraped_interests = skurka_interests,
  n_interest_items = skurka_n_interest_items
)

# -----------------------------------------------------------------------------
# Step 3: Scrape Heather Shoenberger
# -----------------------------------------------------------------------------
shoenberger_page <- read_html(shoenberger_url)

shoenberger_text <- shoenberger_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

shoenberger_title <- str_extract(
  shoenberger_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

shoenberger_email <- str_extract(shoenberger_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

shoenberger_interests_nodes <- shoenberger_page %>%
  html_nodes(xpath = paste0(
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

shoenberger_interests_raw <- shoenberger_interests_nodes %>%
  html_text(trim = TRUE)

shoenberger_interests <- paste(shoenberger_interests_raw, collapse = "; ")
shoenberger_n_interest_items <- length(shoenberger_interests_raw)

shoenberger_row <- tibble(
  name = shoenberger_name,
  department = shoenberger_dept,
  url = shoenberger_url,
  scraped_title = shoenberger_title,
  scraped_email = shoenberger_email,
  scraped_interests = shoenberger_interests,
  n_interest_items = shoenberger_n_interest_items
)

# -----------------------------------------------------------------------------
# Step 4: Scrape Daryl Cameron
# -----------------------------------------------------------------------------
cameron_page <- read_html(cameron_url)

cameron_text <- cameron_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

cameron_title <- str_extract(
  cameron_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

cameron_email <- str_extract(cameron_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

cameron_interests_nodes <- cameron_page %>%
  html_nodes(xpath = paste0(
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

cameron_interests_raw <- cameron_interests_nodes %>%
  html_text(trim = TRUE)

cameron_interests <- paste(cameron_interests_raw, collapse = "; ")
cameron_n_interest_items <- length(cameron_interests_raw)

cameron_row <- tibble(
  name = cameron_name,
  department = cameron_dept,
  url = cameron_url,
  scraped_title = cameron_title,
  scraped_email = cameron_email,
  scraped_interests = cameron_interests,
  n_interest_items = cameron_n_interest_items
)

# -----------------------------------------------------------------------------
# Step 5: Scrape Denise Bortree
# -----------------------------------------------------------------------------
bortree_page <- read_html(bortree_url)

bortree_text <- bortree_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

bortree_title <- str_extract(
  bortree_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

bortree_email <- str_extract(bortree_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

bortree_interests_nodes <- bortree_page %>%
  html_nodes(xpath = paste0(
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

bortree_interests_raw <- bortree_interests_nodes %>%
  html_text(trim = TRUE)

bortree_interests <- paste(bortree_interests_raw, collapse = "; ")
bortree_n_interest_items <- length(bortree_interests_raw)

bortree_row <- tibble(
  name = bortree_name,
  department = bortree_dept,
  url = bortree_url,
  scraped_title = bortree_title,
  scraped_email = bortree_email,
  scraped_interests = bortree_interests,
  n_interest_items = bortree_n_interest_items
)

# -----------------------------------------------------------------------------
# Step 6: Scrape Mary Beth Oliver
# -----------------------------------------------------------------------------
oliver_page <- read_html(oliver_url)

oliver_text <- oliver_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

oliver_title <- str_extract(
  oliver_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

oliver_email <- str_extract(oliver_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

oliver_interests_nodes <- oliver_page %>%
  html_nodes(xpath = paste0(
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

oliver_interests_raw <- oliver_interests_nodes %>%
  html_text(trim = TRUE)

oliver_interests <- paste(oliver_interests_raw, collapse = "; ")
oliver_n_interest_items <- length(oliver_interests_raw)

oliver_row <- tibble(
  name = oliver_name,
  department = oliver_dept,
  url = oliver_url,
  scraped_title = oliver_title,
  scraped_email = oliver_email,
  scraped_interests = oliver_interests,
  n_interest_items = oliver_n_interest_items
)

# -----------------------------------------------------------------------------
# Step 7: Scrape S. Shyam Sundar
# -----------------------------------------------------------------------------
sundar_page <- read_html(sundar_url)

sundar_text <- sundar_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

sundar_title <- str_extract(
  sundar_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

sundar_email <- str_extract(sundar_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

sundar_interests_nodes <- sundar_page %>%
  html_nodes(xpath = paste0(
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

sundar_interests_raw <- sundar_interests_nodes %>%
  html_text(trim = TRUE)

sundar_interests <- paste(sundar_interests_raw, collapse = "; ")
sundar_n_interest_items <- length(sundar_interests_raw)

sundar_row <- tibble(
  name = sundar_name,
  department = sundar_dept,
  url = sundar_url,
  scraped_title = sundar_title,
  scraped_email = sundar_email,
  scraped_interests = sundar_interests,
  n_interest_items = sundar_n_interest_items
)

# -----------------------------------------------------------------------------
# Step 8: Scrape Homero Gil de Zúñiga
# -----------------------------------------------------------------------------
gil_page <- read_html(gil_url)

gil_text <- gil_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

gil_title <- str_extract(
  gil_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

gil_email <- str_extract(gil_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

gil_interests_nodes <- gil_page %>%
  html_nodes(xpath = paste0(
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

gil_interests_raw <- gil_interests_nodes %>%
  html_text(trim = TRUE)

gil_interests <- paste(gil_interests_raw, collapse = "; ")
gil_n_interest_items <- length(gil_interests_raw)

gil_row <- tibble(
  name = gil_name,
  department = gil_dept,
  url = gil_url,
  scraped_title = gil_title,
  scraped_email = gil_email,
  scraped_interests = gil_interests,
  n_interest_items = gil_n_interest_items
)

# -----------------------------------------------------------------------------
# Step 9: Scrape Matthew McAllister
# -----------------------------------------------------------------------------
mcallister_page <- read_html(mcallister_url)

mcallister_text <- mcallister_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

mcallister_title <- str_extract(
  mcallister_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

mcallister_email <- str_extract(mcallister_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

mcallister_interests_nodes <- mcallister_page %>%
  html_nodes(xpath = paste0(
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

mcallister_interests_raw <- mcallister_interests_nodes %>%
  html_text(trim = TRUE)

mcallister_interests <- paste(mcallister_interests_raw, collapse = "; ")
mcallister_n_interest_items <- length(mcallister_interests_raw)

mcallister_row <- tibble(
  name = mcallister_name,
  department = mcallister_dept,
  url = mcallister_url,
  scraped_title = mcallister_title,
  scraped_email = mcallister_email,
  scraped_interests = mcallister_interests,
  n_interest_items = mcallister_n_interest_items
)

# -----------------------------------------------------------------------------
# Step 10: Scrape Holly Overton
# -----------------------------------------------------------------------------
overton_page <- read_html(overton_url)

overton_text <- overton_page %>%
  html_node("body") %>%
  html_text(trim = TRUE)

overton_title <- str_extract(
  overton_text,
  "(Distinguished|Liberal Arts|Roy C\\.|Arnold S\\.|James P\\.)?\\s*(Associate\\s+)?Professor[^\\n\\r]{0,120}"
)

overton_email <- str_extract(overton_text, "[A-Za-z0-9._%+-]+@psu\\.edu")

overton_interests_nodes <- overton_page %>%
  html_nodes(xpath = paste0(
    "//h2[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Areas of')]/following-sibling::ul[1]/li",
    " | //h2[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Areas of Expertise']/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::ul[1]/li",
    " | //h2[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h3[contains(normalize-space(),'Research') and contains(normalize-space(),'Interest')]/following-sibling::*[1]",
    " | //h2[normalize-space()='Interests']/following-sibling::ul[1]/li",
    " | //h3[normalize-space()='Interests']/following-sibling::ul[1]/li"
  ))

overton_interests_raw <- overton_interests_nodes %>%
  html_text(trim = TRUE)

overton_interests <- paste(overton_interests_raw, collapse = "; ")
overton_n_interest_items <- length(overton_interests_raw)

overton_row <- tibble(
  name = overton_name,
  department = overton_dept,
  url = overton_url,
  scraped_title = overton_title,
  scraped_email = overton_email,
  scraped_interests = overton_interests,
  n_interest_items = overton_n_interest_items
)

# -----------------------------------------------------------------------------
# Combine scraped profiles + quick inspection
# -----------------------------------------------------------------------------
scraped_profiles <- bind_rows(
  shen_row, skurka_row, shoenberger_row, cameron_row, bortree_row,
  oliver_row, sundar_row, gil_row, mcallister_row, overton_row
)

print(scraped_profiles)

ggplot(scraped_profiles, aes(x = reorder(name, n_interest_items), y = n_interest_items)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Interest Items Captured from PSU Profile Pages",
    x = "Faculty member",
    y = "Number of interest items captured"
  )

# -----------------------------------------------------------------------------
# Part 3: Google Scholar IDs (YOU MUST FILL THESE IN)
# -----------------------------------------------------------------------------
# NOTE:
# Scholar IDs look like "yPbxmSwAAAAJ".
# Copy from the person’s Google Scholar profile URL:
# https://scholar.google.com/citations?user=THIS_PART_HERE&hl=en

shen_scholar_id        <- "b5gwU-wAAAAJ"     # Fuyuan Shen
skurka_scholar_id      <- "65Cg6DMAAAAJ"     # Chris Skurka
shoenberger_scholar_id <- "fLD0R7QAAAAJ"     # Heather Shoenberger
cameron_scholar_id     <- "NxYURHQAAAAJ"     # Daryl Cameron
bortree_scholar_id     <- "HmJ8VFoAAAAJ"     # Denise Bortree
oliver_scholar_id      <- "MGIJ8gMAAAAJ"     # Mary Beth Oliver
sundar_scholar_id      <- "KP-DwH0AAAAJ"     # S. Shyam Sundar
gil_scholar_id         <- "T3VspYkAAAAJ"     # Homero Gil de Zúñiga
mcallister_scholar_id  <- "LLQKdMcAAAAJ"     # Matthew McAllister
overton_scholar_id     <- "eKTly7IAAAAJ"     # Holly Overton


# -----------------------------------------------------------------------------
# Pull citation history (citations per year), sequentially
# -----------------------------------------------------------------------------
shen_ct <- get_citation_history(shen_scholar_id) %>% mutate(name = shen_name)
skurka_ct <- get_citation_history(skurka_scholar_id) %>% mutate(name = skurka_name)
shoenberger_ct <- get_citation_history(shoenberger_scholar_id) %>% mutate(name = shoenberger_name)
cameron_ct <- get_citation_history(cameron_scholar_id) %>% mutate(name = cameron_name)
bortree_ct <- get_citation_history(bortree_scholar_id) %>% mutate(name = bortree_name)
oliver_ct <- get_citation_history(oliver_scholar_id) %>% mutate(name = oliver_name)
sundar_ct <- get_citation_history(sundar_scholar_id) %>% mutate(name = sundar_name)
gil_ct <- get_citation_history(gil_scholar_id) %>% mutate(name = gil_name)
mcallister_ct <- get_citation_history(mcallister_scholar_id) %>% mutate(name = mcallister_name)
overton_ct <- get_citation_history(overton_scholar_id) %>% mutate(name = overton_name)

citation_df <- bind_rows(
  shen_ct, skurka_ct, shoenberger_ct, cameron_ct, bortree_ct,
  oliver_ct, sundar_ct, gil_ct, mcallister_ct, overton_ct
)

print(head(citation_df, 15))

# -----------------------------------------------------------------------------
# Q3: Plot citations over time (facet, readable for 10 people)
# -----------------------------------------------------------------------------
ggplot(citation_df, aes(x = year, y = cites)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~name, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Google Scholar Citations Over Time (10 Penn State Faculty)",
    x = "Year",
    y = "Citations per Year"
  )

# -----------------------------------------------------------------------------
# Q4: Overlap in scraped interests (shared-topic count + plot)
# -----------------------------------------------------------------------------
interest_long <- scraped_profiles %>%
  mutate(scraped_interests = str_replace_all(scraped_interests, "\\s+", " ")) %>%
  separate_rows(scraped_interests, sep = ";") %>%
  mutate(scraped_interests = str_trim(scraped_interests)) %>%
  filter(scraped_interests != "")

overlap <- interest_long %>%
  count(scraped_interests, sort = TRUE) %>%
  filter(n > 1)

print(overlap)

ggplot(overlap, aes(x = reorder(scraped_interests, n), y = n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Overlapping Research Interests Across Faculty",
    x = "Interest / expertise label (from PSU profiles)",
    y = "Number of faculty sharing it"
  )

# -----------------------------------------------------------------------------
# Q5: Median citation count per year (observed years only)
# -----------------------------------------------------------------------------
median_cites <- citation_df %>%
  group_by(name) %>%
  summarize(median_cites = median(cites, na.rm = TRUE), .groups = "drop")

print(median_cites)

# -----------------------------------------------------------------------------
# Google Scholar IDs
# -----------------------------------------------------------------------------
shen_scholar_id        <- "b5gwU-wAAAAJ"     # Fuyuan Shen
skurka_scholar_id      <- "65Cg6DMAAAAJ"     # Chris Skurka
shoenberger_scholar_id <- "fLD0R7QAAAAJ"     # Heather Shoenberger
cameron_scholar_id     <- "NxYURHQAAAAJ"     # Daryl Cameron
bortree_scholar_id     <- "HmJ8VFoAAAAJ"     # Denise Bortree
oliver_scholar_id      <- "MGIJ8gMAAAAJ"     # Mary Beth Oliver
sundar_scholar_id      <- "KP-DwH0AAAAJ"     # S. Shyam Sundar
gil_scholar_id         <- "T3VspYkAAAAJ"     # Homero Gil de Zúñiga
mcallister_scholar_id  <- "LLQKdMcAAAAJ"     # Matthew McAllister
overton_scholar_id     <- "eKTly7IAAAAJ"     # Holly Overton

# Names (must already exist; re-define here if needed)
shen_name <- "Fuyuan Shen"
skurka_name <- "Chris Skurka"
shoenberger_name <- "Heather Shoenberger"
cameron_name <- "Daryl Cameron"
bortree_name <- "Denise Bortree"
oliver_name <- "Mary Beth Oliver"
sundar_name <- "S. Shyam Sundar"
gil_name <- "Homero Gil de Zúñiga"
mcallister_name <- "Matthew McAllister"
overton_name <- "Holly Overton"

# -----------------------------------------------------------------------------
# Pull profile summaries (sequential, no functions)
# -----------------------------------------------------------------------------
shen_profile        <- get_profile(shen_scholar_id)
skurka_profile      <- get_profile(skurka_scholar_id)
shoenberger_profile <- get_profile(shoenberger_scholar_id)
cameron_profile     <- get_profile(cameron_scholar_id)
bortree_profile     <- get_profile(bortree_scholar_id)
oliver_profile      <- get_profile(oliver_scholar_id)
sundar_profile      <- get_profile(sundar_scholar_id)
gil_profile         <- get_profile(gil_scholar_id)
mcallister_profile  <- get_profile(mcallister_scholar_id)
overton_profile     <- get_profile(overton_scholar_id)

# -----------------------------------------------------------------------------
# Build comparison table
# (get_profile() returns fields like: total_cites, h_index, i10_index, etc.)
# -----------------------------------------------------------------------------
profile_df <- bind_rows(
  tibble(name = shen_name,        total_cites = shen_profile$total_cites,        h_index = shen_profile$h_index),
  tibble(name = skurka_name,      total_cites = skurka_profile$total_cites,      h_index = skurka_profile$h_index),
  tibble(name = shoenberger_name, total_cites = shoenberger_profile$total_cites, h_index = shoenberger_profile$h_index),
  tibble(name = cameron_name,     total_cites = cameron_profile$total_cites,     h_index = cameron_profile$h_index),
  tibble(name = bortree_name,     total_cites = bortree_profile$total_cites,     h_index = bortree_profile$h_index),
  tibble(name = oliver_name,      total_cites = oliver_profile$total_cites,      h_index = oliver_profile$h_index),
  tibble(name = sundar_name,      total_cites = sundar_profile$total_cites,      h_index = sundar_profile$h_index),
  tibble(name = gil_name,         total_cites = gil_profile$total_cites,         h_index = gil_profile$h_index),
  tibble(name = mcallister_name,  total_cites = mcallister_profile$total_cites,  h_index = mcallister_profile$h_index),
  tibble(name = overton_name,     total_cites = overton_profile$total_cites,     h_index = overton_profile$h_index)
)

# Print clean table (sorted by total citations)
profile_df_sorted <- profile_df %>% arrange(desc(total_cites))
print(profile_df_sorted)

# -----------------------------------------------------------------------------
# Bar chart: Total citations (ordered)
# -----------------------------------------------------------------------------
ggplot(profile_df_sorted, aes(x = reorder(name, total_cites), y = total_cites)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Total Google Scholar Citations by Faculty",
    x = "Faculty member",
    y = "Total citations"
  )

# -----------------------------------------------------------------------------
# Optional: Bar chart for h-index
# -----------------------------------------------------------------------------
ggplot(profile_df %>% arrange(desc(h_index)),
       aes(x = reorder(name, h_index), y = h_index)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Google Scholar h-index by Faculty",
    x = "Faculty member",
    y = "h-index"
  )



