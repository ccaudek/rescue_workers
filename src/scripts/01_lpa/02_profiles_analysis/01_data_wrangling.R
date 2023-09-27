
#' The purpose of this notebook is to pre-process the data so as to create 
#' the file `rescue_workers_data.csv` that will be used as input for all the 
#' statistical analyses.


suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("MplusAutomation")
  library("brms")
  library("careless")
  library("mice")
  library("psych")
  library("bayesplot")
  library("patchwork")
})

options("max.print" = .Machine$integer.max)

# Make random things reproducible
set.seed(1234)
options(mc.cores = 4)
bayesplot_theme_set()

# Functions
source(here::here("src", "functions", "funs_add_neoffi60_subscales.R"))
source(here::here("src", "functions", "funs_correct_iesr_scores.R"))
source(here::here("src", "functions", "funs_plot_job_qualification.R"))
source(here::here("src", "functions", "funs_generate_all_items_df.R"))

scale_this <- function(x) as.vector(scale(x))

sum_coding <- function(x, lvls = levels(x)) {
  # codes the first category with -1
  nlvls <- length(lvls)
  stopifnot(nlvls > 1)
  cont <- diag(nlvls)[, -nlvls, drop = FALSE]
  cont[nlvls, ] <- -1
  cont <- cont[c(nlvls, 1:(nlvls - 1)), , drop = FALSE]
  colnames(cont) <- lvls[-1]
  x <- factor(x, levels = lvls)
  contrasts(x) <- cont
  x
}


# Get data ---------------------------------------------------------------------

all_items <- generate_all_items_df()


# IESR problem -----------------------------------------------------------------

# The important point is that there is a problem with IES-R, in the control 
# group. I shift the control distribution of IES-R towards lower values.

temp <- correct_iesr_scores(all_items)
all_items <- temp

ggplot(all_items, aes(x = iesr_ts, colour = is_rescue_worker)) +
  geom_density()

all_items |> 
  ggplot(aes(y=iesr_ts, fill=is_rescue_worker)) + 
    geom_boxplot() +
    scale_y_log10()

all_items |> 
  group_by(is_rescue_worker) |> 
  summarize(
    avg_iesr = mean(iesr_ts, na.rm = TRUE)
  )

# There are some cases in which the RWs of Toscana and Lombardia did not 
# have the proper qualification. They are assigned to the category of RWs.
all_items$idx <- 1:nrow(all_items)
all_items$is_rescue_worker <- as.character(all_items$is_rescue_worker)
all_items$is_rescue_worker <- ifelse(
  all_items$idx < 746, "Si", all_items$is_rescue_worker
)
all_items$idx <- NULL

all_items$commeetee_location <- ifelse(
  all_items$is_rescue_worker == "No", "None", all_items$red_cross_commeetee_location
) |> 
  factor()

all_items$commeetee_location <- ifelse(
  all_items$is_rescue_worker == "No" & all_items$age < 25, "students", all_items$commeetee_location
)

all_items$commeetee <- ifelse(
  all_items$is_rescue_worker == "No" & all_items$age >= 25, "community_sample",
  all_items$commeetee_location
)

all_items$commeetee <- 
  ifelse(is.na(all_items$commeetee), "community_sample", all_items$commeetee) |> 
  factor()

sum(is.na(all_items$commeetee))

# all_items <- all_items %>%
#   mutate(job_qualification = case_when(
#     is_rescue_worker=="Si" & job_qualification == "non_rescue_worker" ~ "team_member",
#     TRUE ~ job_qualification)) 


# Save the data in their final form --------------------------------------------

rio::export(
  all_items,
  here::here("data", "final", "rescue_workers_data.csv")
)


# eof ----



