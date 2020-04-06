plan <- drake_plan(
  ski_tours = extract_all_ski_tours(),
  peaks = get_peaks("./data/peaks.rds")
)

# TODO include robotstxt to check if scraping is allowed



