plan <- drake_plan(
  snezak_html = get_snezak_html(),
  # ski_tours = extract_all_ski_tours(snezak_html),
  peaks = get_peaks("./data/peaks.rds")
)



