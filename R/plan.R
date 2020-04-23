plan <- drake_plan(
  ski_tours = extract_all_ski_tours(),
  peaks = get_peaks("./data/osm_points.rds"), 
  df = left_join(ski_tours, rename(peaks, peak = 'name'), by = 'peak')
)
