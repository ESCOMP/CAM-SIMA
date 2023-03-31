! History file configuration with a single good entry
&hist_config_arrays_nl
  hist_num_inst_fields = 3
  hist_num_avg_fields = 0
  hist_num_min_fields = 0
  hist_num_max_fields = 0
  hist_num_var_fields = 0
/

&hist_file_config_nl
  hist_volume = 'h1'
  hist_inst_fields = 'A','B','C'
  hist_precision = 'REAL32'
  hist_max_frames = 13
  hist_output_frequency = '2*hours'
  hist_filename_spec = '%c.cam.%u.%y-%m-%d-%s.nc'
/
