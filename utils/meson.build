# Supress all warnings on util projects
util_args = []
if fc_id == 'gcc' or fc_id == 'intel'
  util_args += '-w'
elif fc_id == 'intel-cl'
  util_args += '/w'
endif 

# MODFLOW 5 to 6 converter - DO NOT BUILD ON CRAY
if not is_cray
  subdir('mf5to6')
endif

# ZONEBUDGET 6
subdir('zonebudget')
