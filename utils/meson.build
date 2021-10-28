# Supress all warnings on util projects
util_args = []
if fc_id == 'gcc' or fc_id == 'intel'
  util_args += '-w'
elif fc_id == 'intel-cl'
  util_args += '/w'
endif 

subdir('mf5to6')
subdir('zonebudget')
