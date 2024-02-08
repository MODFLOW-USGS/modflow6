# definition files
foreach l : fs.read(root_path / 'utils' / 'idmloader' / 'dfns.txt').split('\n')
  l = l.strip()
  # skip comments and blank lines
  if l.startswith('#') or not l.endswith('.dfn')
    continue  
  endif
  # simulation files
  args = [
    py,
    root_path / 'utils' / 'idmloader' / 'scripts' / 'dfn2f90.py',
  ]
  if l.startswith('sim')
    args += ['-d', dfn_path / l, '-o', idm_path,]
    out = l.replace('.dfn', 'idm.f90')
    configure_file(command: args, input: dfn_path / l, output: out)
    modflow_sources += files(out)
  elif l.startswith('exg')
    # exchange files
    foreach e : exchanges
      if l.contains(e.replace('-', ''))
        args += ['-d', dfn_path / l, '-o', idm_path,]
        out = l.replace('.dfn', 'idm.f90')
        configure_file(command: args, input: dfn_path / l, output: out)
        modflow_sources += files(out)
      endif
    endforeach
  else
    # model files
    foreach m : models
      if l.startswith(m)
        args += ['-d', dfn_path / l, '-o', idm_path,]
        out = l.replace('.dfn', 'idm.f90')
        configure_file(command: args, input: dfn_path / l, output: out)
        modflow_sources += files(out)
      endif
    endforeach
  endif
endforeach

# model package input file
input = 'IdmModelPackageInput.fypp'
output = 'IdmModelPackageInput.f90'
configure_file(
  command: [
    fypp, idm_path / input, idm_path / output,
    '-DMODELS=' + (models.length() > 0 ? ('["' + '", "'.join(models) + '"]') : '[]'),
  ],
  input: input,
  output: output
)
modflow_sources += files(output)

# selector file
packages = []
foreach l : fs.read(root_path / 'utils' / 'idmloader' / 'dfns.txt').split('\n')
  foreach m : models
    if l.startswith(m)
      packages += l.replace('.dfn', '')  # track pkgs for selector
    endif
  endforeach
endforeach
input = 'IdmDfnSelector.fypp'
output = 'IdmDfnSelector.f90'
configure_file(
  command: [
    fypp, idm_path / input, idm_path / output,
    '-DMODELS=' + (models.length() > 0 ? ('["' + '", "'.join(models) + '"]') : '[]'),
    '-DEXCHANGES=' + (exchanges.length() > 0 ? ('["' + '", "'.join(exchanges) + '"]') : '[]'),
    '-DPACKAGES=' + (packages.length() > 0 ? ('["' + '", "'.join(packages) + '"]') : '[]')
  ],
  input: input,
  output: output
)
modflow_sources += files(output)