#! /bin/bash

#SEARCHPATHS=(src srcbmi utils)
SEARCHPATHS=(src srcbmi)
EXCLUDEDIRS=(src/Utilities/Libraries/blas # external library blas
             src/Utilities/Libraries/daglib # external library dag
             src/Utilities/Libraries/rcm # external library rcm
             src/Utilities/Libraries/sparsekit # external library sparsekit
             src/Utilities/Libraries/sparskit2) # external library sparsekit2
EXCLUDEFILES=(src/Utilities/InputOutput.f90) # excluded until refactored 

fformatfails=()
checkcount=0

for path in "${SEARCHPATHS[@]}"; do
  readarray -d '' files < <(find "${path}" -type f -print0 | grep -z '\.[fF]9[0,5]$')
  for file in "${files[@]}"; do
    exclude=0

    for d in "${EXCLUDEDIRS[@]}"; do
      [[ "${d}" == $(dirname "${file}") ]] && exclude=1 && break; done
    if [[ ${exclude} == 1 ]]; then continue; fi

    for f in "${EXCLUDEFILES[@]}"; do
      [[ "${f}" == "${file}" ]] && exclude=1 && break; done
    if [[ ${exclude} == 1 ]]; then continue; fi

    ((checkcount++))

    if [[ ! -z $(fprettify -d -c ./distribution/.fprettify.yaml "${file}" 2>&1) ]]; then
      fformatfails+=("${file}")
    fi
  done
done

echo -e "\nFortran source files checked: ${checkcount}"
echo -e "Fortran source files failed: ${#fformatfails[@]}\n"

if [[ ${#fformatfails[@]} > 0 ]]; then
  for f in "${fformatfails[@]}"; do echo "${f}"; done

  echo -e "\nTo verify file format diff and/or warn in local environment run:"
  echo -e " 'fprettify -d -c <path to modflow6>/distribution/.fprettify.yaml <filepath>'\n\n"

  exit 1
fi

exit 0
