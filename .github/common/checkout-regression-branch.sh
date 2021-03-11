#!/bin/bash

cd ../modflow6-testmodels
if git show-ref -q --heads ${BRANCH}; then
  git checkout ${BRANCH}; echo switched to modflow6-testmodels branch ${BRANCH};
else echo using modflow6-testmodels branch master;  fi
git branch
cd ../modflow6
ls ../
