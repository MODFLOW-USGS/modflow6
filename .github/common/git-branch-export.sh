#!/bin/bash

echo GITHUB_REF branch name ${GITHUB_REF##*/}
echo GITHUB_REF=${GITHUB_REF}
echo GITHUB_HEAD_REF=${GITHUB_HEAD_REF}
echo GITHUB_BASE_REF=${GITHUB_BASE_REF}
if [ -z ${GITHUB_BASE_REF+x} ]; then
  export BRANCH=${GITHUB_REF##*/};
else export BRANCH=${GITHUB_HEAD_REF}; fi
echo BRANCH=${BRANCH}
