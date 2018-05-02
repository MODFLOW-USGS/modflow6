#!/usr/bin/python

from __future__ import print_function
import subprocess
import os
import sys
import datetime
import json

files = ['version.py', 'version.tex', 'README.md', 'DISCLAIMER.md',
         'code.json']

vpth = 'doc'
prod = 'MODFLOW 6'

approved = '''Disclaimer
----------

This software has been approved for release by the U.S. Geological Survey
(USGS). Although the software has been subjected to rigorous review, the USGS
reserves the right to update the software as needed pursuant to further analysis
and review. No warranty, expressed or implied, is made by the USGS or the U.S.
Government as to the functionality of the software and related material nor
shall the fact of release constitute any such warranty. Furthermore, the
software is released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.
'''

preliminary = '''Disclaimer
----------

This software is preliminary or provisional and is subject to revision. It is
being provided to meet the need for timely best science. The software has not
received final approval by the U.S. Geological Survey (USGS). No warranty,
expressed or implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of release
constitute any such warranty. The software is provided on the condition that
neither the USGS nor the U.S. Government shall be held liable for any damages
resulting from the authorized or unauthorized use of the software.
'''

approvedfmt = '''  character(len=*), parameter :: FMTDISCLAIMER =                                &
    "(/,                                                                        &
    &'This software has been approved for release by the U.S. Geological ',/,   &
    &'Survey (USGS). Although the software has been subjected to rigorous ',/,  &
    &'review, the USGS reserves the right to update the software as needed ',/, &
    &'pursuant to further analysis and review. No warranty, expressed or ',/,   &
    &'implied, is made by the USGS or the U.S. Government as to the ',/,        &
    &'functionality of the software and related material nor shall the ',/,     &
    &'fact of release constitute any such warranty. Furthermore, the ',/,       &
    &'software is released on condition that neither the USGS nor the U.S. ',/, &
    &'Government shall be held liable for any damages resulting from its ',/,   &
    &'authorized or unauthorized use. Also refer to the USGS Water ',/,         &
    &'Resources Software User Rights Notice for complete use, copyright, ',/,   &
    &'and distribution information.',/)"'''

preliminaryfmt = '''  character(len=*), parameter :: FMTDISCLAIMER =                                &
    "(/,                                                                        &
    &'This software is preliminary or provisional and is subject to ',/,        &
    &'revision. It is being provided to meet the need for timely best ',/,      &
    &'science. The software has not received final approval by the U.S. ',/,    &
    &'Geological Survey (USGS). No warranty, expressed or implied, is made ',/, &
    &'by the USGS or the U.S. Government as to the functionality of the ',/,    &
    &'software and related material nor shall the fact of release ',/,          &
    &'constitute any such warranty. The software is provided on the ',/,        &
    &'condition that neither the USGS nor the U.S. Government shall be held ',/,&
    &'liable for any damages resulting from the authorized or unauthorized ',/, &
    &'use of the software.',/)"'''


def get_version_str(v0, v1, v2, v3):
    version_type = ('{}'.format(v0),
                    '{}'.format(v1),
                    '{}'.format(v2),
                    '{}'.format(v3))
    version = '.'.join(version_type)
    return version


def get_tag(v0, v1, v2):
    tag_type = ('{}'.format(v0),
                '{}'.format(v1),
                '{}'.format(v2))
    tag = '.'.join(tag_type)
    return tag


def get_disclaimer(branch):
    if branch.lower() == 'master':
        disclaimer = approved
    else:
        disclaimer = preliminary
    return disclaimer


def get_disclaimerfmt(branch):
    if branch.lower() == 'master':
        disclaimer = approvedfmt
    else:
        disclaimer = preliminaryfmt
    return disclaimer
    

def update_version():
    try:
        pth = os.path.join(vpth, files[0])

        vmajor = 0
        vminor = 0
        vmicro = 0
        vbuild = 0
        lines = [line.rstrip('\n') for line in open(pth, 'r')]
        for line in lines:
            t = line.split()
            if 'major =' in line:
                vmajor = int(t[2])
            elif 'minor =' in line:
                vminor = int(t[2])
            elif 'micro =' in line:
                vmicro = int(t[2])
            elif 'build =' in line:
                vbuild = int(t[2])

        v0 = get_version_str(vmajor, vminor, vmicro, vbuild)

        # get latest build number
        tag = get_tag(vmajor, vminor, vmicro)
        print('determining version build from {}'.format(tag))
        try:
            b = subprocess.Popen(("git", "describe", "--match", tag),
                                 stdout=subprocess.PIPE).communicate()[0]
            vbuild = int(b.decode().strip().split('-')[1]) + 1
        # assume if tag does not exist that it has not been added
        except:
            vbuild = 0

        v1 = get_version_str(vmajor, vminor, vmicro, vbuild)

        # get current build number
        b = subprocess.Popen(("git", "describe", "--match", "build"),
                             stdout=subprocess.PIPE).communicate()[0]
        vcommit = int(b.decode().strip().split('-')[1]) + 2

        print('Updating version:')
        print('  ', v0, '->', v1)

        # write new version file
        now = datetime.datetime.now()
        f = open(pth, 'w')
        f.write('# {} version file automatically '.format(prod) +
                'created using...{0}\n'.format(os.path.basename(__file__)))
        f.write('# created on...' +
                '{0}\n'.format(
                    now.strftime('%B %d, %Y %H:%M:%S')))
        f.write('\n')
        f.write('major = {}\n'.format(vmajor))
        f.write('minor = {}\n'.format(vminor))
        f.write('micro = {}\n'.format(vmicro))
        f.write('build = {}\n'.format(vbuild))
        f.write('commit = {}\n\n'.format(vcommit))
        f.write("__version__ = '{:d}.{:d}.{:d}'.format(major, minor, micro)\n")
        f.write(
            "__build__ = '{:d}.{:d}.{:d}.{:d}'.format(major, minor, micro, build)\n")
        f.write("__git_commit__ = '{:d}'.format(commit)\n")
        f.close()
        print('Succesfully updated {}'.format(files[0]))
        
        # update latex version file
        if vbuild == 0:
            version = get_tag(vmajor, vminor, vmicro)
        else:
            version = get_version_str(vmajor, vminor, vmicro, vbuild)
        pth = os.path.join(vpth, files[1])
        f = open(pth, 'w')
        line = '\\newcommand{\\modflowversion}{mf' + \
               '{}'.format(version) + '}' 
        f.write('{}\n'.format(line))
        line = '\\newcommand{\\modflowdate}{' + \
               '{}'.format(now.strftime('%B %d, %Y')) + \
               '}'
        f.write('{}\n'.format(line))
        line = '\\newcommand{\\currentmodflowversion}' + \
               '{Version \\modflowversion---\\modflowdate}'
        f.write('{}\n'.format(line))
        f.close()
        print('Succesfully updated {}'.format(files[1]))
    except:
        print('There was a problem updating the version file')
        sys.exit(1)
        
    # update version.f90
    update_mf6_version(vmajor, vminor, vmicro, vbuild)

    # update README.md with new version information
    update_readme_markdown(vmajor, vminor, vmicro, vbuild)


def add_updated_files():
    try:
        # add modified version file
        print('Adding updated files to repo')
        b = subprocess.Popen(("git", "add", "-u"),
                             stdout=subprocess.PIPE).communicate()[0]
    except:
        print('Could not add updated files')
        sys.exit(1)

def get_branch():
    try:
        # determine current buildstat branch
        b = subprocess.Popen(("git", "status"),
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT).communicate()[0]
        if isinstance(b, bytes):
            b = b.decode('utf-8')

        # determine current buildstat branch
        for line in b.splitlines():
            if 'On branch' in line:
                branch = line.replace('On branch ', '').rstrip()
    except:
        branch = None
    
    return branch

def update_mf6_version(vmajor, vminor, vmicro, vbuild):
    branch = get_branch()
    if branch is None:
        print('Cannot update MODFLOW 6 version - could not determine current branch')
        return
        
    # create version
    version = get_tag(vmajor, vminor, vmicro)
    idevelopmode = 0
    if 'master' not in branch.lower():
        version = '{}.{}'.format(version, vbuild)
        idevelopmode = 1
    
    # develop date text
    now = datetime.datetime.now()
    sdate = now.strftime('%m/%d/%Y')
    
    # create disclaimer text
    disclaimerfmt = get_disclaimerfmt(branch)
    
    # read version.f90 into memory
    fpth = os.path.join('src', 'Utilities', 'version.f90')
    with open(fpth, 'r') as file:
        lines = [line.rstrip() for line in file]

    # rewrite version.f90
    skip = False
    f = open(fpth, 'w')
    for line in lines:
        # skip all of the disclaimer text
        if skip:
            if ',/)"' in line:
                skip = False
            continue
        elif 'IDEVELOPMODE' in line:
            line = '  integer(I4B), parameter :: ' + \
                   'IDEVELOPMODE = {}'.format(idevelopmode)
        elif 'VERSION' in line:
            line = "  character(len=40), parameter :: " + \
                   "VERSION = '{} {}'".format(version, sdate)
        elif 'FMTDISCLAIMER' in line:
            line = disclaimerfmt
            skip = True
        f.write('{}\n'.format(line))
    f.close()

    return

def update_readme_markdown(vmajor, vminor, vmicro, vbuild):
#    try:
#        # determine current buildstat branch
#        b = subprocess.Popen(("git", "status"),
#                             stdout=subprocess.PIPE,
#                             stderr=subprocess.STDOUT).communicate()[0]
#        if isinstance(b, bytes):
#            b = b.decode('utf-8')
#
#        # determine current buildstat branch
#        for line in b.splitlines():
#            if 'On branch' in line:
#                branch = line.replace('On branch ', '').rstrip()
#    except:
#        print('Cannot update README.md - could not determine current branch')
#        return
    branch = get_branch()
    if branch is None:
        print('Cannot update README.md - could not determine current branch')
        return
        
    # create version
    version = get_tag(vmajor, vminor, vmicro)
    
    # create disclaimer text
    disclaimer = get_disclaimer(branch)

    # read README.md into memory
    with open(files[2], 'r') as file:
        lines = [line.rstrip() for line in file]

    # rewrite README.md
    terminate = False
    f = open(files[2], 'w')
    for line in lines:
        if '### Version ' in line:
            line = '### Version {}'.format(version)
            if vbuild > 0:
                line += ' {} &mdash; build {}'.format(branch, vbuild)
        elif '[Build Status]' in line:
            line = '[![Build Status](https://travis-ci.org/MODFLOW-USGS/' + \
                   'modflow6.svg?branch={})]'.format(branch) + \
                   '(https://travis-ci.org/MODFLOW-USGS/modflow6)'
        elif 'https://doi.org/10.5066/F76Q1VQV' in line:
            now = datetime.datetime.now()
            sb = ''
            if vbuild > 0:
                sb = ' &mdash; {}'.format(branch)
            line = '[Langevin, C.D., Hughes, J.D., ' + \
                   'Banta, E.R., Provost, A.M., ' + \
                   'Niswonger, R.G., and Panday, Sorab, ' + \
                   '{}, '.format(now.year) + \
                   'MODFLOW 6 Modular Hydrologic Model ' + \
                   'version {}{}: '.format(version, sb) + \
                   'U.S. Geological Survey Software Release, ' + \
                   '{}, '.format(now.strftime('%d %B %Y')) + \
                   'https://doi.org/10.5066/F76Q1VQV]' + \
                   '(https://doi.org/10.5066/F76Q1VQV)'
        elif 'Disclaimer' in line:
            line = disclaimer
            terminate = True
        f.write('{}\n'.format(line))
        if terminate:
            break
    f.close()
    
    # write disclaimer markdown file
    f = open(files[3], 'w')
    f.write(disclaimer)
    f.close()
    
    # load and modify json file
    jsonFile = open(files[4], 'r') # Open the JSON file for reading
    data = json.load(jsonFile) # Read the JSON into the buffer
    jsonFile.close() # Close the JSON file
    
    # modify the json file data
    now = datetime.datetime.now()
    sdate = now.strftime('%Y-%m-%d')
    data[0]['date']['metadataLastUpdated'] = sdate
    if branch.lower() == 'master':
        data[0]['version'] = version
        data[0]['status'] = 'Production'
    else:
        data[0]['version'] = version + '.{}'.format(vbuild)
        data[0]['status'] = 'Release Candidate'
    
    # rewrite the json file
    with open(files[4], 'w') as f:
        json.dump(data, f, indent=4)  
          
    return


if __name__ == "__main__":
    update_version()
    add_updated_files()
