#!/bin/bash

#SBATCH -J software-build
#SBATCH --hint=nomultithread
#SBATCH -o software-build-%j.out

## This is "version 2" of the Denali build script which,
## is assoicated with a new repo and attempt to formalize this all more
Buildscript_version="2.1"

#BEGIN FUNC DEFS

function helpmsg_and_exit {
  [[ $1 ]] && echo $1
cat <<EOF
    Usage:
      sbatch install_v2.slurm <all|intel|cray|gnu> <package> <package_version>
    
    Examples: 
      sbatch install.slurm all openblas 0.22.2
      sbatch install.slurm gnu R 4.2.0
EOF
    exit
}

function errmsg_and_exit {
    echo -n "ERROR: "
    echo $1
    exit 100
}

## Function to download/untar source if needed
function func_setup_source {
    : # true/placeholder
}

## Function to set target/compilation environment. 
## Also creates dirs where the compiled code will go.
##  Relies on global variables defined below.
##  Sets global $Prefix which is used elsewhere.
function func_env_set {

    # actually switch to the target PE module (necessary for compiler variables)
    if [ "$Target_pe" != "${PE_ENV,,}" ] # PE_ENV Defined by Cray Programming Environment
    then
        cmd="module switch PrgEnv-${PE_ENV,,} PrgEnv-${Target_pe,,} && module petsc"
        echo $cmd
        eval $cmd || errmsg_and_exit "Failed to eval $cmd"
    fi

    # This needs to happen here because it relies on variables set by the module switch above
    case $Target_pe in
        "cray")
            Prefix="${Install_root}/CRAYCLANG/${PE_LEVEL}"
        ;;
        "gnu")
            Prefix="${Install_root}/${Target_pe^^}/${GNU_VERSION}"
            export PKG_CONFIG_PATH=/opt/cray/pe/mpt/7.7.19/gni/mpich-gnu/8.1/lib/pkgconfig:$PKG_CONFIG_PATH
        ;;
        "intel")
            Prefix="${Install_root}/${Target_pe^^}/${INTEL_MAJOR_VERSION}"
        ;;
        *)
            errmsg_and_exit "Unrecognized Target PE: $Target_pe" #this shouldn't happen
        ;;
    esac
}
## Called after func_env_set
##  broken out for debugging. May not be needed.
function func_env_create {
 
    if [ ! -d $Prefix ]
    then
        mkdir -p $Prefix || errmsg_and_exit "Failed to create dir: $Prefix"
    fi
}

function func_configure {

    # Modflow6 Meson build specific
    CC=cc CXX=CC F77=ftn F90=ftn FC=ftn meson setup build-${Target_pe} --reconfigure --prefix=$Prefix --libdir=bin --Dparallel=true
    
    [[ $? -ne 0 ]] && errmsg_and_exit "Failed at configure step!"
}

function func_make {

    # Modflow6 Meson build specific
    CC=cc CXX=CC F77=ftn F90=ftn FC=ftn meson compile -C build-${Target_pe}
    
    [[ $? -ne 0 ]] && errmsg_and_exit "Failed at make step!"
}

function func_install {

    #make install 

    # Modflow6 Meson build specific
    CC=cc CXX=CC F77=ftn F90=ftn FC=ftn meson install -C build-${Target_pe}

    [[ $? -ne 0 ]] && errmsg_and_exit "Failed at make-install step!"
}

## Function to install a env module for package. Relies on global variables defined below.
function func_module_install {

    local module_dir="${Software_home}/modulefiles/${Application_name}"
    local module_template="${Buildscript_home}/modulefile_template"
    local module_file="${module_dir}/${Application_version}"

    if [ ! -d "$module_dir" ]
    then
        mkdir $module_dir || errmsg_and_exit "Failed to mkdir $module_dir"
    fi
    
    install -o crayadm -g crayadm -m 644 $module_template $module_file || errmsg_and_exit "Failed to install module template"
    
    # note that a dash in the app name was a problem so we replace them with underscores for "application".
    sed -i "s/{application}/${Application_name//-/_}/g" $module_file
    sed -i "s/{APPLICATION}/${Application_name^^}/g" $module_file
    sed -i "s/{VERSION}/$Application_version/g" $module_file
    
    if [ -f "$module_file" ]
    then
	    echo "Modulefile created: ${module_file}"
    else
	    errmsg_and_exit "modulefile creation failed for: ${module_file}"
    fi
}

## Function to clean package. Default is "make clean", or optional parameter like distclean, etc.
function func_clean {

    # Modflow6 6.4.2 specific
    rm -rf builddir
    
    [[ $? -ne 0 ]] && errmsg_and_exit "Failed at make-clean step!"
}

## Function to build for a target/PE - used for multiple targets.
##  Or, you can move these functions to func_main for each target
function func_build {
    
    # Install $Prefix gets set as a result of func_env_set()
    func_env_set

    ## Modules that all target PEs need to build the package

    # Meson build specific
    module load meson ninja

    func_configure
    func_make
    func_install

    # func_module_install

    ## Don't clean - keep build dir "build-$Target_pe" for logs
    #func_clean
}

function func_main {

    case "$Target_pe" in
        "cray")
            ## Cray customizations go here
            echo "--- Compiling for cray" 
            func_build
        ;;
        "intel")
            ## Intel customizations go here
            echo "--- Compiling for intel"
            func_build
        ;;
        "gnu")
            ## GNU customizations go here
            echo "--- Compiling for gnu"
            func_build
        ;;
        "all")
            for t in cray intel gnu
            do
                Target_pe=$t
                func_main
            done
        ;;
        "debug")
            for t in cray intel gnu
            do
                Target_pe=$t
                func_env_set
                
                echo "DEBUG: Target_pe: $Target_pe"
                echo "DEBUG: Application_name: $Application_name"
                echo "DEBUG: Application_version: $Application_version"
                echo "DEBUG: Extra_args: ${Extra_args[@]}"
                echo "DEBUG: Software_home: $Software_home"
                echo "DEBUG: Source_home: $Source_home"
                echo "DEBUG: Buildscript_home: $Buildscript_home"
                echo "DEBUG: Install_root: $Install_root"
                echo "DEBUG: Prefix: $Prefix"
            done
        ;;
        *)
            errmsg_and_exit "Unrecognized Target PE: $Target_pe"
        ;;
esac
}
#END FUNC DEFS

#BEGIN MAIN ROUTINE

## Setup error handling
#set -o errtrace
#trap "echo ERROR: There was an error in ${FUNCNAME-main context}, details to follow" ERR
set -x # Echo all commands

## Check command-line arguments and set parameters

[ $# -lt 3 ] && helpmsg_and_exit "ERROR: Incorrect number of aguments to script"
argv=($@)

## Set command-line arguments

Target_pe=${argv[0]}
Application_name=${argv[1]}
Application_version=${argv[2]}

Extra_args=(${argv[@]:3}) # All other command-line options are extra

## change directory to the parent directory
cd ../
echo "running script in '$(pwd)'"

## Set derived variables

Software_home="/home/software/denali/arc"
Source_home="${Software_home}/source"
Buildscript_home="${Source_home}/buildscripts-denali"

Install_root="${Software_home}/apps/${Application_name}/${Application_version}"

## Set up the source package or not
#func_setup_source

## Run the main function for all requested PE targets (put customizations in func_main)
func_main

echo "END of buildscript!"

#END MAIN ROUTINE
