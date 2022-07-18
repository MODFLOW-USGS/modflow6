!> @brief This module contains the TspLabels Module
!!
!! A generic module containing the labels used by
!! the generalized TransportModel module that assigns
!! labeling based on which type of transport model an
!! instance of this module is associated with (GWT or
!! GWE)
!!
!! Labels that need to be transport model type specific:
!!
!!        GWT       |        GWE        | src files w/label
!! -----------------|-------------------|--------------
!! "Concentration"  |"Temperature"      | gwt1.f90/gwe1.f90
!!                  |                   | gwt1apt1.f90
!!                  |                   | gwt1cnc1.f90
!!                  |                   | gwt1ist1.f90
!!                  |                   | gwt1lkt1.f90
!!                  |                   | gwt1mst1.f90
!!                  |                   | gwt1obs1.f90
!!                  |                   | gwt1oc1.f90
!!                  |                   | gwt1sft1.f90 (?)
!!                  |                   | gwt1ssm1.f90
!!                  |                   | gwt1fmi1.f90
!!                  |                   | tsp1ic1.f90
!!                  |                   | GwtSpc.f90
!! "Cumulative Mass"|"Cumulative Energy"| Budget.f90 (_ot routine)
!! "MASS", "M"      |"ENERGY", "E"      | gwt1.f90 (gwt_df routine & _ot routine)
!! "M/T"            |"Watts"  (?)       |
!! "M"              |"Joules" or "E"    |
!<
module TspLabelsModule

  use KindModule, only: DP, LGP, I4B
  use ConstantsModule, only: LENVARNAME

  implicit none
  private
  public :: TspLabelsType
  public :: tsplabels_cr
  public :: setTspLabels

  !> @brief Define labels for use with generalized transport model
  !!
  !! Subroutine to store which type of units are in use since a
  !! generalized transport model is the base clase for either a
  !! solute transport (GWT) or heat transport (GWE)
  !!
  !<
  type TspLabelsType
    
    character(len=LENVARNAME), pointer :: modname => null() !< name of the model that module is associated with
    character(len=LENVARNAME), pointer :: tsptype => null() !< "solute" or "heat"
    character(len=LENVARNAME), pointer :: depvartype => null() !< "concentration" or "temperature"
    character(len=LENVARNAME), pointer :: depvarunit => null() !< "mass" or "energy"
    character(len=LENVARNAME), pointer :: depvarunitabbrev => null() !< "M" or "J"

  contains
    !-- public
    procedure, public :: setTspLabels
    procedure, public :: tsplabels_da
    ! -- private
    procedure :: allocate_label_names

  end type TspLabelsType

contains

  !> @brief Create a new transport labels object
  !!
  !! Create a new labels object
  !!
  !<
  subroutine tsplabels_cr(this, name_model)
    ! -- modules
    ! -- dummy variables
    type(TspLabelsType), pointer :: this !< TspLabelsType object
    character(len=*), intent(in) :: name_model !< name of the model
    ! -------------------------------------------------------------------
    !
    ! -- Create the object
    allocate (this)
    ! -- local variables
    !
    ! -- Allocate variable names
    call this%allocate_label_names(name_model)
    !
    ! -- Return
    return
  end subroutine tsplabels_cr

  !> @brief Define the labels corresponding to the flavor of
  !! transport model
  !!
  !! Set variable names according to type of transport model
  !!
  !<
  subroutine setTspLabels(this, tsptype, depvartype, depvarunit, depvarunitabbrev)
    class(TspLabelsType) :: this
    character(len=*), intent(in) :: tsptype !< type of model, default is GWT6
    character(len=*), intent(in) :: depvartype !< dependent variable type, default is "CONCENTRATION"
    character(len=*), intent(in) :: depvarunit !< units of dependent variable for writing to list file
    character(len=*), intent(in) :: depvarunitabbrev !< abbreviation of associated units
    !
    ! -- Set the model type
    this%tsptype = tsptype
    !
    ! -- Set the type of dependent variable being solved for
    this%depvartype = depvartype
    !
    ! -- Set the units associated with the dependent variable
    this%depvarunit = depvarunit
    !
    ! -- Set the units abbreviation
    this%depvarunitabbrev = depvarunitabbrev
    !
    ! -- Return
    return
  end subroutine setTspLabels

  !> @brief Define the information this object holds
  !!
  !! Allocate strings for storing label names
  !! Intended to be analogous to allocate_scalars()
  !!
  !<
  subroutine allocate_label_names(this, name_model)
    ! -- modules
    ! -- dummy
    class(TspLabelsType) :: this !< TspLabelsType object
    character(len=*), intent(in) :: name_model !< name of the model
    !
    allocate (this%modname)
    allocate (this%tsptype)
    allocate (this%depvartype)
    allocate (this%depvarunit)
    allocate (this%depvarunitabbrev)
    !
    ! -- Initialize values
    this%tsptype = ''
    this%depvartype = ''
    this%depvarunit = ''
    this%depvarunitabbrev = ''
    !
    ! -- Initialize model name that labels module is associated with
    this%modname = name_model
    !
    return
  end subroutine allocate_label_names

  !> @ breif Deallocate memory
  !!
  !!  Deallocate budget memory
  !!
  !<
  subroutine tsplabels_da(this)
    class(TspLabelsType) :: this !< TspLabelsType object
    !
    ! -- Strings
    deallocate (this%modname)
    deallocate (this%tsptype)
    deallocate (this%depvartype)
    deallocate (this%depvarunit)
    deallocate (this%depvarunitabbrev)
    !
    ! -- Return
    return
  end subroutine tsplabels_da

end module TspLabelsModule
