!> @brief This module contains the NCExportCreateModule
!!
!! This module creates derived model netcdf export
!! objects. It is dependent on netcdf libraries.
!!
!<
module NCExportCreateModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg
  use ConstantsModule, only: DIS, DISU, DISV
  use SimModule, only: store_error, store_error_filename
  use NumericalModelModule, only: NumericalModelType
  use BaseDisModule, only: DisBaseType
  use DisModule, only: DisType
  use DisvModule, only: DisvType
  use DisuModule, only: DisuType
  use ModelExportModule, only: export_models, get_export_model
  use ModelExportModule, only: ExportModelType

  implicit none
  private
  public :: nc_export_create

contains

  !> @brief create model netcdf export type
  !!
  subroutine create_nc_export(export_model, num_model)
    use NCModelExportModule, only: NETCDF_UGRID, NETCDF_STRUCTURED
    use MeshDisModelModule, only: Mesh2dDisExportType
    use MeshDisvModelModule, only: Mesh2dDisvExportType
    use DisNCStructuredModule, only: DisNCStructuredType
    type(ExportModelType), pointer, intent(inout) :: export_model
    class(NumericalModelType), pointer, intent(in) :: num_model
    class(Mesh2dDisExportType), pointer :: ugrid_dis
    class(Mesh2dDisvExportType), pointer :: ugrid_disv
    class(DisNCStructuredType), pointer :: structured_dis
    class(DisBaseType), pointer :: disbase
    !
    select case (export_model%disenum)
    case (DIS)
      ! -- allocate nc structured grid export object
      if (export_model%nctype == NETCDF_UGRID) then
        !
        ! -- allocate nc structured grid export object
        allocate (ugrid_dis)
        !
        ! -- set dis base type
        disbase => num_model%dis
        select type (disbase)
        type is (DisType)
          ugrid_dis%dis => disbase
        end select
        !
        ! -- initialize export object
        call ugrid_dis%init(export_model%modelname, export_model%modeltype, &
                            export_model%modelfname, export_model%disenum, &
                            NETCDF_UGRID, export_model%iout)
        !
        ! -- define export object
        call ugrid_dis%df()
        !
        ! -- set base pointer
        export_model%nc_export => ugrid_dis
      else if (export_model%nctype == NETCDF_STRUCTURED) then
        !
        ! -- allocate nc structured grid export object
        allocate (structured_dis)
        !
        ! -- set dis base type
        disbase => num_model%dis
        select type (disbase)
        type is (DisType)
          structured_dis%dis => disbase
        end select
        !
        ! -- initialize export object
        call structured_dis%init(export_model%modelname, export_model%modeltype, &
                                 export_model%modelfname, export_model%disenum, &
                                 NETCDF_STRUCTURED, export_model%iout)
        !
        ! -- define export object
        call structured_dis%df()
        !
        ! -- set base pointer
        export_model%nc_export => structured_dis
      end if
    case (DISV)
      if (export_model%nctype == NETCDF_UGRID) then
        ! -- allocate nc structured grid export object
        allocate (ugrid_disv)
        !
        ! -- set dis base type
        disbase => num_model%dis
        select type (disbase)
        type is (DisvType)
          ugrid_disv%disv => disbase
        end select
        !
        ! -- initialize export object
        call ugrid_disv%init(export_model%modelname, export_model%modeltype, &
                             export_model%modelfname, export_model%disenum, &
                             NETCDF_UGRID, export_model%iout)
        !
        ! -- define export object
        call ugrid_disv%df()
        !
        ! -- set base pointer
        export_model%nc_export => ugrid_disv
      else
        errmsg = 'DISV model discretization only &
                 &supported as UGRID NetCDF export. &
                 &Model='//trim(export_model%modelname)//'.'
        call store_error(errmsg)
        call store_error_filename(export_model%modelfname)
      end if
    case default
      errmsg = 'Unsupported discretization for NetCDF model export. &
               &Model='//trim(export_model%modelname)//'.'
      call store_error(errmsg)
      call store_error_filename(export_model%modelfname)
    end select
  end subroutine create_nc_export

  !> @brief initialize netcdf model export type
  !!
  subroutine nc_export_create()
    use NumericalModelModule, only: GetNumericalModelFromList
    use ListsModule, only: basemodellist
    use NCModelExportModule, only: NETCDF_UNDEF
    integer(I4B) :: n
    type(ExportModelType), pointer :: export_model
    class(NumericalModelType), pointer :: num_model
    integer(I4B) :: im
    !
    do n = 1, export_models%Count()
      ! -- set pointer to export model
      export_model => get_export_model(n)
      if (export_model%nctype /= NETCDF_UNDEF) then
        !
        ! -- netcdf export is active identify model
        do im = 1, basemodellist%Count()
          !
          ! -- set model pointer
          num_model => GetNumericalModelFromList(basemodellist, im)
          if (num_model%name == export_model%modelname .and. &
              num_model%macronym == export_model%modeltype) then
            !
            ! -- allocate and initialize nc export model
            call create_nc_export(export_model, num_model)
            exit
            !
          end if
        end do
      end if
    end do
  end subroutine nc_export_create

end module NCExportCreateModule
