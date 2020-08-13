!> @brief This module contains helper routines and parameters for the MODFLOW 6 BMI
!!
!<
module mf6bmiUtil  
  use iso_c_binding, only: c_int, c_char, c_null_char
  use ConstantsModule, only: MAXCHARLEN, LENMEMPATH, LENVARNAME, &
                             LENMODELNAME, LINELENGTH, LENMEMTYPE, &
                             LENMEMADDRESS, LENCOMPONENTNAME
  use KindModule, only: DP, I4B  
  use GenericUtilitiesModule, only: sim_message
  use SimVariablesModule, only: istdout
  use MemoryHelperModule, only: split_mem_address
  implicit none
  
  ! the following exported parameters will trigger annoying warnings with
  ! the Intel Fortran compiler (4049,4217). We know that these can be ignored:
  !
  ! https://community.intel.com/t5/Intel-Fortran-Compiler/suppress-linker-warnings/td-p/855137
  ! https://community.intel.com/t5/Intel-Fortran-Compiler/Locally-Defined-Symbol-Imported/m-p/900805
  !
  ! and gfortran does so anyway. They have been disabled in the linker config.

  integer(I4B), parameter :: LENGRIDTYPE = 16 !< max length for Fortran grid type string

  integer(c_int), bind(C, name="BMI_LENVARTYPE") :: BMI_LENVARTYPE = LENMEMTYPE + 1 !< max. length for variable type C-strings
  !DEC$ ATTRIBUTES DLLEXPORT :: BMI_LENVARTYPE

  integer(c_int), bind(C, name="BMI_LENGRIDTYPE") :: BMI_LENGRIDTYPE = LENGRIDTYPE + 1 !< max. length for grid type C-strings
  !DEC$ ATTRIBUTES DLLEXPORT :: BMI_LENGRIDTYPE
  
  integer(c_int), bind(C, name="BMI_LENVARADDRESS") :: BMI_LENVARADDRESS = LENMEMADDRESS + 1 !< max. length for the variable's address C-string
  !DEC$ ATTRIBUTES DLLEXPORT :: BMI_LENVARADDRESS

contains
   
  !> @brief split the variable address string
  !!
  !! splits the full address string into a memory path and variable name,
  !! following the rules used by the memory manager
  !!
  !! @param[in]   c_var_address   the full address C-string of a variable
  !! @param[out]  mem_path        the memory path used by the memory manager
  !! @param[out]  var_name        the name of the variable (without the path)
  !!
  !<
  subroutine split_address(c_var_address, mem_path, var_name)
    use MemoryHelperModule, only: memPathSeparator
    character (kind=c_char), intent(in) :: c_var_address(*)
    character(len=LENMEMPATH), intent(out) :: mem_path
    character(len=LENVARNAME), intent(out) :: var_name    
    ! local
    integer(I4B) :: idx
    character(len=LENMEMPATH) :: var_address   

    ! convert to fortran string
    var_address = char_array_to_string(c_var_address, strlen(c_var_address)) 

    split_mem_address(var_address, meme_path, var_name)
        
  end subroutine split_address

  integer(c_int) pure function strlen(char_array)
    character(c_char), intent(in) :: char_array(LENMEMPATH)
    integer(I4B) :: i
    
    strlen = 0
    do i = 1, size(char_array)
      if (char_array(i) .eq. C_NULL_CHAR) then
          strlen = i-1
          exit
      end if
    end do
    
  end function strlen

  pure function char_array_to_string(char_array, length)
    integer(c_int), intent(in) :: length
    character(c_char),intent(in) :: char_array(length)
    character(len=length) :: char_array_to_string
    integer(I4B) :: i
    
    do i = 1, length
      char_array_to_string(i:i) = char_array(i)
    enddo
    
  end function char_array_to_string

  pure function string_to_char_array(string, length)
  integer(c_int),intent(in) :: length
  character(len=length), intent(in) :: string
  character(kind=c_char,len=1) :: string_to_char_array(length+1)
  integer(I4B) :: i
  
  do i = 1, length
      string_to_char_array(i) = string(i:i)
  enddo
  string_to_char_array(length+1) = C_NULL_CHAR
  
  end function string_to_char_array

  ! get the model name from the string, assuming that it is
  ! the substring in front of the first space
  pure function extract_model_name(var_name)
    character(len=*), intent(in) :: var_name
    character(len=LENMODELNAME) :: extract_model_name
    integer(I4B) :: idx
    
    idx = index(var_name, ' ')
    extract_model_name = var_name(:idx-1)
    
  end function extract_model_name

  function get_model_name(grid_id) result(model_name)
    use ListsModule, only: basemodellist
    use BaseModelModule, only: BaseModelType, GetBaseModelFromList
    integer(kind=c_int), intent(in) :: grid_id
    character(len=LENMODELNAME) :: model_name
    ! local
    integer(I4B) :: i
    class(BaseModelType), pointer :: baseModel    
    character(len=LINELENGTH) :: error_msg
    
    model_name = ''
    
    do i = 1,basemodellist%Count()
      baseModel => GetBaseModelFromList(basemodellist, i)
      if (baseModel%id == grid_id) then
        model_name = baseModel%name
        return
      end if
    end do
    
    write(error_msg,'(a,i0)') 'BMI error: no model for grid id ', grid_id
    call sim_message(error_msg, iunit=istdout, skipbefore=1, skipafter=1)
  end function get_model_name

  ! the subcomponent_idx runs from 1 to the nr of 
  ! solutions in the solution group
  function getSolution(subcomponent_idx) result(solution)
    use SolutionGroupModule
    use NumericalSolutionModule
    use ListsModule, only: basesolutionlist, solutiongrouplist
    integer(I4B), intent(in) :: subcomponent_idx
    class(NumericalSolutionType), pointer :: solution
    ! local
    class(SolutionGroupType), pointer :: sgp
    integer(I4B) :: solutionIdx
    
    ! this is equivalent to how it's done in sgp_ca
    sgp => GetSolutionGroupFromList(solutiongrouplist, 1)
    solutionIdx = sgp%idsolutions(subcomponent_idx)
    solution => GetNumericalSolutionFromList(basesolutionlist, solutionIdx)    
    
  end function getSolution

end module mf6bmiUtil