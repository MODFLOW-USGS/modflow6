module TestNpfModule
  use ftnunit
  use TestData
  
  implicit none
  
  private
  public :: testAllNpf
 
contains
  
  subroutine testAllNpf()
  
    call test(testNpfInitMem, "Npf, set up from memory")
    call test(testNpfFormulateWithMask, "Npf, test masking of connections: masked connections should not contribute to amat")
      
  end subroutine
  
  subroutine testNpfInitMem()
    use KindModule, only: DP, I4B
    use BaseDisModule
    use GwfDisModule
    use GwfNpfModule
    use GwfModule
    class(DisBaseType), pointer :: dis
    type(GwfNpfType), pointer :: npf
    class(GwfModelType), pointer :: gwf
    
    integer(I4B), pointer :: ixt3d
    integer(I4B), dimension(:), pointer :: icelltype
    real(DP), dimension(:), pointer ::k11
    
    ! need a dummy gwf model because of memory mgr repointering (e.g. INEWTON)
    allocate(gwf)
    call gwf%allocate_scalars("modelname4")
    
    ! create dis
    open(unit=1971, file=TESTDATADIR//"discretizations/dis_2x3.dis", status='old', action='read')
    call dis_cr(dis, "modelname4", 1971, -1)
    call dis%dis_df()
    close(1971)
    
    call npf_cr(npf, "modelname4", 0, 0)
    
    allocate(ixt3d)
    allocate(icelltype(dis%nodes))
    allocate(k11(dis%nodes))
    
    ixt3d = 0
    icelltype = 1
    k11 = 1.0_DP
    
    call npf%npf_init_mem(dis, ixt3d, icelltype, k11)
    
    call assert_equal_realdp(npf%k11(1), 1.0_DP, "Conductivity was properly set")
    call assert_equal(npf%ixt3d, 0, "XT3D flag should be set")
    
  end subroutine testNpfInitMem
  
  subroutine testNpfFormulateWithMask()
    use KindModule, only: DP, I4B
    use BaseDisModule
    use GwfDisModule
    use GwfNpfModule
    use GwfModule
    use Xt3dModule
    class(DisBaseType), pointer :: dis
    type(GwfNpfType), pointer :: npf
    class(GwfModelType), pointer :: gwf
    
    integer(I4B) :: i    
    integer(I4B), pointer :: ixt3d
    integer(I4B), dimension(:), pointer :: icelltype
    real(DP), dimension(:), pointer ::k11
    
    integer(I4B) :: kiter, njasln
    integer(I4B), dimension(:), pointer :: idxglo, ibound
    real(DP), dimension(:), pointer :: amat, rhs, hnew
    integer(I4B), dimension(:), pointer :: newMask
    
    ! need a dummy gwf model because of memory mgr repointering (e.g. INEWTON)
    allocate(gwf)
    call gwf%allocate_scalars("modelname40")
    
    ! create dis
    open(unit=1971, file=TESTDATADIR//"discretizations/dis_2x3.dis", status='old', action='read')
    call dis_cr(dis, "modelname40", 1971, -1)
    call dis%dis_df()
    close(1971)
    
    call npf_cr(npf, "modelname40", 0, 0)
    
    allocate(ixt3d)
    allocate(icelltype(dis%nodes))
    allocate(k11(dis%nodes))
    allocate(ibound(dis%nodes))
    
    ixt3d = 0
    icelltype = 1
    k11 = 1.0_DP    
    call npf%npf_init_mem(dis, ixt3d, icelltype, k11)
    
    ibound = 1
    npf%ibound => ibound
    
    ! set the mask for all connections to zero
    do i = 1, dis%con%nja
      call dis%con%set_mask(i, 0)
    end do
    
    kiter = 1
    njasln = dis%con%nja
    allocate(idxglo(njasln))
    allocate(amat(njasln))
    allocate(rhs(dis%nodes), hnew(dis%nodes))
        
    do i = 1, njasln
      idxglo(i) = i
    end do
    amat = -999.0_DP 
    rhs = 0.0_DP
    hnew = 1.0_DP
    
    call npf%npf_cf(kiter, dis%nodes, hnew)
    call npf%npf_fc(kiter, njasln, amat, idxglo, rhs, hnew)
    do i = 1, njasln
      call assert_equal_realdp(amat(i), -999.0_DP, "amat elements should remain unchanged")  
    end do
    
    ! and now without a mask
    do i = 1, dis%con%nja
      call npf%dis%con%set_mask(i, 1)
    end do
    amat = -999.0_DP 
    call npf%npf_cf(kiter, dis%nodes, hnew)
    call npf%npf_fc(kiter, njasln, amat, idxglo, rhs, hnew)
    do i = 1, njasln
      call assert_true(amat(i) /= -999.0_DP, "amat elements should have been calculated")  
    end do
    
  end subroutine testNpfFormulateWithMask
  
end module