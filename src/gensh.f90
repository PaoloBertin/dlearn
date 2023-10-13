subroutine gensh(r,s,t,sh,numgp,iopt)
    !
    ! subroutine to call shape function routines
    ! for isoparametric generation
    !
    implicit double precision (a-h,o-z)
    
    ! modify above card for single-precision operation
    dimension sh(1)
    
    go to (100,200,300),iopt
    
100 call gensh1(r,sh,numgp)
    return
    
200 call gensh2(r,s,sh,numgp)
    return
    
    300 call gensh3(r,s,t,sh,numgp)
    return
    
end subroutine gensh
    