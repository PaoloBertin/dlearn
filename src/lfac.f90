subroutine lfac(g,t,g1,nltftn,nptslf)
    !
    !.... program to compute load factors at time t
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    dimension g(nptslf,2,1),g1(1)
    
    do 100 nlf=1,nltftn
        call interp(g(1,1,nlf),g(1,2,nlf),t,g1(nlf),nptslf)
100 continue
    
    return
    
end subroutine lfac
    