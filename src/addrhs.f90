subroutine addrhs(brhs,elresf,lm,nee)
    !
    !.... program to add element residual-force vector to
    !     global right-hand-side vector
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single-precision operation
    dimension brhs(1),elresf(1),lm(1)
    
    do 100 j=1,nee
        k = lm(j)
        if (k.gt.0) brhs(k) = brhs(k) + elresf(j)
100 continue
    
    return
    
end subroutine addrhs
    