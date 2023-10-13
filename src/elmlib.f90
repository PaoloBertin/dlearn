subroutine elmlib(ntype,mpnpar,itask,neg)
    !
    ! subroutine to call element routines
    !
    !-ZACE-2005.08
    include 'memory_size.inc'
    common a(MAX_SIZE)
    
    go to (100,200,300),ntype
    
100 continue
    call quadc(itask,a(mpnpar),a(mpnpar+16),neg)
    return
    
200 continue
    call truss(itask,a(mpnpar),a(mpnpar+16),neg)
    return
    
300 continue
    call brickc(itask,a(mpnpar),a(mpnpar+16),neg)
    return
    
    ! add additional elements for fun and valuable prizes
    
end subroutine elmlib
    