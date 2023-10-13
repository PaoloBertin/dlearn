subroutine clear(a,m)
    !
    !.... program to clear a floating-point array
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single-precision operation
    
    dimension a(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    do 100 i=1,m
        a(i) = zero
100 continue

    return

end subroutine clear