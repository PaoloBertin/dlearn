subroutine move(a,b,n)
    !
    !.... program to move a floating-point array
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single-precision operation
    dimension a(1),b(1)
    
    do 100 i=1,n
        a(i) = b(i)
100 continue
    
    return

end subroutine move