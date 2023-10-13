subroutine ztest(a,n,lzero)
    !
    !.... program to determine if an array contains only zero entries
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    dimension a(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    logical lzero
    
    lzero = .true.
    
    do 100 i=1,n
        if (a(i).ne.zero) then
            lzero = .false.
            return
        endif
100 continue
    
    return
end subroutine ztest

