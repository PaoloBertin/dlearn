function coldot(a,b,n)
    !
    !.... function to compute the dot product of vectors stored column-wise
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single-precision operation
    !
    dimension a(1),b(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    !
    coldot = zero
    !
    do 100 i=1,n
        coldot = coldot + a(i)*b(i)
100 continue
    
    return
    
end function coldot
    