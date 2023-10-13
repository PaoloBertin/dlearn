function rcdot(a,b,ma,n)
    !
    !.... program to compute the dot product of a vector stored row-wise
    !        with a vector stored column-wise
    !
    implicit double precision (a-h,o-z)

    !.... deactivate above card(s) for single-precision operation
    dimension a(ma,1),b(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    rcdot = zero
    
    do 100 i=1,n
        rcdot = rcdot + a(1,i)*b(i)
100 continue
    
    return
    
end function rcdot
