function rowdot(a,b,ma,mb,n)
    !
    !.... program to compute the dot product of vectors stored row-wise
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    dimension a(ma,1),b(mb,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    rowdot = zero
    
    do 100 i=1,n
        rowdot = rowdot + a(1,i)*b(1,i)
100 continue
    
    return
end function rowdot

