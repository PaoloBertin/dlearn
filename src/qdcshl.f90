subroutine qdcshl(shl,w,nint)
    !
    !.... program to calculate integration-rule weights, shape functions
    !        and local derivatives for a four-node quadrilateral element
    !
    !               s,t = local element coordinates ("xi", "eta", resp.)
    !        shl(1,i,l) = local ("xi") derivative of shape function
    !        shl(2,i,l) = local ("eta") derivative of shape function
    !        shl(3,i,l) = local  shape function
    !              w(l) = integration-rule weight
    !                 i = local node number
    !                 l = integration point number
    !              nint = number of integration points, eq. 1 or 4
    !
    implicit double precision (a-h,o-z)
    !.... deactivate above card(s) for single precision operation
    
    dimension shl(3,4,1),w(1),ra(4),sa(4)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    data ra/-0.50,0.50,0.50,-0.50/,sa/-0.50,-0.50,0.50,0.50/
    
    g = zero
    w(1) = four
    if (nint.eq.4) then
        g = two/sqrt(three)
        w(1) = one
        w(2) = one
        w(3) = one
        w(4) = one
    endif
    
    do 200 l=1,nint
        r = g*ra(l)
        s = g*sa(l)
        
        do 100 i=1,4
            tempr = pt5 + ra(i)*r
            temps = pt5 + sa(i)*s
            shl(1,i,l) = ra(i)*temps
            shl(2,i,l) = tempr*sa(i)
            shl(3,i,l) = tempr*temps
100     continue
        
200 continue
    
    return
    
end subroutine qdcshl

