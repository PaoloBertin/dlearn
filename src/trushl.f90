subroutine trushl(shl,w,nint,nen)
    !
    !.... program to calculate integration-rule weights, shape functions
    !        and local derivatives for a two or three node,
    !        one-dimensional element
    !
    !                 r = local element coordinate ("xi")
    !        shl(1,i,l) = local ("xi") derivative of shape function
    !        shl(2,i,l) = shape function
    !              w(l) = integration-rule weight
    !                 i = local node number
    !                 l = integration-point number
    !              nint = number of integration points, eq. 1, 2 or 3
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single precision operation
    !
    dimension shl(2,nen,1),w(1),ra(3)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    data    ra/-1.00,1.00,0.00/,                                                                                                   &
         five9/0.5555555555555555/,eight9/0.8888888888888888/
    
    if (nint.eq.1) then
        w(1) = two
        g = zero
    endif
    
    if (nint.eq.2) then
        w(1) = one
        w(2) = one
        g = one/sqrt(three)
    endif
    
    if (nint.eq.3) then
        w(1) = five9
        w(2) = five9
        w(3) = eight9
        g = sqrt(three/five)
    endif
    
    do 100 l=1,nint
        r = g*ra(l)
        
        shl(1,1,l) = - pt5
        shl(1,2,l) =   pt5
        shl(2,1,l) =   pt5*(one - r)
        shl(2,2,l) =   pt5*(one + r)
        !
        if (nen.eq.3) then
            shl(1,3,l) = - two*r
            shl(2,3,l) = one - r**2
            !
            temp = - pt5*shl(2,3,l)
            shl(1,1,l) = shl(1,1,l) + r
            shl(1,2,l) = shl(1,2,l) + r
            shl(2,1,l) = shl(2,1,l) + temp
            shl(2,2,l) = shl(2,2,l) + temp
            
        endif
        
100 continue
    
    return
    
end subroutine trushl

