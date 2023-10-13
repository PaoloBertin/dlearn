subroutine bkcshl(shl,w,nint)
    !
    !     subroutine to calculate integration-rule weights, shape functions
    !     and local derivatives for a eight-node brick element
    !
    !     r,s,t = local element coord ("xi", "eta", "zeta" resp.)
    !     shl(1,i,l) = local ("xi") derivative of shape function
    !     shl(2,i,l) = local ("eta") derivative of shape function
    !     shl(3,i,l) = local ("zeta") derivative of shape function
    !     shl(4,i,l) = local  shape function
    !           w(l) = integration-rule weight
    !              i = local node number
    !              l = integration point number
    !           nint = number of integration points, eq. 1 or 8
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single precision operation
    dimension shl(4,8,1),w(1),ra(8),sa(8),ta(8)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    data ra/-0.50, 0.50, 0.50,-0.50,-0.50, 0.50, 0.50,-0.50/
    data sa/-0.50,-0.50, 0.50, 0.50,-0.50,-0.50, 0.50, 0.50/
    data ta/-0.50,-0.50,-0.50,-0.50, 0.50, 0.50, 0.50, 0.50/
    
    g = zero
        w(1) = four + four
        if (nint.eq.8) then
            g = two/sqrt(three)
            w(1) = one
            w(2) = one
            w(3) = one
            w(4) = one
            w(5) = one
            w(6) = one
            w(7) = one
            w(8) = one
        endif
    
        do 200 l=1,nint
            r = g*ra(l)
            s = g*sa(l)
            t = g*ta(l)
    
            do 100 i=1,8
                tempr = pt5 + ra(i)*r
                temps = pt5 + sa(i)*s
                tempt = pt5 + ta(i)*t
                shl(1,i,l) = ra(i)*temps*tempt
                shl(2,i,l) = tempr*sa(i)*tempt
                shl(3,i,l) = tempr*temps*ta(i)
                shl(4,i,l) = tempr*temps*tempt
100         continue

200     continue
    
        return
end subroutine bkcshl
    