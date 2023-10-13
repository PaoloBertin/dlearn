subroutine princ(n,s,p)
    !
    !.... program to compute principal values of symmetric 2nd-rank tensor
    !
    !        s = symmetric second-rank tensor stored as a vector
    !        n = number of dimensions (2 or 3)
    !        p = vector of principal values
    !
    !.... the components of s must be stored in the following orders
    !
    !        2-d problems: s11,s22,s12
    !        3-d problems: s11,s22,s33,s12,s23,s31
    !
    implicit double precision (a-h,o-z)

    !.... deactivate above card(s) for single precision operation
    dimension s(1),p(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    data rt2/1.41421356237309/,pi23/2.09439510239321/,two2/22.50/,four5/45.0/

    if (n.eq.2) then

        ! 2-d problem
        a = two2/atan(one)
        x = pt5*(s(1) + s(2))
        y = pt5*(s(1) - s(2))
        r = sqrt(y*y + s(3)*s(3))
        p(1) = x + r
        p(2) = x - r
        p(3) = r
        p(4) = four5
        if (y.ne.zero .or. s(3).ne.zero) p(4) = a*atan2(s(3),y)
    endif

    if (n.eq.3) then

        ! 3-d problem
100     r = zero
        x = (s(1) + s(2) + s(3))/three
        y = s(1)*(s(2) + s(3)) + s(2)*s(3) - s(4)*s(4) - s(6)*s(6) - s(5)*s(5)
        z = s(1)*s(2)*s(3) - two*s(4)*s(6)*s(5) - s(1)*s(5)*s(5) - s(2)*s(6)*s(6) - s(3)*s(4)*s(4)
        t = three*x*x - y
        u = zero
        if (t.ne.zero) then
            u = sqrt(two*t/three)
            ucubed = u*u*u
            if (ucubed.ne.zero) then
                a = (z + (t - x*x)*x)*rt2/ucubed
                r = sqrt(abs(one - a*a))
                if (r.ne.zero.or.a.ne.zero) then
                    r = atan2(r,a)/three
                else
                    r = zero
                endif
            else
                u = zero
            endif
        endif
        p(1) = x + u*rt2*cos(r)
        p(2) = x + u*rt2*cos(r - pi23)
        p(3) = x + u*rt2*cos(r + pi23)
    endif

    return

end subroutine princ

