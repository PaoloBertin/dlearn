subroutine gensh2(r,s,sh,n)
    !
    !.... subroutine to compute 2d shape functions
    !        for isoparametric generation
    !
    implicit double precision (a-h,o-z)
    
    ! modify above card for single-precision operation
    dimension sh(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    r2 = pt5*r
    r1 = pt5 - r2
    r2 = pt5 + r2
    s2 = pt5*s
    s1 = pt5 - s2
    s2 = pt5 + s2
    sh(1) = r1*s1
    sh(2) = r2*s1
    sh(3) = r2*s2
    sh(4) = r1*s2
    if (n.eq.4) return
    
    r3 = one - r*r
    s3 = one - s*s
    sh(5) = r3*s1
    sh(6) = s3*r2
    sh(7) = r3*s2
    sh(8) = s3*r1
    sh(1) = sh(1) - pt5*(sh(5) + sh(8))
    sh(2) = sh(2) - pt5*(sh(6) + sh(5))
    sh(3) = sh(3) - pt5*(sh(7) + sh(6))
    sh(4) = sh(4) - pt5*(sh(8) + sh(7))
    
    return
end subroutine gensh2
    