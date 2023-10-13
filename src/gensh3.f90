subroutine gensh3(r,s,t,sh,n)
    !
    ! subroutine program to compute 3d shape functions
    !        for isoparametric generation
    !
    implicit double precision (a-h,o-z)
    !
    !.... modify above card for single-precision operation
    !
    dimension sh(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    r2 = pt5*r
    r1 = pt5 - r2
    r2 = pt5 + r2
    s2 = pt5*s
    s1 = pt5 - s2
    s2 = pt5 + s2
    t2 = pt5*t
    t1 = pt5 - t2
    t2 = pt5 + t2
    
    rs1 = r1*s1
    rs2 = r2*s1
    rs3 = r2*s2
    rs4 = r1*s2
    sh(1) = rs1*t1
    sh(2) = rs2*t1
    sh(3) = rs3*t1
    sh(4) = rs4*t1
    sh(5) = rs1*t2
    sh(6) = rs2*t2
    sh(7) = rs3*t2
    sh(8) = rs4*t2
    if (n.eq.8) return
    
    r3 = one - r*r
    s3 = one - s*s
    t3 = one - t*t
    sh(17) = t3*rs1
    sh(18) = t3*rs2
    sh(19) = t3*rs3
    sh(20) = t3*rs4
    rs1 = r3*s1
    rs2 = s3*r2
    rs3 = r3*s2
    rs4 = s3*r1
    sh( 9) = rs1*t1
    sh(10) = rs2*t1
    sh(11) = rs3*t1
    sh(12) = rs4*t1
    sh(13) = rs1*t2
    sh(14) = rs2*t2
    sh(15) = rs3*t2
    sh(16) = rs4*t2
    
    sh(1) = sh(1) - pt5*(sh( 9) + sh(12) + sh(17))
    sh(2) = sh(2) - pt5*(sh( 9) + sh(10) + sh(18))
    sh(3) = sh(3) - pt5*(sh(10) + sh(11) + sh(19))
    sh(4) = sh(4) - pt5*(sh(11) + sh(12) + sh(20))
    sh(5) = sh(5) - pt5*(sh(13) + sh(16) + sh(17))
    sh(6) = sh(6) - pt5*(sh(13) + sh(14) + sh(18))
    sh(7) = sh(7) - pt5*(sh(14) + sh(15) + sh(19))
    sh(8) = sh(8) - pt5*(sh(15) + sh(16) + sh(20))
    
    return
    
end subroutine gensh3
    