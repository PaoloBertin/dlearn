subroutine gensh1(r,sh,n)
    !
    ! subroutine to compute 1d shape functions
    ! for isoparametric generation
    !
    implicit double precision (a-h,o-z)
    
    ! modify above card(s) for single-precision operation
    dimension sh(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    sh(2) = pt5*r
    sh(1) = pt5 - sh(2)
    sh(2) = pt5 + sh(2)
    if (n.eq.3) then
        sh(3) = one - r*r
        sh(1) = sh(1) - pt5*sh(3)
        sh(2) = sh(2) - pt5*sh(3)
    endif
    
    return
    
end subroutine gensh1