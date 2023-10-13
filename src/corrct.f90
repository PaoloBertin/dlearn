subroutine corrct(d,v,dpred,vpred,ndof,numnp)
    !
    !.... program to perform corrector update of displacements
    !        and velocities
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single-precision operation
    dimension d(ndof,1),v(ndof,1),dpred(ndof,1),vpred(ndof,1)
    common /coeffs/ coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,coeff7,coeff8,alpha1,beta1 ,gamma1,dt
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    temp = one/coeff1
    
    do 200 i=1,ndof
        do 100 j=1,numnp
            dn = d(i,j)
            vn = v(i,j)
            d(i,j) = (dpred(i,j) - dn)*temp + dn
            v(i,j) = (vpred(i,j) - vn)*temp + vn
100     continue

200 continue
    
    return

end subroutine corrct
    