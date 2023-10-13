subroutine predct(d,v,a,dpred,vpred,ndof,numnp)
    !
    !.... program to calculate predictor for displacements,velocities
    !        and accelerations
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single precision operation
    dimension d(ndof,1),v(ndof,1),a(ndof,1),dpred(ndof,1),vpred(ndof,1)
    common /coeffs/ coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,coeff7,coeff8,alpha1,beta1 ,gamma1,dt1
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five

    do 200 i=1,ndof

        do 100 j=1,numnp
            dpred(i,j) = d(i,j) + coeff6*v(i,j) + coeff7*a(i,j)
            vpred(i,j) = v(i,j) + coeff8*a(i,j)
            a(i,j) = zero
100     continue

200 continue

    return

end subroutine predct
