subroutine compbc(id,d,v,a,dpred,vpred,f,g1,ndof,numnp,nlvect,ldyn)
!
!.... program to compute displacement, velocity and
!        acceleration boundary conditions
!
     implicit double precision (a-h,o-z)
!
!.... deactivate above card(s) for single-precision operation
!
    logical ldyn
    dimension id(ndof,1),d(ndof,1),v(ndof,1),a(ndof,1),dpred(ndof,1),vpred(ndof,1),f(ndof,numnp,1),g1(1)

    common /coeffs/ coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,coeff7,coeff8,alpha1,beta1 ,gamma1,dt1
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five

    do 700 i=1,ndof

        do 600 j=1,numnp

            k = id(i,j)
            if (k.gt.0) go to 500
            val = zero
            do 100 lv=1,nlvect
                val = val + f(i,j,lv)*g1(lv)
100         continue

            m = 1 - k
            go to (200,300,400),m

200         continue
            if (ldyn) then
                temp = coeff1*val - alpha1*d(i,j)
                a(i,j) = (temp - dpred(i,j))/coeff5
                dpred(i,j) = temp
                vpred(i,j) = vpred(i,j) + coeff4*a(i,j)
            else
                d(i,j) = val
            endif
            go to 500

300         temp = coeff1*val - alpha1*v(i,j)
            a(i,j) = (temp - vpred(i,j))/coeff4
            vpred(i,j) = temp
            dpred(i,j) = dpred(i,j) + coeff5*a(i,j)
            go to 500

400         dpred(i,j) = dpred(i,j) + coeff5*val
            vpred(i,j) = vpred(i,j) + coeff4*val
            a(i,j) = val

500         continue

600     continue
700 continue

    return

end subroutine compbc
