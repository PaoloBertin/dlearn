subroutine sufdet(xl,shl,nel,neg,if,xs,det)
!
! subroutine to calculate jacobian and his determinant
! for a four-node quadrilateral surface
!
!        xl(j,i)    = global coordinates
!        det(l)     = jacobian determinant
!        shl(1,i,l) = local ("xi") derivative of shape function
!        shl(2,i,l) = local ("eta") derivative of shape function
!        shl(3,i,l) = local  shape function
!        xs(i,j)    = jacobian matrix
!                 i = local node number or global coordinate number
!                 j = global coordinate number
!                 l = integration-point number
!              nint = number of integration points, eq. 1 or 4
!
    implicit double precision (a-h,o-z)
!
!.... deactivate above card(s) for single precision operation
!
    dimension xl(3,1),shl(3,1),xs(3,3)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /iounit/ iin,iout,irsin,irsout

    do 300 j=1,3
        do 200 i=1,2
            xs(i,j) = rowdot(shl(i,1),xl(j,1),3,3,4)
        200 continue
300 continue

    ! third direction
    xs(3,1)= xs(1,2)*xs(2,3)-xs(2,2)*xs(1,3)
    xs(3,2)= xs(2,1)*xs(1,3)-xs(1,1)*xs(2,3)
    xs(3,3)= xs(1,1)*xs(2,2)-xs(2,1)*xs(1,2)
    zn     = sqrt(xs(3,1)**2+xs(3,2)**2+xs(3,3)**2)
    do 400 i=1,3
        xs(3,i)= xs(3,i)/zn
400 continue

    det    = xs(1,1)*(xs(2,2)*xs(3,3)-xs(2,3)*xs(3,2))                                                                             &
            +xs(1,2)*(xs(2,3)*xs(3,1)-xs(2,1)*xs(3,3))                                                                             &
            +xs(1,3)*(xs(2,1)*xs(3,2)-xs(2,2)*xs(3,1))
    if (det.le.zero) then
        write(iout,1000) if,nel,neg
        stop
    endif

    return

1000 format('1','non-positive determinant in side  ',i5, ' in element number  ',i5, ' in element group  ',i5)

end subroutine sufdet
