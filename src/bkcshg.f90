subroutine bkcshg(xl,det,shl,shg,nint,nel,neg)
    !
    !.... subroutine to calculate global derivatives of shape functions and
    !     jacobian determinants for a eight-node brick element
    !
    !     xl(j,i)    = global coordinates
    !     det(l)     = jacobian determinant
    !     shl(1,i,l) = local ("xi") derivative of shape function
    !     shl(2,i,l) = local ("eta") derivative of shape function
    !     shl(3,i,l) = local ("zeta") derivative of shape function
    !     shl(4,i,l) = local  shape function
    !     shg(1,i,l) = x-derivative of shape function
    !     shg(2,i,l) = y-derivative of shape function
    !     shg(3,i,l) = z-derivative of shape function
    !     shg(4,i,l) = shl(4,i,l)
    !     xs(i,j)    = jacobian matrix
    !              i = local node number or global coordinate number
    !              j = global coordinate number
    !              l = integration-point number
    !           nint = number of integration points, eq. 1 or 8
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single precision operation
    !
    dimension xl(3,1),det(1),shl(4,8,1),shg(4,8,1),xs(3,3),xsj(3,3)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /iounit/ iin,iout,irsin,irsout
    
    call move(shg,shl,32*nint)
    
    do 700 l=1,nint
    
        do 300 j=1,3
            do 200 i=1,3
                xs(i,j) = rowdot(shg(i,1,l),xl(j,1),4,3,8)
200         continue
300     continue
    
        det(l) = xs(1,1)*(xs(2,2)*xs(3,3)-xs(2,3)*xs(3,2))                                                                         &
                +xs(1,2)*(xs(2,3)*xs(3,1)-xs(2,1)*xs(3,3))                                                                         &
                +xs(1,3)*(xs(2,1)*xs(3,2)-xs(2,2)*xs(3,1))
        if (det(l).le.zero) then
            write(iout,1000) nel,neg
            stop
        endif
    
    !   inversion
        xsj(1,1) = (xs(2,2)*xs(3,3)-xs(2,3)*xs(3,2))/det(l)
        xsj(1,2) = (xs(2,3)*xs(3,1)-xs(2,1)*xs(3,3))/det(l)
        xsj(1,3) = (xs(2,1)*xs(3,2)-xs(2,2)*xs(3,1))/det(l)
        xsj(2,1) = (xs(1,3)*xs(3,2)-xs(1,2)*xs(3,3))/det(l)
        xsj(2,2) = (xs(1,1)*xs(3,3)-xs(1,3)*xs(3,1))/det(l)
        xsj(2,3) = (xs(1,2)*xs(3,1)-xs(1,1)*xs(3,2))/det(l)
        xsj(3,1) = (xs(1,2)*xs(2,3)-xs(1,3)*xs(2,2))/det(l)
        xsj(3,2) = (xs(1,3)*xs(2,1)-xs(1,1)*xs(2,3))/det(l)
        xsj(3,3) = (xs(1,1)*xs(2,2)-xs(1,2)*xs(2,1))/det(l)
    
        do 600 i=1,8
            temp1=xsj(1,1)*shg(1,i,l)+xsj(2,1)*shg(2,i,l)+xsj(3,1)*shg(3,i,l)
            temp2=xsj(1,2)*shg(1,i,l)+xsj(2,2)*shg(2,i,l)+xsj(3,2)*shg(3,i,l)
            shg(3,i,l) = xsj(1,3)*shg(1,i,l)+xsj(2,3)*shg(2,i,l)+xsj(3,3)*shg(3,i,l)
            shg(1,i,l) = temp1
            shg(2,i,l) = temp2
600     continue
    
700 continue
    
    return
    
1000 format('1','non-positive determinant in element number  ',i5, ' in element group  ',i5)
          end
    