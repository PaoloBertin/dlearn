subroutine qdcshg(xl,det,shl,shg,nint,nel,neg,lquad)
    !
    !.... program to calculate global derivatives of shape functions and
    !        jacobian determinants for a four-node quadrilateral element
    !
    !        xl(j,i)    = global coordinates
    !        det(l)     = jacobian determinant
    !        shl(1,i,l) = local ("xi") derivative of shape function
    !        shl(2,i,l) = local ("eta") derivative of shape function
    !        shl(3,i,l) = local  shape function
    !        shg(1,i,l) = x-derivative of shape function
    !        shg(2,i,l) = y-derivative of shape function
    !        shg(3,i,l) = shl(3,i,l)
    !        xs(i,j)    = jacobian matrix
    !                 i = local node number or global coordinate number
    !                 j = global coordinate number
    !                 l = integration-point number
    !              nint = number of integration points, eq. 1 or 4
    !
    implicit double precision (a-h,o-z)
    !.... deactivate above card(s) for single precision operation
    
    logical lquad
    dimension xl(2,1),det(1),shl(3,4,1),shg(3,4,1),xs(2,2)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /iounit/ iin,iout,irsin,irsout
    
    call move(shg,shl,12*nint)
    
    do 700 l=1,nint
        
        if (.not.lquad) then
            do 100 i=1,3
                shg(i,3,l) = shl(i,3,l) + shl(i,4,l)
                shg(i,4,l) = zero
100         continue
        endif
        
        do 300 j=1,2
            do 200 i=1,2
                xs(i,j) = rowdot(shg(i,1,l),xl(j,1),3,2,4)
200         continue
300     continue
        
        det(l) = xs(1,1)*xs(2,2)-xs(1,2)*xs(2,1)
        if (det(l).le.zero) then
            write(iout,1000) nel,neg
            stop
        endif
        
        do 500 j=1,2
            do 400 i=1,2
                xs(i,j) = xs(i,j)/det(l)
400         continue
500     continue
        
        do 600 i=1,4
            temp = xs(2,2)*shg(1,i,l) - xs(1,2)*shg(2,i,l)
            shg(2,i,l) = - xs(2,1)*shg(1,i,l) + xs(1,1)*shg(2,i,l)
            shg(1,i,l) = temp
600     continue
        
700 continue
    
    return
    
1000 format('1','non-positive determinant in element number  ',i5,' in element group  ',i5)

end subroutine qdcshg

