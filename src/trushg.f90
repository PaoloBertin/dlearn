subroutine trushg(xl,det,shl,shg,xs,nen,nint,nel,neg,lnode3)
    !
    !.... program to calculate global derivatives of shape functions
    !        and jacobian determinants for the three-dimensional,
    !        elastic truss element
    !
    !           xl(j,l) = global coordinates of nodal points
    !        shl(1,i,l) = local ("xi") derivative of shape function
    !        shl(2,i,l) = shape function
    !        shg(1,i,l) = global ("arc-length") derivative of shape ftn
    !        shg(2,i,l) = shl(2,i,l)
    !           xs(j,l) = jth component of the local derivative
    !                        of the position vector; then scaled to
    !                        direction cosine
    !            det(l) = euclidean length of xs
    !                 i = local node number
    !                 j = global coordinate number
    !                 l = integration-point number
    !              nint = number of integration points
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    logical lnode3
    dimension xl(3,1),det(1),shl(2,nen,1),shg(2,nen,1),xs(3,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /iounit/ iin,iout,irsin,irsout
    
    call move(shg,shl,2*nen*nint)
    
    do 400 l=1,nint
        if (.not.lnode3) then
            temp = pt5*shg(1,3,l)
            shg(1,1,l) = shg(1,1,l) + temp
            shg(1,2,l) = shg(1,2,l) + temp
            temp = pt5*shg(2,3,l)
            shg(2,1,l) = shg(2,1,l) + temp
            shg(2,2,l) = shg(2,2,l) + temp
        endif
        det(l) = zero
        
        do 100 j=1,3
            xs(j,l) = rowdot(shl(1,1,l),xl(j,1),2,3,nen)
            det(l) = det(l) + xs(j,l)**2
100     continue
        
        det(l) = sqrt(det(l))
        
        if (det(l).le.zero) then
            write(iout,1000) nel,neg
            stop
        endif
        
        do 200 j=1,3
            xs(j,l) = xs(j,l)/det(l)
200     continue
        
        do 300 i=1,nen
            shg(1,i,l) = shl(1,i,l)/det(l)
300     continue
        
400 continue
    
    return
    
1000 format('1','non-positive determinant in element number  ',i5, ' in element group  ',i5)

end subroutine trushg

