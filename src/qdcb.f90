subroutine qdcb(shg,shgbar,b,r,iopt,nrowsh,nrowb,nen,ibbar)
    !
    !.... program to set up the strain-displacement matrix "b" for
    !        two-dimensional continuum elements
    !
    !        ibbar = 0, standard b-matrix
    !
    !        ibbar = 1, mean-dilatational b-matrix
    !
    
    implicit double precision (a-h,o-z)
    !.... deactivate above card(s) for single-precision operation
    
    dimension shg(nrowsh,1),shgbar(3,1),b(nrowb,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    do 100 j=1,nen
        
        j2   = 2*j
        j2m1 = j2 - 1
        
        b(1,j2m1) = shg(1,j)
        b(1,j2  ) = zero
        b(2,j2m1) = zero
        b(2,j2  ) = shg(2,j)
        b(3,j2m1) = shg(2,j)
        b(3,j2  ) = shg(1,j)
        
        if (iopt.eq.2) then
            b(4,j2m1) = shg(3,j)/r
            b(4,j2  ) = zero
        endif
        
100 continue
    
    if (ibbar.eq.0) return

    !.... add contributions to form b-bar
    constb = one/three
    do 200 j=1,nen
        
        j2   = 2*j
        j2m1 = j2 - 1
        
        if (iopt.eq.2) then
            temp3 = constb*(shgbar(3,j) - shg(3,j)/r)
            b(1,j2m1) = b(1,j2m1) + temp3
            b(2,j2m1) = b(2,j2m1) + temp3
            b(4,j2m1) = b(4,j2m1) + temp3
        else
            b(4,j2m1) = zero
            b(4,j2  ) = zero
        endif
        
        temp1 = constb*(shgbar(1,j) - shg(1,j))
        temp2 = constb*(shgbar(2,j) - shg(2,j))
        
        b(1,j2m1) = b(1,j2m1) + temp1
        b(1,j2  ) = b(1,j2  ) + temp2
        b(2,j2m1) = b(2,j2m1) + temp1
        b(2,j2  ) = b(2,j2  ) + temp2
        b(4,j2m1) = b(4,j2m1) + temp1
        b(4,j2  ) = b(4,j2  ) + temp2
        
200 continue
    
    return
    
end subroutine qdcb

