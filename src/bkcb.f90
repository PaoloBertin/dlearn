subroutine bkcb(shg,shgbar,b,nrowsh,nrowb,nen,ibbar)
    !
    !.... subroutine to set up the strain-displacement matrix "b" for
    !     three-dimensional continuum elements
    !
    !     ibbar = 0, standard b-matrix
    !
    !     ibbar = 1, mean-dilatational b-matrix
    !
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single-precision operation
    dimension shg(nrowsh,1),shgbar(nrowsh,1),b(nrowb,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    !
    do 100 j=1,nen
    
        j3   = 3*j
        j3m1 = j3 - 1
        j3m2 = j3 - 2
    
        b(1,j3m2) = shg(1,j)
        b(1,j3m1) = zero
        b(1,j3  ) = zero
        b(2,j3m2) = zero
        b(2,j3m1) = shg(2,j)
        b(2,j3  ) = zero
        b(3,j3m2) = zero
        b(3,j3m1) = zero
        b(3,j3  ) = shg(3,j)
        b(4,j3m2) = shg(2,j)
        b(4,j3m1) = shg(1,j)
        b(4,j3  ) = zero
        b(5,j3m2) = zero
        b(5,j3m1) = shg(3,j)
        b(5,j3  ) = shg(2,j)
        b(6,j3m2) = shg(3,j)
        b(6,j3m1) = zero
        b(6,j3  ) = shg(1,j)
    
100 continue
    
    if (ibbar.eq.0) return
    
        ! add contributions to form b-bar
    constb = one/three
    
    do 200 j=1,nen
    
        j3   = 3*j
        j3m1 = j3 - 1
        j3m2 = j3 - 2
    
        temp1 = constb*(shgbar(1,j) - shg(1,j))
        temp2 = constb*(shgbar(2,j) - shg(2,j))
        temp3 = constb*(shgbar(3,j) - shg(3,j))
    
        b(1,j3m2) = b(1,j3m2) + temp1
        b(1,j3m1) = b(1,j3m1) + temp2
        b(1,j3  ) = b(1,j3  ) + temp3
        b(2,j3m2) = b(2,j3m2) + temp1
        b(2,j3m1) = b(2,j3m1) + temp2
        b(2,j3  ) = b(2,j3  ) + temp3
        b(3,j3m2) = b(3,j3m2) + temp1
        b(3,j3m1) = b(3,j3m1) + temp2
        b(3,j3  ) = b(3,j3  ) + temp3
    
200 continue
        
    return

end subroutine bkcb
    