subroutine meansh(shgbar,w,det,r,shg,nen,nint,iopt,nesd,nrowsh)
    !
    !.... program to calculate mean values of shape function
    !        global derivatives for b-bar method
    !
    !        note: if iopt.eq.2, det(l) = det(l)*r(l) upon entry
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    dimension shgbar(3,1),w(1),det(1),r(1),shg(nrowsh,nen,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    call clear(shgbar,3*nen)
    
    volinv = one/coldot(w,det,nint)
    
    do 300 l=1,nint
        temp1 = w(l)*det(l)*volinv
        if (iopt.eq.2) temp2 = temp1/r(l)
    
        do 200 j=1,nen
    
            do 100 i=1,nesd
                shgbar(i,j) = shgbar(i,j) + temp1*shg(i,j,l)
100         continue
    
            if (iopt.eq.2) shgbar(3,j) = shgbar(3,j) + temp2*shg(3,j,l)
200     continue
    
300 continue
    
    return

end subroutine meansh
    