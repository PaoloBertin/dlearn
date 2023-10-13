subroutine trusb(b,shg,xs,nen,nesd,nrowb,nrowsh)
    !
    !.... program to set up the strain-displacement matrix for the
    !        three-dimensional, elastic truss element
    
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    dimension b(nrowb,1),shg(nrowsh,1),xs(1)
    
    do 200 j=1,nen
        k = (j - 1)*nesd
        
        do 100 i=1,nesd
            b(1,k+i) = shg(1,j)*xs(i)
100     continue
        
200 continue
    
    return
    
end subroutine trusb
