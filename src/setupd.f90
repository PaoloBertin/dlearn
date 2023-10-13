subroutine setupd(c,dmat,const,nstr,nrowb)
    !
    !.... program to calculate the d matrix
    !
    implicit double precision (a-h,o-z)

    !.... deactivate above card(s) for single precision operation
    dimension c(nrowb,1),dmat(nrowb,1)
    
    do 200 j=1,nstr
    
        do 100 i=1,j
            dmat(i,j) = const*c(i,j)
            dmat(j,i) = dmat(i,j)
100     continue
        
200 continue
    
    return

end subroutine setupd

