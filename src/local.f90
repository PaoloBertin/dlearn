subroutine local(ien,x,xl,nen,nrowx,nrowxl)
    !
    !.... program to localize a global array
    !
    !        note: it is assumed nrowxl.le.nrowx
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    dimension ien(1),x(nrowx,1),xl(nrowxl,1)
    
    do 200 j=1,nen
          node = ien(j)
    
        do 100 i=1,nrowxl
            xl(i,j)= x(i,node)
100     continue
    
200 continue
    
    return

end subroutine local
    