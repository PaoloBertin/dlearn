subroutine multab(a,b,c,ma,mb,mc,l,m,n,iopt)
    !
    !.... program to multiply two matrices
    !
    !        l = range of dot-product index
    !        m = number of active rows in c
    !        n = number of active columns in c
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    dimension a(ma,1),b(mb,1),c(mc,1)
    
    go to (1000,2000,3000,4000),iopt
    
    !.... iopt = 1, c(i,j) = a(i,k)*b(k,j) , (c = a * b)
1000 do 1200 i=1,m
    
        do 1100 j=1,n
            c(i,j) = rcdot(a(i,1),b(1,j),ma,l)
1100    continue
    
1200 continue
    return

    !                                        t
    ! iopt = 2, c(i,j) = a(k,i)*b(k,j) (c = a  * b)
2000 do 2200 i=1,m
    
        do 2100 j=1,n
            c(i,j) = coldot(a(1,i),b(1,j),l)
2100    continue
    
2200 continue
    return

    !                                            t
    !.... iopt = 3, c(i,j) = a(i,k)*b(j,k) (c = a * b )
3000 do 3200 i=1,m
    
        do 3100 j=1,n
            c(i,j) = rowdot(a(i,1),b(j,1),ma,mb,l)
3100    continue
    
3200 continue
    return

    !                                            t    t
    !.... iopt = 4, c(i,j) = a(k,i)*b(j,k) (c = a  * b )
4000 do 4200 i=1,m
    
        do 4100 j=1,n
            c(i,j) = rcdot(b(j,1),a(1,i),mb,l)
4100    continue
    
4200 continue
    
    return
end subroutine multab
    