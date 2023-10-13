subroutine smult(a,b,c,mb,mc,m,n,iopt)
    !
    !.... program to perform scalar multiplication of a matrix
    !
    !        c(i,j) = a*b(i,j)
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    dimension b(mb,1),c(mc,1)
    
    go to (1000,2000,3000),iopt
    
    ! iopt = 1, multiply entire matrix
1000 do 1200 j=1,n
        
        do 1100 i=1,m
            c(i,j) = a*b(i,j)
1100    continue
        
1200 continue
    return
    
    !.... iopt = 2, multiply lower triangular and diagonal elements
2000 do 2200 j=1,n
        
        do 2100 i=j,m
            c(i,j) = a*b(i,j)
2100    continue
        
2200 continue
    return
    
    !.... iopt = 3, multiply upper triangular and diagonal elements
3000 do 3200 j=1,n
        
        do 3100 i=1,j
            c(i,j) = a*b(i,j)
3100    continue
        
3200 continue
    
    return
    
end subroutine smult

