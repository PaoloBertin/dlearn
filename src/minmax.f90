subroutine minmax(x,xmax,xmin,l,m,n)
    !
    !.... program to compute the min and max in the row of a matrix
    !
    !        x = matrix
    !        l = number of rows in x
    !        m = number of columns in x
    !        n = row number
    !
    dimension x(l,1)
    
    xmax = x(n,1)
    xmin = x(n,1)
    
    do 100 i = 2,m
        if (x(n,i).gt.xmax) xmax = x(n,i)
        if (x(n,i).lt.xmin) xmin = x(n,i)
100 continue
    
    return
    
end subroutine minmax
    