subroutine interp(x,y,xx,yy,n)
    !
    !.... program to perform linear interpolation
    !
    !        x(i) = abscissas
    !        y(i) = ordinates
    !          xx = input abscissa
    !          yy = output ordinate
    !           n = total number of data points (1.le.i.le.n)
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single-precision operation
    dimension x(1),y(1)
    
    if (xx.le.x(1)) then
        yy = y(1)
    
    else if (xx.ge.x(n)) then
        yy = y(n)
    
    else
        do 100 i=1,n
            if (x(i).ge.xx) then
                yy = y(i-1) + (xx - x(i-1))*(y(i) - y(i-1))/(x(i) - x(i-1))
                return
            endif
100     continue
    
    endif
    
    return
    
end subroutine interp
    