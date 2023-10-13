subroutine iclear(ia,m)
    !-ZACE 2005.08
    !
    !subroutine to clear an integer array
    !
    dimension ia(*)
    
    do 100 i=1,m
        ia(i) = 0
100 continue
    
    return
    
end subroutine iclear