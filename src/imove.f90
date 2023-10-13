subroutine imove(ia,ib,n)
    !
    ! subroutine to move an integer array
    !
    dimension ia(*),ib(*)
    
    do 100 i=1,n
        ia(i)=ib(i)
100 continue
    
    return
    
end subroutine imove
    