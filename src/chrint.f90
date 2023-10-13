subroutine chrint (ia,a1)
    !
    ! converts char*4 to integer*4
    !
    character*4 a1

    ia=0

    do 10 i=1,4
        ia=ia*256 + ichar(a1(i:i))
10  continue

    return
    
end subroutine chrint

