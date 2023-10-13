subroutine geneli(ien2,ien1,inc,nen)
    !
    ! subroutine to increment element node numbers
    !
    dimension ien1(1),ien2(1)
    
    do 100 i=1,nen
        if (ien1(i).eq.0) then
            ien2(i) = 0
        else
            ien2(i) = ien1(i) + inc
        endif
100 continue
    
    return

end subroutine geneli
    