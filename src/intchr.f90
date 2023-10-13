subroutine intchr (ia,a1)
    !
    ! ... converts integer*4 to char*4
    !
    
    character*4 a1
    
    ival=ia
    
    do 10 i=4,1,-1
        ich=mod(ival,256)
        a1(i:i)=char(ich)
        ival=int( (ival-ich)/256 )
10  continue
    
    return
    
end subroutine intchr

