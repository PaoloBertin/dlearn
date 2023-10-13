subroutine printd(name,dva,ndof,numnp,ntstep,time)
    !
    !.... program to print kinematic data
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single precision operation
    logical lzero,lskip
    ! ZACE correction 21.05.1997
    character*(*) name
    dimension dva(ndof,1) ! ,name(11)
    common /iounit/ iin,iout,irsin,irsout
    
    nn = 0
    lskip = .true.
    
    do 100 n=1,numnp
        call ztest(dva(1,n),ndof,lzero)
        if (.not.lzero) then
            nn = nn + 1
            if (mod(nn,50).eq.1) write(iout,1000) name,ntstep,time,(i,i=1,ndof)
            write(iout,2000) n,(dva(i,n),i=1,ndof)
            lskip = .false.
        endif
100 continue
    
    if (lskip) then
        write(iout,1000) name,ntstep,time,(i,i=1,ndof)
        write(iout,3000)
    endif
    
    return
    
1000 format('1',a44//5x,                                                                   & !ZACE correction 21.05.1997   11a4//5x,                                                                   
            ' step number = ',i10//5x,                                                                                             &
            ' time        = ',1pe10.3///5x,                                                                                        &
            ' node no.',6(13x,'dof',i1,:)/ )
2000 format(6x,i5,10x,6(1pe15.8,2x))
3000 format(' ',//,' there are no nonzero components')

end subroutine printd
    