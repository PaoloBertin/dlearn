subroutine rsout(d,v,a,ndof,numnp,ntstep,time)
    !
    !.... program to write restart file
    !
    double precision a,d,v,time
    !
    !.... deactivate above card(s)s for single precision operation
    !
    character*4 title
    dimension d(ndof,1),v(ndof,1),a(ndof,1)
    common /iounit/ iin,iout,irsin,irsout
    common /titlec/ title(20)
    !
    write(irsout,1000) title,ntstep,time
    !
    do 100 j=1,numnp
        write(irsout,2000) (d(i,j),v(i,j),a(i,j),i=1,ndof)
100 continue
    !
    return
    !
1000 format(' ',20a4//,' step number = ',i5/,                                                                                      &
                       ' time        = ',1pe12.5/' ')
2000 format(3(6e16.8/ ))

end subroutine rsout

