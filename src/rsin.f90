subroutine rsin(d,v,a,ndof,numnp,ntstep,time)
    !
    !.... program to read restart file
    !
    implicit double precision (a-h,o-z)

    !.... deactivate above card(s) for single precision operation
    dimension d(ndof,1),v(ndof,1),a(ndof,1)
    common /iounit/ iin,iout,irsin,irsout
    
    read(irsin,1000) ntstep,time,ijunk
    
    do 100 j=1,numnp
        read(irsin,2000) (d(i,j),v(i,j),a(i,j),i=1,ndof)
100 continue
    
    return
    
1000 format(//,15x,i5,/,15x,e12.5/,i1)
2000 format(3(6e16.8/ ))

end subroutine rsin

