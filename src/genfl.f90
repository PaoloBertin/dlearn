subroutine genfl(a,nra)
    !
    ! subroutine to read and generate floating-point nodal data
    !
    ! a       = input array
    ! nra     = number of rows in a (le.6)
    ! n       = node number
    ! numgp   = number of generation points
    ! ninc(i) = number of increments for direction i
    ! inc(i)  = increment for direction i
    !
    implicit double precision (a-h,o-z)

    ! deactivate above card(s) for single-precision operation
    dimension a(nra,1)
    common /iounit/ iin,iout,irsin,irsout
    common /genflc/ temp(6,20),n,numgp,ninc(3),inc(3)

100 continue
    read(iin,1000) n,numgp,(temp(i,1),i=1,nra)
    if (n.eq.0) return
    call move(a(1,n),temp,nra)
    if (numgp.ne.0) then
        do 200 j=2,numgp
            read(iin,1000) m,mgen,(temp(i,j),i=1,nra)
            if (mgen.ne.0) call move(temp(1,j),a(1,m),nra)
200     continue
        read(iin,2000) (ninc(i),inc(i),i=1,3)
        call genfl1(a,nra)
    endif
    go to 100

1000 format(2i5,6f10.0)
2000 format(16i5)

end subroutine genfl

