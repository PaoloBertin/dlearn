subroutine coord(x,nsd,numnp,iprtin)
    !
    ! subroutine to read, generate and write coordinate data
    !
    ! x(nsd,numnp) = coordinate array
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single-precision operation
    !
    dimension x(nsd,1)
    common /iounit/ iin,iout,irsin,irsout
    
    call genfl(x,nsd)
    
    if (iprtin.eq.1) return
    
    do 100 n=1,numnp
        if (mod(n,50).eq.1) write(iout,1000) (i,i=1,nsd)
        write(iout,2000) n,(x(i,n),i=1,nsd)
100 continue

    return
    
1000 format('1',' n o d a l   c o o r d i n a t e   d a t a '///5x,' node no.',3(13x,' x',i1,' ',:)//)
2000 format(6x,i5,10x,3(1pe15.8,2x))

end subroutine coord
    