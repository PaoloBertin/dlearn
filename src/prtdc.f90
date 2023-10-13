subroutine prtdc
    !
    !.... program to print memory-pointer dictionary
    !
    common /bpoint/ mfirst,mlast,mtot,iprec
    common /iounit/ iin,iout,irsin,irsout
    !-ZACE-2005.08
    include 'memory_size.inc'
    common ia(max_size)
    
    n = (mtot-mlast)/7
    j = mtot + 1
    
    do 100 i=1,n
        if (mod(i,50).eq.1) write(iout,1000)
        j = j - 7
        call prtdc1(i,ia(j),ia(j+2),ia(j+3),ia(j+4),ia(j+5),ia(j+6))
100 continue
    
    return
    
1000 format('1',                                                                                                                   &
    &' d y n a m i c   s t o r a g e    a l l o c a t i o n',                                                                      &
    &'   i n f o r m a t i o n '//                                                                                                 &
    &  12x,'array no.',5x,'array',8x,'address',6x,'dim1',6x,'dim2',                                                                &
    &  6x, 'dim3',6x,'prec.'/ )
    
end subroutine prtdc

