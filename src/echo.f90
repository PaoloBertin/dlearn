subroutine echo 
    !
    !.... program to echo input data
    !
    dimension ia(20)
    common /iounit/ iin,iout,irsin,irsout
    
    read(iin,1000) iecho

    if (iecho.eq.0) return
    write(iout,2000) iecho
    backspace iin
    
    do 100 i=1,100000
        read(iin,3000,end=200) ia
        if (mod(i,50).eq.1.and.i.gt.1) write(iout,4000)
        write(iout,5000) ia
100 continue

200 continue

    rewind iin
    read(iin,1000) iecho

    return
    
1000 format(16i5)

2000 format('1',' i n p u t   d a t a   f i l e               ',  //5x,                                                            &
            ' echo print code . . . . . . . . . . . . . . (iecho ) = ',i5//5x,                                                     &
            '    eq. 0, no echo of input data                        ',   /5x,                                                     &
            '    eq. 1, echo input data                              ',   ///,                                                     &
          8('123456789*'),//)

3000 format(20a4)
4000 format('1',8('123456789*'),//)
5000 format(' ',20a4)

end subroutine echo
    