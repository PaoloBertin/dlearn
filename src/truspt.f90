subroutine truspt(xint,stress,force,strain,nn,nntot,neg,nel,lint)
    !
    !.... program to print stress, strain and force for the
    !        three-dimensional, elastic truss element
    !
    implicit double precision (a-h,o-z)
    !.... deactivate above card(s) for single-precision operation

    dimension xint(3)
    common /iounit/ iin,iout,irsin,irsout

    nn = nn + 1

    if (mod(nn,nntot).eq.1) then
        write(iout,1000) neg
        nn = 1
    endif

    write(iout,2000) nel,lint,xint,stress,force,strain

    return

1000 format('1',                                                                                                                   &
        ' e l e m e n t   s t r e s s e s   a n d   s t r a i n s',  //5x,                                                         &
        ' element group number  . . . . . . . . . . . (neg   ) = ',i5///,                                                          &
        '  element  int. pt.          x1        x2        x3    ',5x,                                                              &
        '  stress    force     strain  ',/                                                                                         &
        '   number   number         coord.    coord.    coord.  ')

2000 format( /2x,i5,7x,i2,8x,3(1pe10.2),5x,3(1pe10.2))

end

