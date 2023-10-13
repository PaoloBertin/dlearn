subroutine prts2d(xint,stress,pstrs,strain,pstrn,nn,nntot,neg,nel,lint)
    !
    !.... program to print stress, strain, and principal values
    !        for two-dimensional continuum elements
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single precision operation
    !
    dimension xint(2),stress(4),pstrs(4),strain(4),pstrn(4)
    common /iounit/ iin,iout,irsin,irsout
    !
    nn = nn+1
    if (mod(nn,nntot).eq.1) write(iout,1000) neg
    write(iout,2000) nel,lint,xint,stress,pstrs,strain,pstrn
    !
    return
    !
1000 format('1',                                                                                                                   &
     ' e l e m e n t  s t r e s s e s   a n d   s t r a i n s ',  //5x,                                                            &
     ' element group number  . . . . . . . . . . . (neg   ) = ',i5///                                                              &   
     '  element  int. pt.         x1        x2    ',5x,                                                                            &
     '  stress    stress    stress    stress  ',                                                                                   &
     '  princ.    princ.    shear     stress  ',  /,                                                                               &
     '   number   number                          ',5x,                                                                            &
     '    11        22        12        33    ',                                                                                   &
     ' stress 1  stress 2   stress     angle  ',//,49x,                                                                            &
     '  strain    strain    strain    strain  ',                                                                                   &
     '  princ.    princ.    shear     strain  ', /,49x,                                                                            &
     '    11        22        12        33    ',                                                                                   &
     ' strain 1  strain 2   strain     angle  ')
2000 format( /2x,i5,6x,i2,8x,2(1pe10.2),5x,8(1pe10.2)/48x,8(1pe10.2))

end subroutine prts2d
