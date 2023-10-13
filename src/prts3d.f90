subroutine prts3d(xint,stress,pstrs,strain,pstrn,nn,nntot,neg,nel,lint)
!
!.... subroutine to print stress, strain, and principal values
!     for three-dimensional continuum elements
!
    implicit double precision (a-h,o-z)

    ! deactivate above card(s) for single precision operation
    dimension xint(3),stress(6),pstrs(3),strain(6),pstrn(3)
    common /iounit/ iin,iout,irsin,irsout
    
    nn = nn+1
    if (mod(nn,nntot).eq.1) write(iout,1000) neg
    write(iout,2000) nel,lint,xint,stress,pstrs,strain,pstrn

    return

1000 format('1',                                                                                                                   &
     ' e l e m e n t  s t r e s s e s   a n d   s t r a i n s ',  //5x,                                                            &
     ' element group number  . . . . . . . . . . . (neg   ) = ',i5///                                                              &
     '  element  int. pt.         x1        x2        x3    ',5x,                                                                  &
     '  stress    stress    stress    stress  ',                                                                                   &
     '  stress    stress  ',/,                                                                                                     &
     '   number   number                          ',15x,                                                                           &
     '    11        22        33        12        23        31',/,59x,                                                             &
     ' stress 1  stress 2   stress 3  ',//,59x,                                                                                    &
     '  strain    strain    strain    strain  ',                                                                                   &
     '  strain    strain  ',/,59x,                                                                                                 &
     '    11        22        33        12        23        31',/,59x,                                                             &
     ' strain 1  strain 2   strain 3  ')
2000 format( /2x,i5,6x,i2,8x,3(1pe10.2),5x,6(1pe10.2)/58x,3(1pe10.2),                                                              &
            /58x,6(1pe10.2),/58x,3(1pe10.2))
     end
