subroutine truspr(rho,rdampm,rdampk,area,c,numat)
    !
    ! program to read, write and store properties for
    ! three-dimensional, elastic truss element

    implicit double precision (a-h,o-z)
    ! deactivate above card(s) for single-precision operation
    
    dimension rho(1),rdampm(1),rdampk(1),area(1),c(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /iounit/ iin,iout,irsin,irsout
    
    do 100 n=1,numat
        if (mod(n,50).eq.1) write(iout,1000) numat
        read(iin,2000) m,e,rho(m),rdampm(m),rdampk(m),area(m)
        write(iout,3000) m,e,rho(m),rdampm(m),rdampk(m),area(m)
        c(m) = e
100 continue
    
    return
    
1000 format('1',                                                                                                                   &
     ' m a t e r i a l   s e t   d a t a                      '   //5x,                                                            &
     ' number of material sets . . . . . . . . . . (numat ) = ',i5///,                                                             &
      7x,'set',5x,'young''s',6x,'mass',8x,'mass',                                                                                  &
      6x,'stiffness',6x,'area',/6x,'number',3x,'modulus',                                                                          &
      5x,'density',5x,'damping',5x,'damping',/ )

2000 format(i5,5x,7f10.0)
3000 format(4x,i5,3x,5(2x,1pe10.4))

end subroutine truspr
