subroutine prop2d(rho,rdampm,rdampk,th,c,numat,iopt,nrowb)
    !
    !.... program to read, write and store properties for two-dimensional
    !        continuum elements
    !
    !        note: this routine is presently restricted to the
    !              isotropic linearly-elastic case
    !
    !              iopt = 0; plane stress
    !                   = 1; plane strain
    !                   = 2; torsionless axisymmetric
    !
    implicit double precision (a-h,o-z)

    !.... deactivate above card(s) for single precision operation
    dimension rho(1),rdampm(1),rdampk(1),th(1),c(nrowb,nrowb,1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /iounit/ iin,iout,irsin,irsout

    do 100 n=1,numat
        if (mod(n,50).eq.1) write(iout,1000) numat
        read(iin,2000) m,e,pois,rho(m),rdampm(m),rdampk(m),th(m)
        if (th(m).eq.zero) th(m) = one
        write(iout,3000) m,e,pois,rho(m),rdampm(m),rdampk(m),th(m)
        
        !.... set material constants for out-of-plane components
        amu2 = e/(one + pois)
        alam = amu2*pois/(one - two*pois)
        
        c(1,4,m) = alam
        c(2,4,m) = alam
        c(3,4,m) = zero
        c(4,4,m) = alam + amu2
        
        c(4,1,m) = c(1,4,m)
        c(4,2,m) = c(2,4,m)
        c(4,3,m) = c(3,4,m)
        
        !.... set material constants for in-plane components
        if (iopt.eq.0) alam = alam*amu2/(alam + amu2)
    
        c(1,1,m) = alam + amu2
        c(1,2,m) = alam
        c(2,2,m) = c(1,1,m)
        c(1,3,m) = zero
        c(2,3,m) = zero
        c(3,3,m) = pt5*amu2
        
        c(2,1,m) = c(1,2,m)
        c(3,1,m) = c(1,3,m)
        c(3,2,m) = c(2,3,m)
        
100 continue
    
    return
    
1000 format('1',                                                                                                                   &
    &' m a t e r i a l   s e t   d a t a                      '   //5x,                                                            &
    &' number of material sets . . . . . . . . . . (numat ) = ',i5///,                                                             &
    & 7x,'set',5x,'young''s',4x,'poisson''s',5x,'mass',8x,'mass',                                                                  &
    & 6x,'stiffness',3x,'thickness',/6x,'number',3x,'modulus',                                                                     &
    & 6x,'ratio',6x,'density',5x,'damping',5x,'damping',/ )
2000 format(i5,5x,7f10.0)
3000 format(4x,i5,3x,6(2x,1pe10.4))

end subroutine prop2d

