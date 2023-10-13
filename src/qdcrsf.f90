subroutine qdcrsf(ielno,iside,press,shear,nsurf)
    !
    !.... program to read, write and store surface force data for the
    !        four-node quadrilateral, elastic continuum element
    !
    implicit double precision (a-h,o-z)
    !.... deactivate above card(s) for single precision operation
    
    dimension ielno(1),iside(1),press(2,1),shear(2,1)
    common /iounit/ iin,iout,irsin,irsout
    
    do 100 n=1,nsurf
        if (mod(n,50).eq.1) write(iout,1000) nsurf
        read(iin,2000) ielno(n),iside(n),press(1,n),press(2,n),shear(1,n),shear(2,n)
        write(iout,3000) ielno(n),iside(n),press(1,n),press(2,n),shear(1,n),shear(2,n)
100 continue
    
    return
    !
1000 format('1',                                                                                                                   &
    ' e l e m e n t   s u r f a c e   f o r c e   d a t a    ',  //5x,                                                             &
    ' number of surface force cards . . . . . . . (nsurf ) = ',i5///                                                               &
    5x,' element    side    ',2('   pressure   '), 2('    shear     '),/                                                           &
    5x,2('  number  '),2('    node i        node j    '),/ )

2000 format(2i5,4f10.0)
3000 format(6x,i5,7x,i2,3x,4(2x,e12.4))

end subroutine qdcrsf

