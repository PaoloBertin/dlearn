subroutine elcard(npar,neg)
    !
    !.... subroutine to read element group control card
    !
    dimension npar(1)
    common /iounit/ iin,iout,irsin,irsout
    
    read(iin,1000) (npar(i),i=1,16)
    write(iout,2000) neg
    
    return
    
1000 format(16i5)
2000 format('1',' e l e m e n t   g r o u p   d a t a         ',  //5x,                                                            &
            ' element group number  . . . . . . . . . . . (neg   ) = ',i5/// )
    
end subroutine elcard
    