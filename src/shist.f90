subroutine shist(ishist,nsout,ntype)
    !
    !.... program to read, write and store element time-history input data
    !
    character*4 labeld,label1,label2,label3
    dimension ishist(3,1)
    common /iounit/ iin,iout,irsin,irsout
    common /labels/ labeld(3),label1(16),label2(3),label3(24)
    
    do 100 n=1,nsout
        if (mod(n,50).eq.1) write(iout,1000) nsout
        read(iin,2000) nel,intpt,ncomp
        if (intpt.eq.0) intpt = 1

        if (ntype.eq.1) write(iout,3000) nel,intpt,label1(ncomp)
        if (ntype.eq.2) write(iout,3000) nel,intpt,label2(ncomp)
        if (ntype.eq.3) write(iout,3000) nel,intpt,label3(ncomp)

        ! add if/write statements as above for additional element types
        ishist(1,n) = nel
        ishist(2,n) = intpt
        ishist(3,n) = ncomp
100 continue

    return

1000 format('1',                                                                                                                   &
        ' e l e m e n t   t i m e   h i s t o r y  ',                                                                              &
        ' i n f o r m a t i o n '//5x,                                                                                             &
        ' number of stress/strain time histories  . . (nsout ) = ',i5//                                                            &
        5x,'   element   int pt   component',/,                                                                                    &
        5x,'   number    number            ',/ )
2000 format(3i5)
3000 format(7x,i5,5x,i5,7x,1a4)

end subroutine shist

