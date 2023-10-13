subroutine prntel(mat,ien,nen,numel)
    !
    !.... program to print data for element with "nen" nodes
    !
    !        note: presently the label formats are limited to
    !              elements with one to nine nodes
    !
    dimension mat(1),ien(nen,1)
    common /iounit/ iin,iout,irsin,irsout
    
    do 100 n=1,numel
        if (mod(n,50).eq.1) write(iout,1000) (i,i=1,nen)
        write(iout,2000) n,mat(n),(ien(i,n),i=1,nen)
100 continue
    
    return
    
1000 format('1',                                                                                                                   &
            ' e l e m e n t   d a t a',//5x,                                                                                       &
            ' element   material',9('  node ',i1,:,2x),/5x,                                                                        &
            '  number    number'//)

2000 format(6x,i5,9(5x,i5))

end subroutine prntel
    