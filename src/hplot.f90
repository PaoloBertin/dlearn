subroutine hplot(ih,xt,nplots,nrows,io)
    !
    !.... program to plot output histories
    !
    !           ih(nrows,nplots) = dof/component information
    !        xt(nplots+1,nplpts) = output history data
    !        xt(       1,nplpts) = time record
    !                     nplots = number of histories to be plotted
    !                     nplpts = number of time points at which
    !                              data is to be plotted
    !                      nrows = number of rows in ih array
    !                         io = output code
    !                                 eq.0, nodal output histories
    !                                 eq.n.gt.0, element output histories
    !                                    (n = ntype in calling routine)
    !
          double precision time
    !
    !.... deactivate above card(s) for single-precision operation
    !
    character*1 iblank,istar,line(53)
    character*4 title,labeld,label1,label2,label3
    dimension ih(nrows,1),xt(nplots+1,1)
    common /hplotc/ nplpts,locplt,time
    common /iounit/ iin,iout,irsin,irsout
    common /labels/ labeld(3),label1(16),label2(3),label3(24)
    common /titlec/ title(20)
    !
    data iblank,istar/' ','*'/,nchar/53/
    !
    do 300 i=1,nplots
        
        i1 = ih(1,i)
        i2 = ih(2,i)
        i3 = ih(3,i)
        
        if (io.eq.0) write(iout,1000) title,i1,i2,labeld(i3)
        if (io.eq.1) write(iout,2000) title,i1,i2,label1(i3)
        if (io.eq.2) write(iout,2000) title,i1,i2,label2(i3)
        if (io.eq.3) write(iout,2000) title,i1,i2,label3(i3)
        
        ! add if/write statements as above for additional element types
        call minmax(xt,xmax,xmin,nplots+1,nplpts,i+1)
        if (xmax.eq.xmin) then
            write(iout,3000) xmax
        else
            scale = xmax - xmin
            write(iout,4000) xmin,xmax
    
            do 200 j=1,nplpts
                t = xt(1,j)
                do 100 k = 1,nchar
                    line(k) = iblank
100             continue
    !
                xk = ((xt(i+1,j) - xmin)/scale)*nchar
                k  = xk + 1
                if (k.gt.nchar) k = nchar
                line(k) = istar
                write(iout,5000) t,xt(i+1,j),(line(k),k=1,nchar)
200         continue
        endif
    
300 continue
    
    return
    
1000 format('1',20a4//                                                                                                             &
          ' node number = ',i5//                                                                                                   &
          ' dof number  = ',i5,5x,'output: ',a4//5x)

2000 format('1',20a4//                                                                                                             &
           ' element number            = ',i5//                                                                                    &
           ' integration point number  = ',i5,5x,'output: ',a4//5x)

3000 format(' ',                                                                                                                   &
            ' value is constant ( = ',1pe11.4,' ), plot omitted')
     
4000 format(' ',4x,'time',8x,'value',6x,1pe11.4,31x,1pe11.4,/,                                                                     & 
            2x,10('-'),3x,10('-'),3x,53('-'))

5000 format(' ',1pe11.4,2x,1pe11.4,3x,53a1)

end subroutine hplot
    