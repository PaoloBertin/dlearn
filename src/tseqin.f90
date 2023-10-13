subroutine tseqin(nstep,ndprt,nsprt,nhplt,niter,alpha,beta,gamma,dt,numseq,nplpts,ldyn)
    !
    !.... program to read, write and store time sequence data
    !
    !        note: "nplpts" is passed to subroutine hplot by way of
    !               common /hplotc/
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single precision operation
    logical ldyn
    dimension nstep(1),ndprt(1),nsprt(1),nhplt(1),niter(1),alpha(1),beta(1),gamma(1),dt(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /iounit/ iin,iout,irsin,irsout
    
    nplpts = 1
    
    do 100 i=1,numseq
        read(iin,1000) n,nstep(n),ndprt(n),nsprt(n),nhplt(n),niter(n),alpha(n),beta(n),gamma(n),dt(n)
        if (nhplt(n).gt.0) nplpts = nplpts + nstep(n)/nhplt(n)
100 continue
    
    !.... set default sequence parameters for static analysis
    if (.not.ldyn) then
        do 200 i=1,numseq
            nstep(i) = max0(1,nstep(i))
            ndprt(i) = 1
            nsprt(i) = 1
            nhplt(i) = 0
            niter(i) = 1
            alpha(i) = zero
            beta(i)  = one
            gamma(i) = zero
            dt(i)    = one
200     continue
    endif
    
    do 300 n=1,numseq
        if (mod(n,2).eq.1) write(iout,2000) numseq
        write(iout,3000) n,nstep(n),ndprt(n),nsprt(n),nhplt(n),niter(n),alpha(n),beta(n) ,gamma(n),dt(n)
300 continue
    
    return
    
1000 format(6i5,4f10.0)
2000 format('1',' t i m e   s e q u e n c e   d a t a      ',     //5x,                                                            &
     ' number of time sequences . . . . . . (numseq  ) = ',    i5///  )
3000 format(5x,                                                                                                                    &
     ' time sequence number . . . . . . . . (n       ) = ',     i5//5x,                                                            &
     ' number of time steps . . . . . . . . (nstep(n)) = ',     i5//5x,                                                            &
     ' kinematic print increment  . . . . . (ndprt(n)) = ',     i5//5x,                                                            &
     ' stress/strain print increment  . . . (nsprt(n)) = ',     i5//5x,                                                            &
     ' time history plot increment  . . . . (nhplt(n)) = ',     i5//5x,                                                            &
     ' number of iterations . . . . . . . . (niter(n)) = ',     i5//5x,                                                            &
     ' first integration parameter  . . . . (alpha(n)) = ',1pe12.5//5x,                                                            &
     ' second integration parameter . . . . (beta(n) ) = ',1pe12.5//5x,                                                            &
     ' third integration parameter  . . . . (gamma(n)) = ',1pe12.5//5x,                                                            &
     ' time step  . . . . . . . . . . . . . (dt(n)   ) = ',1pe12.5////)
    
end
