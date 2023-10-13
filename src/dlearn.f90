subroutine dlearn
    !
    !.... DLEARN - a linear static and dynamic finite element
    !                analysis program: global driver
    !
    double precision                                                                                                               &
                 time,zero,pt1667,pt25,pt5,one,two,three,four,five,tempf,                                                          &
                 coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,                                                                        &
                 coeff7,coeff8,alpha1,beta1 ,gamma1,dt1
    !
    !.... deactivate above card(s) for single-precision operation
    !
    logical ldyn
    character*4 title,ciao
    !
    !.... catalog of common statements
    !
    common /bpoint/ mfirst,mlast,mtot,iprec
    common /coeffs/ coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,                                                                     &
                    coeff7,coeff8,alpha1,beta1 ,gamma1,dt1
    common /colhtc/ neq
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /dpoint/ mpstep,mpdprt,mpsprt,mphplt,mpiter,mpalph,mpbeta,                                                              &
                    mpgamm,mpdt  ,mpidhs,mpdout,mpvprd,mpdprd,mpa,mpv
    common /etimec/ etime(7)
    common /genelc/ n,nel(3),incel(3),inc(3)
    common /genflc/ tempf(6,20),nf,numgpf,nincf(3),incf(3)
    common /hplotc/ nplpts,locplt,time
    common /info  / iexec,iacode,ldyn,ireadr,iwritr,iprtin,irank,                                                                  &
                    numseq,ndout,nsd,numnp,ndof,nlvect,nltftn,nptslf,                                                              &
                    numeg
    common /iounit/ iin,iout,irsin,irsout   
    common /labels/ labeld(3),label1(16),label2(3),label3(24)
    common /spoint/ mpd,mpx,mpid,mpf,mpg,mpg1,mpdiag,mpngrp,                                                                       &
                    mpalhs,mpbrhs
    common /titlec/ title(20)
    !-ZACE-2005.08
    include 'memory_size.inc'
    common a(MAX_SIZE)
    dimension ia(MAX_SIZE)
    equivalence(a(1),ia(1))
    data ciao/'*end'/
    !
    !.... input phase
    !
    call echo
    100 continue
    
    do 200 i=1,7
        etime(i) = 0.0
    200 continue
    
    call timing(t1)
    read(iin,1000) title
    if (title(1).eq.ciao) return
    read(iin,2000) iexec,iacode,ireadr,iwritr,iprtin,irank,numseq,                                                                 &
                   ndout,nsd,numnp,ndof,nlvect,nltftn,nptslf,numeg
    ldyn = .true.
    if (iacode.eq.1) ldyn = .false.
    write(iout,3000) title , iexec,iacode,ireadr,iwritr,iprtin                                                               
    write(iout,4000) irank ,numseq, ndout,   nsd, numnp,  ndof, nlvect,nltftn,nptslf, numeg
    
    ! initialization phase
    call tseq
    call dynpts 
    if (ndout.gt.0) call dhist(ia(mpidhs),ndout)
    call statin(neq)
!
    ntstep = 0
    time   = zero
    if (ldyn) then
!
        if (ireadr.eq.1) then
            ! read initial conditions from restart file
            call rsin(a(mpd),a(mpv),a(mpa),ndof,numnp,ntstep,time)
            if (iprtin.eq.0) then
                call printd(' r e s t a r t   d i s p l a c e m e n t s  ', a(mpd),ndof,numnp,ntstep,time)
                call printd(' r e s t a r t   v e l o c i t i e s        ', a(mpv),ndof,numnp,ntstep,time)
                call printd(' r e s t a r t   a c c e l e r a t i o n s  ', a(mpa),ndof,numnp,ntstep,time)
            endif
        else
!           read initial conditions from input file
            call input(a(mpd),ndof,numnp,1,1,iprtin,time)
            call input(a(mpv),ndof,numnp,2,1,iprtin,time)
            call input(a(mpa),ndof,numnp,3,1,iprtin,time)
        endif
!
    endif
    
    ! store initial kinematic data for time histories
    if ( ldyn .and. ndout.gt.0 ) then
        locplt = 1
        call stored(a(mpidhs),a(mpd),a(mpv),a(mpa),a(mpdout), ndof,ndout,ndout+1)
    endif
    
    ! input element data
    call elemnt('input___',a(mpngrp))

    ! store initial stress/strain data for element time histories
    if (ldyn) then
        locplt = 1
        call elemnt('str_stor',a(mpngrp))
    endif
    
    ! allocate memory for global equation system
    call eqset(neq,nalhs)
    call timing(t2)
    etime(1) = t2 - t1
    
    ! solution phase    
    if (iexec.eq.1) call driver(ntstep,neq,nalhs)
    
    ! write restart file
    if ( ldyn .and. (iwritr.eq.1) ) call rsout(a(mpd),a(mpv),a(mpa),ndof,numnp,ntstep,time)
    
    ! print memory-pointer dictionary
    call prtdc
    call timing(t1)
    etime(2) = t1 - t2

    ! print elapsed time summary
    call timlog
    go to 100

1000 format(20a4)
2000 format(16i5)
3000 format('1',20a4///                                                                                                            &
          ' e x e c u t i o n   c o n t r o l   i n f o r m a t i o n '//5x,                                                       &
          ' execution code  . . . . . . . . . . . . . . (iexec ) = ',i5//5x,                                                       &
          '    eq. 0, data check                                   ',   /5x,                                                       &
          '    eq. 1, execution                                    ',  //5x,                                                       &
          ' analysis code . . . . . . . . . . . . . . . (iacode) = ',i5//5x,                                                       &
          '    eq. 0, dynamic analysis                             ',   /5x,                                                       &
          '    eq. 1, static analysis                              ',  //5x,                                                       &
          ' read restart file code  . . . . . . . . . . (ireadr) = ',i5//5x,                                                       &
          '    eq. 0, do not read restart file                     ',   /5x,                                                       &
          '    eq. 1, read restart file                            ',  //5x,                                                       &
          ' write restart file code . . . . . . . . . . (iwritr) = ',i5//5x,                                                       &
          '    eq. 0, do not write restart file                    ',   /5x,                                                       &
          '    eq. 1, write restart file                           ',  //5x,                                                       &
          ' input data print code . . . . . . . . . . . (iprtin) = ',i5//5x,                                                       &
          '    eq. 0, print nodal and element input data           ',   /5x,                                                       &
          '    eq. 1, do not print nodal and element input data    ',   /5x)                                                       
4000 format(5x,                                                                                                                    &
          ' rank check code . . . . . . . . . . . . . . (irank ) = ',i5//5x,                                                       &
          '    eq. 0, do not perform rank check                    ',   /5x,                                                       &
          '    eq. 1, print numbers of zero and negative pivots    ',   /5x,                                                       &
          '    eq. 2, print all pivots                             ',  //5x,                                                       &
          ' number of time sequences  . . . . . . . . . (numseq) = ',i5//5x,                                                       &
          ' number of nodal output time-histories . . . (ndout ) = ',i5//5x,                                                       &
          ' number of space dimensions  . . . . . . . . (nsd   ) = ',i5//5x,                                                       &
          ' number of nodal points  . . . . . . . . . . (numnp ) = ',i5//5x,                                                       &
          ' number of nodal degrees-of-freedom  . . . . (ndof  ) = ',i5//5x,                                                       &
          ' number of load vectors  . . . . . . . . . . (nlvect) = ',i5//5x,                                                       &
          ' number of load-time functions . . . . . . . (nltftn) = ',i5//5x,                                                       &
          ' number of points on load-time functions . . (nptslf) = ',i5//5x,                                                       &
          ' number of element groups  . . . . . . . . . (numeg ) = ',i5//5x)
    
end subroutine dlearn
    