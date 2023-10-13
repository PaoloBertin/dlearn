subroutine driver(ntstep,neq,nalhs)
    !
    ! solution driver program
    !
    double precision time,coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,coeff7,coeff8,alpha1,beta1 ,gamma1,dt1

    ! deactivate above card(s) for single-precision operation
    logical ldyn,lout
    common /coeffs/ coeff1,coeff2,coeff3,coeff4,coeff5,coeff6,                                                                     &
                    coeff7,coeff8,alpha1,beta1 ,gamma1,dt1
    common /dpoint/ mpstep,mpdprt,mpsprt,mphplt,mpiter,mpalph,mpbeta,                                                              &
                    mpgamm,mpdt  ,mpidhs,mpdout,mpvprd,mpdprd,mpa,mpv
    common /etimec/ etime(7)
    common /hplotc/ nplpts,locplt,time
    common /info  / iexec,iacode,ldyn,ireadr,iwritr,iprtin,irank,                                                                  &
                    numseq,ndout,nsd,numnp,ndof,nlvect,nltftn,nptslf,numeg
    common /spoint/ mpd,mpx,mpid,mpf,mpg,mpg1,mpdiag,mpngrp,mpalhs,mpbrhs
    !-ZACE-2005.08
    include 'memory_size.inc'
    common a(MAX_SIZE)

    ! time sequence loop
    do 300 nsq=1,numseq
        ! set current time sequence parameters
        call timcon(nsq,a(mpstep),a(mpdprt),a(mpsprt),a(mphplt),a(mpiter),                                                         &
                    nstep1,   ndprt1,   nsprt1,   nhplt1,   niter1,                                                                &
                    a(mpalph),a(mpbeta),a(mpgamm),a(mpdt  ))

        ! form effective mass matrix
        call clear(a(mpalhs),nalhs)
        call timing(t1)
        call elemnt('form_lhs',a(mpngrp))
        call timing(t2)
        etime(3) = etime(3) + t2 - t1
        
        ! perform factorization of effective mass matrix
        call factor(a(mpalhs),a(mpdiag),neq)
        call timing(t1)
        etime(4) = etime(4) + t1 - t2
        
        ! rank check (note: subroutines "pivots" and "printp"
        ! return to statement 300)
        if (irank.eq.1) call pivots(a(mpalhs),a(mpdiag),neq,nsq,*300)
        if (irank.eq.2) call printp(a(mpalhs),a(mpdiag),neq,nsq,*300)
        
        ! time step loop
        do 200 n=1,nstep1
            time = time + dt1
            ntstep = ntstep + 1
            if (ldyn) then
                ! predictor update of all degrees-of-freedom
                call predct(a(mpd),a(mpv),a(mpa),a(mpdprd),a(mpvprd),ndof,numnp)
            else
                call clear(a(mpd),ndof*numnp)
            endif
            
            ! evaluate load-time functions at time n+1
            if (nlvect.gt.0) call lfac(a(mpg),time,a(mpg1),nltftn,nptslf)
            
            ! overwrite predictors to account for kinematic boundary conditions
            if (dt1.ne.0) call compbc(a(mpid),a(mpd),a(mpv),a(mpa),a(mpdprd),a(mpvprd),                                            &
                                      a(mpf),a(mpg1),ndof,numnp,nlvect,ldyn)
            
            ! multi-corrector iteration loop
            do 100 i=1,niter1
                call clear(a(mpbrhs),neq)

                ! evaluate load-time functions at time n+1+alpha 
                if (nltftn.gt.0) call lfac(a(mpg),time+alpha1*dt1,a(mpg1),nltftn,nptslf)                
                
                ! form nodal contribution to residual force vector
                if (nlvect.gt.0) call load(a(mpid),a(mpf),a(mpbrhs),a(mpg1),ndof,numnp,nlvect)
                
                !.... form element contribution to residual force vector
                call timing(t1)
                call elemnt('form_rhs',a(mpngrp))
                call timing(t2)
                etime(5) = etime(5) + t2 - t1
                
                ! solve equation system
                call back(a(mpalhs),a(mpbrhs),a(mpdiag),neq)
                call timing(t1)
                etime(6) = etime(6) + t1 - t2

                ! perform intermediate update of active degrees-of-freedom
                call iterup(a(mpid),a(mpd),a(mpdprd),a(mpvprd),a(mpa),a(mpbrhs),ndof,numnp,ldyn)
100         continue

            ! perform corrector update of all degrees-of-freedom        
            if (ldyn) call corrct(a(mpd),a(mpv),a(mpdprd),a(mpvprd),ndof,numnp)

            if (lout(n,ndprt1)) then
                ! write kinematic output
                call printd(' d i s p l a c e m e n t s                  ', a(mpd),ndof,numnp,ntstep,time)
            
                if (ldyn) then 
                    call printd(' v e l o c i t i e s                        ', a(mpv),ndof,numnp,ntstep,time)
                    call printd(' a c c e l e r a t i o n s                  ', a(mpa),ndof,numnp,ntstep,time)
                endif
            endif

            call timing(t1)

            ! calculate and write element output
            if (lout(n,nsprt1)) call elemnt('str_prnt',a(mpngrp))

            if (ldyn.and.lout(n,nhplt1)) then 
                locplt = locplt + 1

                ! note: variables "locplt" and "time" are passed into sub-
                !       routine "stored" and element routines performing
                !       task5 ('str_stor') by way of common /hplotc/
                ! store kinematic time history data
            
                ! ZACE MODIF (RD 15 may 1987) -------(T.J.R. Hughes tel. call)-----------
                if (ndout.GT.0) call stored(a(mpidhs),a(mpd),a(mpv),a(mpa),a(mpdout),ndof,ndout,ndout+1)

                ! calculate and store element time history data
                call elemnt('str_stor',a(mpngrp))
            endif
            call timing(t2)
            etime(7) = etime(7) + t2 - t1
        
200     continue

300 continue

    if (ldyn) then
    
        ! plot nodal time-histories
        if (ndout.gt.0) call hplot (a(mpidhs),a(mpdout),ndout,3,0)
        
        ! plot element time-histories
        call elemnt('str_plot',a(mpngrp))
    endif

    return

end subroutine driver
