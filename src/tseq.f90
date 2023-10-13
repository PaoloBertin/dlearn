subroutine tseq
    !
    ! subroutine to set memory pointers for time sequence 
    ! and nodal time history data arrays
    !
    double precision time
    logical ldyn
    common /bpoint/ mfirst,mlast,mtot,iprec
    common /dpoint/ mpstep,mpdprt,mpsprt,mphplt,mpiter,mpalph,mpbeta,                                                              &
                    mpgamm,mpdt  ,mpidhs,mpdout,mpvprd,mpdprd,mpa,mpv             
    common /hplotc/ nplpts,locplt,time
    common /info  / iexec,iacode,ldyn,ireadr,iwritr,iprtin,irank,                                                                  &
                    numseq,ndout,nsd,numnp,ndof,nlvect,nltftn,nptslf,                                                              &
                    numeg
    !-ZACE-2005.08
    include 'memory_size.inc'
    common a(MAX_SIZE)

    mpstep = mpoint('nstep   ',numseq,0,0,1)
    mpdprt = mpoint('ndprt   ',numseq,0,0,1)
    mpsprt = mpoint('nsprt   ',numseq,0,0,1)
    mphplt = mpoint('nhplt   ',numseq,0,0,1)
    mpiter = mpoint('niter   ',numseq,0,0,1)
    mpalph = mpoint('alpha   ',numseq,0,0,iprec)
    mpbeta = mpoint('beta    ',numseq,0,0,iprec)
    mpgamm = mpoint('gamma   ',numseq,0,0,iprec)
    mpdt   = mpoint('dt      ',numseq,0,0,iprec)
    
    call tseqin(a(mpstep),a(mpdprt),a(mpsprt),a(mphplt),                                                                           &
                a(mpiter),a(mpalph),a(mpbeta),a(mpgamm),                                                                           &
                a(mpdt  ),numseq,nplpts,ldyn)

    if (ndout.eq.0) then
        mpidhs = 1
        mpdout = 1
    else
        mpidhs = mpoint('idhist  ',3      ,ndout ,0,1)
        mpdout = mpoint('dout    ',ndout+1,nplpts,0,1)
    endif
    
    return

end subroutine tseq
    