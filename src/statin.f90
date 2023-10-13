subroutine statin(neq)
    !
    !.... program to set memory pointers for static analysis data arrays,
    !     and call associated input routines
    !
    double precision zero,pt1667,pt25,pt5,one,two,three,four,five
    !
    !.... deactivate above card(s) for single precision operation
    !
    logical ldyn
    common /bpoint/ mfirst,mlast,mtot,iprec
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    common /dpoint/ mpstep,mpdprt,mpsprt,mphplt,mpiter,mpalph,mpbeta,                                                              &
                    mpgamm,mpdt  ,mpidhs,mpdout,mpvprd,mpdprd,mpa,mpv
    common /info  / iexec,iacode,ldyn,ireadr,iwritr,iprtin,irank,                                                                  &
                    numseq,ndout,nsd,numnp,ndof,nlvect,nltftn,nptslf,                                                              &
                    numeg
    common /spoint/ mpd,mpx,mpid,mpf,mpg,mpg1,mpdiag,mpngrp,mpalhs,mpbrhs
    !-ZACE-2005.08
    include 'memory_size.inc'
    common a(MAX_SIZE)
    
    mpd    = mpoint('d       ',ndof  ,numnp ,0    ,iprec)
    if (.not.ldyn) mpdprd = mpd
    mpx    = mpoint('x       ',nsd   ,numnp ,0     ,iprec)
    mpid   = mpoint('id      ',ndof  ,numnp ,0     ,1)
    
    if (nlvect.eq.0) then
        mpf = 1
    else
        mpf = mpoint('f       ',ndof  ,numnp ,nlvect,iprec)
    endif
    
    if (nltftn.eq.0) then
        mpg  = 1
        mpg1 = 1
    else
        mpg  = mpoint('g       ',nptslf,2     ,nltftn,iprec)
        mpg1 = mpoint('g1      ',nltftn,0     ,0     ,iprec)
    endif
    
    ! input coordinate data
    call coord(a(mpx),nsd,numnp,iprtin)
    
    ! input boundary condition data and establish equation numbers
    call bc(a(mpid),ndof,numnp,neq,iprtin)
    
    ! input nodal force and prescribed kinematic boundary-value data
    if (nlvect.gt.0) call input(a(mpf),ndof,numnp,0,nlvect,iprtin,zero)
    
    ! input load-time functions!
    if (nltftn.gt.0) call ltimef(a(mpg),nptslf,nltftn,iprtin)
    
    ! allocate memory for idiag array and clear
    mpdiag = mpoint('idiag   ',neq   ,0     ,0     ,1)
    call iclear(a(mpdiag),neq)
    
    mpngrp = mpoint('ngrp    ',numeg ,0     ,0     ,1)
    
    return
    
end subroutine statin
    
    