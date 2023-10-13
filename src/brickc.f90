subroutine brickc(itask,npar,mp,neg)
    !
    !.... program to set storage and call tasks for the
    !        eight-node brick, elastic element
    !
    double precision time
    !
    !.... deactivate above card(s) for single-precision operation
    !
    logical ldyn
    dimension npar(1),mp(1)
    common /bpoint/ mfirst,mlast,mtot,iprec
    common /dpoint/ mpstep,mpdprt,mpsprt,mphplt,mpiter,mpalph,mpbeta,                                                              &
                    mpgamm,mpdt,mpidhs,mpdout,mpvprd,mpdprd,mpa,mpv
    common /hplotc/ nplpts,locplt,time
    common /info  / iexec,iacode,ldyn,ireadr,iwritr,iprtin,irank,                                                                  &
                    numseq,ndout,nsd,numnp,ndof,nlvect,nltftn,nptslf,numeg
    common /spoint/ mpd,mpx,mpid,mpf,mpg,mpg1,mpdiag,mpngrp,mpalhs,mpbrhs
    !-ZACE-2005.08
    include 'memory_size.inc'
    common a(MAX_SIZE)
    
    mw     = 1
    mdet   = 2
    mshl   = 3
    mshg   = 4
    mshgbr = 5
    mrho   = 6
    mrdpm  = 7
    mrdpk  = 8
    mc     = 9
    mgrav  = 10
    mien   = 11
    mmat   = 12
    mlm    = 13
    mielno = 14
    miside = 15
    mpress = 16
    mishst = 17
    msout  = 18
    melefm = 19
    mxl    = 20
    mwork  = 21
    mb     = 22
    mdmat  = 23
    mdb    = 24
    mvl    = 25
    mal    = 26
    melres = 27
    mdl    = 28
    mstrn  = 29
    mstrs  = 30
    mpstrn = 31
    mpstrs = 32
    
    ntype  = npar( 1)
    numel  = npar( 2)
    numat  = npar( 3)
    nsurf  = npar( 4)
    nsout  = npar( 5)
    iopt   = npar( 6)
    istprt = npar( 7)
    lfsurf = npar( 8)
    lfbody = npar( 9)
    nicode = npar(10)
    ibbar  = npar(11)
    imass  = npar(12)
    impexp = npar(13)
    
    ! set element parameters
    nen    = 8
    ned    = 3
    nee    = nen*ned
    nesd   = 3
    nrowsh = 4
    neesq  = nee*nee
    nrowb  = 6
    nstr   = 6
    if (ibbar.eq.1) nstr = 4
    nint   = 1
    if (nicode.eq.0) nint = 8
    nrint  = 1
    if (itask.eq.1) then
    
        ! set memory pointers
        ! note:  the mp array is stored directly after the npar array,
        !        beginning at location mpnpar + 16 of blank common.
        !        the variable "junk" is not used subsequently.
        !
        junk       = mpoint('mp      ',32     ,0     ,0     ,1)
        
        mp(mw    ) = mpoint('w       ',nint   ,0     ,0     ,iprec)
        mp(mdet  ) = mpoint('det     ',nint   ,0     ,0     ,iprec)
        mp(mshl  ) = mpoint('shl     ',nrowsh ,nen   ,nint  ,iprec)
        mp(mshg  ) = mpoint('shg     ',nrowsh ,nen   ,nint  ,iprec)
        mp(mshgbr) = mpoint('shgbar  ',nrowsh ,nen   ,nrint ,iprec)
        mp(mrho  ) = mpoint('rho     ',numat  ,0     ,0     ,iprec)
        mp(mrdpm ) = mpoint('rdampm  ',numat  ,0     ,0     ,iprec)
        mp(mrdpk ) = mpoint('rdampk  ',numat  ,0     ,0     ,iprec)
        mp(mc    ) = mpoint('c       ',nrowb  ,nrowb ,numat ,iprec)
        mp(mgrav ) = mpoint('grav    ',nesd   ,0     ,0     ,iprec)
        mp(mien  ) = mpoint('ien     ',nen    ,numel ,0     ,1)
        mp(mmat  ) = mpoint('mat     ',numel  ,0     ,0     ,1)
        mp(mlm   ) = mpoint('lm      ',ned    ,nen   ,numel ,1)
        mp(mielno) = mpoint('ielno   ',nsurf  ,0     ,0     ,1)
        mp(miside) = mpoint('iside   ',nsurf  ,0     ,0     ,1)
        mp(mpress) = mpoint('press   ',4      ,nsurf ,0     ,iprec)
        
        if (nsout.eq.0) then
            mp(mishst) = junk
            mp(msout ) = junk
        else
            mp(mishst) = mpoint('ishist  ',3      ,nsout ,0     ,1)
            mp(msout ) = mpoint('sout    ',nsout+1,nplpts,0     ,1)
        endif
    
            mp(melefm) = mpoint('eleffm  ',nee    ,nee   ,0     ,iprec)
            mp(mxl   ) = mpoint('xl      ',nesd   ,nen   ,0     ,iprec)
            mp(mwork ) = mpoint('work    ',24     ,0     ,0     ,iprec)
            mp(mb    ) = mpoint('b       ',nrowb  ,nee   ,0     ,iprec)
            mp(mdmat ) = mpoint('dmat    ',nrowb  ,nrowb ,0     ,iprec)
            mp(mdb   ) = mpoint('db      ',nrowb  ,nee   ,0     ,iprec)
            mp(mvl   ) = mpoint('vl      ',ned    ,nen   ,0     ,iprec)
            mp(mal   ) = mpoint('al      ',ned    ,nen   ,0     ,iprec)
            mp(melres) = mpoint('elresf  ',nee    ,0     ,0     ,iprec)
            mp(mdl   ) = mpoint('dl      ',ned    ,nen   ,0     ,iprec)
            mp(mstrn ) = mpoint('strain  ',nrowb  ,0     ,0     ,iprec)
            mp(mstrs ) = mpoint('stress  ',nrowb  ,0     ,0     ,iprec)
            mp(mpstrn) = mpoint('pstrn   ',nrowb  ,0     ,0     ,iprec)
            mp(mpstrs) = mpoint('pstrs   ',nrowb  ,0     ,0     ,iprec)
    endif
    
    ! task calls
    if (itask.gt.6) return
    go to (100,200,300,400,500,600),itask
    
100 continue
    
    ! input element data ('input___')
    call bkct1(a(mp(mshl  )),a(mp(mw    )),a(mp(mrho  )),                                                                          &
               a(mp(mrdpm )),a(mp(mrdpk )),                                                                                        &
               a(mp(mc    )),a(mp(mgrav )),a(mp(mien  )),                                                                          &
               a(mp(mmat  )),a(mpid      ),a(mp(mlm   )),                                                                          &
               a(mpdiag    ),a(mp(mielno)),a(mp(miside)),                                                                          &
               a(mp(mpress)),              a(mp(mishst)),                                                                          &
               ntype ,numel ,numat ,nsurf ,nsout ,                                                                                 &
               istprt,lfsurf,lfbody,nicode,nint  ,ibbar ,                                                                          &
               imass ,impexp,nrowsh,nrowb ,nesd  ,nen   ,                                                                          &
               ndof  ,ned   ,iprtin,ldyn  )
    
    return
    
200 continue
    ! form element effective mass and assemble into global left-hand-side matrix  ('form_lhs')
    call bkct2(a(mp(melefm)),a(mp(mien  )),a(mpx       ),                                                                          &
               a(mp(mxl   )),a(mp(mmat  )),a(mp(mdet  )),                                                                          &
               a(mp(mshl  )),a(mp(mshg  )),                                                                                        &
               a(mp(mrdpm )),a(mp(mrdpk )),                                                                                        &
               a(mp(mrho  )),a(mp(mw    )),a(mp(mwork )),                                                                          &
               a(mp(mshgbr)),a(mp(mb    )),a(mp(mc    )),                                                                          &
               a(mp(mdmat )),a(mp(mdb   )),a(mpalhs    ),                                                                          &
               a(mpdiag    ),a(mp(mlm   )),                                                                                        &
               impexp,imass ,numel ,neesq ,nen   ,nsd   ,                                                                          &
               nesd  ,nint  ,neg   ,nrowsh,ldyn  ,ned   ,                                                                          &
               ibbar ,nrowb ,nstr  ,nee   )

    return
    
300 continue
    
    ! form element residual-force vector and assemble into global right-hand-side vector ('form_rhs')
    call bkct3(a(mp(mmat  )),a(mp(mien  )),a(mpdprd    ),                                                                          &
                    a(mp(mdl   )),a(mpvprd    ),a(mp(mvl   )),                                                                     &
                    a(mpa       ),a(mp(mal   )),a(mp(mrdpk )),                                                                     &
                    a(mp(mrdpm )),a(mp(mrho  )),a(mp(mgrav )),                                                                     &
                    a(mp(melres)),a(mpx       ),a(mp(mxl   )),                                                                     &
                    a(mp(mdet  )),a(mp(mshl  )),a(mp(mshg  )),                                                                     &
                    a(mpg1      ),a(mp(mwork )),                                                                                   &
                    a(mp(mw    )),a(mp(melefm)),                                                                                   &
                    a(mp(mshgbr)),a(mp(mb    )),a(mp(mstrn )),                                                                     &
                    a(mp(mc    )),a(mp(mstrs )),a(mpbrhs    ),                                                                     &
                    a(mp(mlm   )),a(mp(mielno)),a(mp(miside)),                                                                     &
                    a(mp(mpress)),                                                                                                 &
                    numel ,ned   ,nen   ,ndof  ,ldyn  ,nee   ,                                                                     &
                    imass ,nesd  ,lfbody,nsd   ,nint  ,nrowsh,                                                                     &
                    neg   ,nrowb ,nstr  ,ibbar , nsurf,                                                                            &
                    lfsurf)
    
    return
    
    400 continue
    
    ! calculate and print element stress/strain output ('str_prnt')
    if (istprt.eq.0)                                                                                                               &
        call bkct4(a(mp(mmat  )),a(mp(mien  )),a(mpd       ),                                                                      &
                   a(mp(mdl   )),a(mpx       ),a(mp(mxl   )),                                                                      &
                   a(mp(mdet  )),a(mp(mshl  )),a(mp(mshg  )),                                                                      &
                   a(mp(mwork ))              ,a(mp(mshgbr)),                                                                      &
                   a(mp(mw    )),a(mp(mb    )),a(mp(mstrn )),                                                                      &
                   a(mp(mc    )),a(mp(mstrs )),a(mp(mpstrn)),                                                                      &
                   a(mp(mpstrs)),                                                                                                  &
                   nint  ,numel ,nen   ,ndof  ,ned   ,nsd   ,                                                                      &
                   nesd  ,nrowsh,neg   ,ibbar ,nrowb ,                                                                             &
                   nee   ,nstr  )
    return
    
500 continue
    ! calculate and store element time-histories ('str_stor')
    if (nsout.gt.0)                                                                                                                &
        call bkct5(a(mp(mishst)),a(mp(msout )),a(mp(mmat  )),                                                                      &
                   a(mp(mien  )),a(mpd       ),a(mp(mdl   )),                                                                      &
                   a(mpx       ),a(mp(mxl   )),a(mp(mdet  )),                                                                      &
                   a(mp(mshl  )),a(mp(mshg  )),                                                                                    &
                   a(mp(mshgbr)),a(mp(mw    )),a(mp(mb    )),                                                                      &
                   a(mp(mstrn )),a(mp(mc    )),a(mp(mstrs )),                                                                      &
                   a(mp(mpstrn)),a(mp(mpstrs)),a(mp(mwork )),                                                                      &
                   nsout ,nen   ,ndof  ,ned   ,nsd   ,nesd  ,                                                                      &
                   nrowsh,nint  ,neg   ,ibbar ,nrowb ,                                                                             &
                   nee   ,nstr  ,nsout+1)
    return
    
600 continue
    ! plot element time-histories ('str_plot')
    if (nsout.gt.0)                                                                                                                &
        call hplot(a(mp(mishst)),a(mp(msout )),nsout ,3,ntype )
    return

end subroutine brickc
    