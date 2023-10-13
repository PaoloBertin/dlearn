subroutine truss(itask,npar,mp,neg)
    !
    !.... program to set storage and call tasks for the
    !        three-dimensional, elastic truss element
    !
    double precision time
    !.... deactivate above card(s) for single precision operation
    
    logical ldyn
    dimension npar(1),mp(1)
    common /bpoint/ mfirst,mlast,mtot,iprec
    common /dpoint/ mpstep,mpdprt,mpsprt,mphplt,mpiter,mpalph,mpbeta,                                                              &
                    mpgamm,mpdt  ,mpdout,mpidhs,mpvprd,mpdprd,mpa,mpv
    common /hplotc/ nplpts,locplt,time
    common /info  / iexec,iacode,ldyn,ireadr,iwritr,iprtin,irank,                                                                  &
                    numseq,ndout,nsd,numnp,ndof,nlvect,nltftn,nptslf,numeg
    common /spoint/ mpd,mpx,mpid,mpf,mpg,mpg1,mpdiag,mpngrp, mpalhs,mpbrhs
    !-ZACE-2005.08
    include 'memory_size.inc'
    common a(MAX_SIZE)
    
    mw     = 1
    mdet   = 2
    mshl   = 3
    mshg   = 4
    mxs    = 5
    mrho   = 6
    mrdpm  = 7
    mrdpk  = 8
    marea  = 9
    mc     = 10
    mgrav  = 11
    mien   = 12
    mmat   = 13
    mlm    = 14
    mishst = 15
    msout  = 16
    melefm = 17
    mxl    = 18
    mwork  = 19
    mb     = 20
    mdmat  = 21
    mdb    = 22
    mvl    = 23
    mal    = 24
    melres = 25
    mdl    = 26
    mstrn  = 27
    mstrs  = 28
    mforce = 29
    
    ntype  = npar( 1)
    numel  = npar( 2)
    numat  = npar( 3)
    nen    = npar( 4)
    nsout  = npar( 5)
    istprt = npar( 6)
    lfbody = npar( 7)
    
    if (npar(8).eq.0) npar(8) = 2
    
    nint   = npar( 8)
    imass  = npar( 9)
    impexp = npar(10)
    
    !.... set element parameters
    ned    = 3
    nee    = nen*ned
    nesd   = 3
    nrowsh = 2
    neesq  = nee*nee
    nrowb  = 1
    nstr   = 1

    if (itask.eq.1) then
        !
        !....... set memory pointers
        !
        !        note:  the mp array is stored directly after the npar array,
        !               beginning at location mpnpar + 16 of blank common.
        !               the variable "junk" is not used subsequently.
        !
        junk       = mpoint('mp      ',29     ,0     ,0     ,1)
        
        mp(mw    ) = mpoint('w       ',nint   ,0     ,0     ,iprec)
        mp(mdet  ) = mpoint('det     ',nint   ,0     ,0     ,iprec)
        mp(mshl  ) = mpoint('shl     ',nrowsh ,nen   ,nint  ,iprec)
        mp(mshg  ) = mpoint('shg     ',nrowsh ,nen   ,nint  ,iprec)
        mp(mxs   ) = mpoint('xs      ',nesd   ,nint  ,0     ,iprec)
        mp(mrho  ) = mpoint('rho     ',numat  ,0     ,0     ,iprec)
        mp(mrdpm ) = mpoint('rdampm  ',numat  ,0     ,0     ,iprec)
        mp(mrdpk ) = mpoint('rdampk  ',numat  ,0     ,0     ,iprec)
        mp(marea ) = mpoint('area    ',numat  ,0     ,0     ,iprec)
        mp(mc    ) = mpoint('c       ',nrowb  ,nrowb ,numat ,iprec)
        mp(mgrav ) = mpoint('grav    ',nesd   ,0     ,0     ,iprec)
        mp(mien  ) = mpoint('ien     ',nen    ,numel ,0     ,1)
        mp(mmat  ) = mpoint('mat     ',numel  ,0     ,0     ,1)
        mp(mlm   ) = mpoint('lm      ',ned    ,nen   ,numel ,1)
        
        if (nsout.eq.0) then
            mp(mishst) = junk
            mp(msout ) = junk
        else
            mp(mishst) = mpoint('ishist  ',3      ,nsout ,0     ,1)
            mp(msout ) = mpoint('sout    ',nsout+1,nplpts,0     ,1)
        endif
        
        mp(melefm) = mpoint('eleffm  ',nee    ,nee   ,0     ,iprec)
        mp(mxl   ) = mpoint('xl      ',nesd   ,nen   ,0     ,iprec)
        mp(mwork ) = mpoint('work    ',16     ,0     ,0     ,iprec)
        mp(mb    ) = mpoint('b       ',nrowb  ,nee   ,0     ,iprec)
        mp(mdmat ) = mpoint('dmat    ',nrowb  ,nrowb ,0     ,iprec)
        mp(mdb   ) = mpoint('db      ',nrowb  ,nee   ,0     ,iprec)
        mp(mvl   ) = mpoint('vl      ',ned    ,nen   ,0     ,iprec)
        mp(mal   ) = mpoint('al      ',ned    ,nen   ,0     ,iprec)
        mp(melres) = mpoint('elresf  ',nee    ,0     ,0     ,iprec)
        mp(mdl   ) = mpoint('dl      ',ned    ,nen   ,0     ,iprec)
        mp(mstrn ) = mpoint('strain  ',nrowb  ,0     ,0     ,iprec)
        mp(mstrs ) = mpoint('stress  ',nrowb  ,0     ,0     ,iprec)
        mp(mforce) = mpoint('force   ',nrowb  ,0     ,0     ,iprec)
    endif
    !
    !.... task calls
    !
    if (itask.gt.6) return
    go to (100,200,300,400,500,600    ),itask
    
100 continue
    
    ! input element data ('input___')
    call trust1(a(mp(mshl  )),a(mp(mw    )),a(mp(mrho  )),                                                                         &
                a(mp(mrdpm )),a(mp(mrdpk )),a(mp(marea )),                                                                         &
                a(mp(mc    )),a(mp(mgrav )),a(mp(mien  )),                                                                         &
                a(mp(mmat  )),a(mpid      ),a(mp(mlm   )),                                                                         &
                a(mpdiag    ),a(mp(mishst)),                                                                                       &
                ntype ,numel ,numat ,nen   ,nsout ,istprt,                                                                         &
                lfbody,nint  ,imass ,impexp,nrowsh,nrowb ,                                                                         &
                nesd  ,ndof  ,ned   ,iprtin,ldyn  )
    
    return
    
200 continue
    !
    ! form element effective mass and assemble into global
    ! left-hand-side matrix  ('form_lhs')
    call trust2(a(mp(melefm)),a(mp(mien  )),a(mpx       ),                                                                         &
                a(mp(mxl   )),a(mp(mmat  )),a(mp(mdet  )),                                                                         &
                a(mp(mshl  )),a(mp(mshg  )),a(mp(mrdpm )),                                                                         &
                a(mp(mrdpk )),a(mp(marea )),a(mp(mrho  )),                                                                         &
                a(mp(mw    )),a(mp(mwork )),a(mp(mb    )),                                                                         &
                a(mp(mc    )),a(mp(mdmat )),a(mp(mdb   )),                                                                         &
                a(mpalhs    ),a(mpdiag    ),a(mp(mlm   )),                                                                         &
                a(mp(mxs   )),                                                                                                     &
                impexp,imass ,numel ,neesq ,nen   ,nsd   ,                                                                         &
                nesd  ,nint  ,neg   ,nrowsh,ldyn  ,ned   ,                                                                         &
                nrowb ,nstr  ,nee   )
    
    return
    
300 continue
    !
    ! form element residual-force vector and assemble into global
    !        right-hand-side vector ('form_rhs')
    call trust3(a(mp(mmat  )),a(mp(mien  )),a(mpdprd    ),                                                                         &
                a(mp(mdl   )),a(mpvprd    ),a(mp(mvl   )),                                                                         &
                a(mpa       ),a(mp(mal   )),a(mp(mrdpk )),                                                                         &
                a(mp(mrdpm )),a(mp(mrho  )),a(mp(mgrav )),                                                                         &
                a(mp(melres)),a(mpx       ),a(mp(mxl   )),                                                                         &
                a(mp(mdet  )),a(mp(mshl  )),a(mp(mshg  )),                                                                         &
                a(mpg1      ),a(mp(mwork )),a(mp(marea )),                                                                         &    
                a(mp(mw    )),a(mp(melefm)),a(mp(mb    )),                                                                         &
                a(mp(mstrn )),a(mp(mc    )),a(mp(mdmat )),                                                                         &
                a(mp(mstrs )),a(mpbrhs    ),a(mp(mlm   )),                                                                         &
                a(mp(mxs   )),                                                                                                     &
                numel ,ned   ,nen   ,ndof  ,ldyn  ,nee   ,                                                                         &
                imass ,nesd  ,lfbody,nsd   ,nint  ,nrowsh,                                                                         &
                neg   ,nrowb )
    
    return
    
400 continue
    
    ! calculate and print element stress/strain output ('str_prnt')
    if (istprt.eq.0)                                                                                                               &
        call trust4(a(mp(mmat  )),a(mp(mien  )),a(mpd       ),                                                                     &
                    a(mp(mdl   )),a(mpx       ),a(mp(mxl   )),                                                                     &
                    a(mp(mdet  )),a(mp(mshl  )),a(mp(mshg  )),                                                                     &
                    a(mp(mxs   )),a(mp(mwork )),a(mp(mb    )),                                                                     &
                    a(mp(mstrn )),a(mp(mc    )),a(mp(mstrs )),                                                                     &
                    a(mp(mforce)),a(mp(marea )),                                                                                   &
                    nint  ,numel ,nen   ,ndof  ,ned   ,nsd   ,                                                                     &
                    nesd  ,nrowsh,neg   ,nrowb ,nee   )
    
    return
    
500 continue
    
    ! calculate and store element time-histories ('str_stor')
    if (nsout.gt.0)                                                                                                                &
        call trust5(a(mp(mishst)),a(mp(msout )),a(mp(mmat  )),                                                                     &
                    a(mp(mien  )),a(mpd       ),a(mp(mdl   )),                                                                     &
                    a(mpx       ),a(mp(mxl   )),a(mp(mdet  )),                                                                     &
                    a(mp(mshl  )),a(mp(mshg  )),a(mp(mxs   )),                                                                     &
                    a(mp(mb    )),a(mp(mstrn )),a(mp(mc    )),                                                                     &
                    a(mp(mstrs )),a(mp(mforce)),a(mp(marea )),                                                                     &
                    a(mp(mwork )),                                                                                                 &
                    nsout ,nen   ,ndof  ,ned   ,nsd   ,nesd  ,                                                                     &
                    nrowsh,nint  ,neg   ,nrowb ,nee   ,nsout+1)
    
    return
    
600 continue
    
    ! plot element time-histories ('str_plot')
    if (nsout.gt.0) call hplot(a(mp(mishst)),a(mp(msout )),nsout ,3,ntype )
    
    return
    
end subroutine truss

