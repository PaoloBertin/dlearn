subroutine dynpts 
    !
    !.... subroutine to set memory pointers for dynamic analysis data arrays
    !
    logical ldyn
    common /bpoint/ mfirst,mlast,mtot,iprec
    common /dpoint/ mpstep,mpdprt,mpsprt,mphplt,mpiter,mpalph,mpbeta,mpgamm,mpdt  ,mpidhs,mpdout,mpvprd,mpdprd,mpa,mpv
    common /info  / iexec,iacode,ldyn,ireadr,iwritr,iprtin,irank,numseq,ndout,nsd,numnp,ndof,nlvect,nltftn,nptslf,numeg
    !-ZACE-2005.08
    if (ldyn) then
        mpvprd = mpoint('vpred   ',ndof   ,numnp ,0,iprec)
        mpdprd = mpoint('dpred   ',ndof   ,numnp ,0,iprec)
        mpa    = mpoint('a       ',ndof   ,numnp ,0,iprec)
        mpv    = mpoint('v       ',ndof   ,numnp ,0,iprec)
    else
        mpvprd = mpoint('vpred   ',0   ,0 ,0,iprec)
        mpdprd = mpoint('dpred   ',0   ,0 ,0,iprec)
        mpa    = mpoint('a       ',0   ,0 ,0,iprec)
        mpv    = mpoint('v       ',0   ,0 ,0,iprec)
    end if
    
    return
    
end subroutine dynpts