subroutine elemnt(task,ngrp)
    !
    ! subroutine to calculate element task number
    !
    character*8 task,eltask(6)
    dimension ngrp(1)
    common /info  / iexec,iacode,ldyn,ireadr,iwritr,iprtin,irank,                                                                  &
                    numseq,ndout,nsd,numnp,ndof,nlvect,nltftn,nptslf,numeg
    include 'memory_size.inc'
    !-ZACE-2005.08
    common ia(max_size)
    data ntask,    eltask                                                                                                          &
              /    6,'input___',                                                                                                   &
                     'form_lhs',                                                                                                   &
                     'form_rhs',                                                                                                   &
                     'str_prnt',                                                                                                   &
                     'str_stor',                                                                                                   &
                     'str_plot'/
    
    do 100 i=1,ntask
        if (task.eq.eltask(i)) itask = i
100 continue
    
    do 200 neg=1,numeg
    
        if (itask.eq.1) then
            mpnpar = mpoint('npar    ',16   ,0,0,1)
            ngrp(neg) = mpnpar
            call elcard(ia(mpnpar),neg)
        else
            mpnpar = ngrp(neg)
        endif

        ntype  = ia(mpnpar)
        call elmlib(ntype,mpnpar,itask,neg)
200 continue
    
    return
    
end subroutine elemnt
    