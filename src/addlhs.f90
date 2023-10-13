subroutine addlhs(alhs,eleffm,idiag,lm,nee,ldiag)
    !
    !.... subroutine to add element left-hand-side matrix to
    !     global left-hand-side matrix
    !
    !     ldiag = .true.,  add diagonal element matrix
    !
    !     ldiag = .false., add upper triangle of full element matrix
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single-precision operation
    !
    logical ldiag
    dimension alhs(1),eleffm(nee,1),idiag(1),lm(1)
    !
    if (ldiag) then
    
        do 100 j=1,nee
            k = lm(j)
            if (k.gt.0) then
                l = idiag(k)
                alhs(l) = alhs(l) + eleffm(j,j)
            endif
100     continue

    else
    
        do 300 j=1,nee
            k = lm(j)
            if (k.gt.0) then

                do 200 i=1,j
                    m = lm(i)
                    if (m.gt.0) then
                        if (k.ge.m) then
                            l = idiag(k) - k + m
                        else
                            l = idiag(m) - m + k
                        endif
                        alhs(l) = alhs(l) + eleffm(i,j)
                    endif
200             continue

            endif
300     continue
    
    endif
    
    return
    
end subroutine addlhs
    