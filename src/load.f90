subroutine load(id,f,brhs,g1,ndof,numnp,nlvect)
    !
    !.... program to accumulate nodal forces and transfer into
    !        right-hand-side vector
    !
    implicit double precision (a-h,o-z)
    
    !.... deactivate above card(s) for single-precision operation
    dimension id(ndof,1),f(ndof,numnp,1),brhs(1),g1(1)
    
    do 300 i=1,ndof
    
        do 200 j=1,numnp
            k = id(i,j)
            if (k.gt.0) then
    
                do 100 nlv=1,nlvect
                    brhs(k) = brhs(k) + f(i,j,nlv)*g1(nlv)
100             continue
    
            endif
    
200     continue
    
300 continue
    
    return
    
end subroutine load
    