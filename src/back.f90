subroutine back(a,b,idiag,neq)
    !
    !.... program to perform forward reduction and back substitution
    !
    implicit double precision (a-h,o-z)
    !
    !.... deactivate above card(s) for single-precision operation
    !
    dimension a(1),b(1),idiag(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    ! forward reduction
    jj = 0
    
    do 100 j=1,neq
        jjlast = jj
        jj     = idiag(j)
        jcolht = jj - jjlast
        if (jcolht.gt.1) b(j) = b(j) - coldot(a(jjlast+1),b(j-jcolht+1),jcolht-1)
100 continue
    
    !.... diagonal scaling
    do 200 j=1,neq
        ajj = a(idiag(j))

        ! warning: diagonal scaling is not performed if ajj equals zero
        if (ajj.ne.zero) b(j) = b(j)/ajj
200 continue
    
    ! back substitution
    if (neq.eq.1) return
        jjnext = idiag(neq)
        
        do 400 j=neq,2,-1
            jj     = jjnext
            jjnext = idiag(j-1)
            jcolht = jj - jjnext
            if (jcolht.gt.1) then
                bj = b(j)
                istart = j - jcolht + 1
                jtemp  = jjnext - istart + 1
                
                do 300 i=istart,j-1
                    b(i) = b(i) - a(jtemp+i)*bj
300             continue
            endif
    
400     continue
    
    return
    
end subroutine back
    