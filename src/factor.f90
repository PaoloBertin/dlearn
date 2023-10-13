subroutine factor(a,idiag,neq) 
    !
    ! subroutine to perform Crout factorization: a = u(transpose) * d * u
    !
    !   a(i):  coefficient matrix stored in compacted column form;
    !          after factorization contains d and u
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single-precision operation
    dimension a(1),idiag(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    jj = 0
    
    do 300 j=1,neq
    
        jjlast = jj
        jj     = idiag(j)
        jcolht = jj - jjlast
    
        if (jcolht.gt.2) then
    
            ! for column j and i.le.j-1, replace a(i,j) with d(i,i)*u(i,j)
            istart = j - jcolht + 2
            jm1    = j - 1
            ij     = jjlast + 2
            ii     = idiag(istart-1)
    
            do 100 i=istart,jm1
    
                iilast = ii
                ii     = idiag(i)
                icolht = ii - iilast
                jlngth = i - istart + 1
                length = min0(icolht-1,jlngth)
                if (length.gt.0) a(ij) = a(ij) - coldot(a(ii-length),a(ij-length),length)
                ij = ij + 1
100         continue
    
        endif
    
        if (jcolht.ge.2) then
    
            ! for column j and i.le.j-1, replace a(i,j) with u(i,j);
            ! replace a(j,j) with d(j,j).
            jtemp = j - jj
            
            do 200 ij=jjlast+1,jj-1
                
                ii = idiag(jtemp + ij)

                ! warning: the following calculations are skipped if a(ii) equals zero                
                if (a(ii).ne.zero) then
                    temp  = a(ij)
                    a(ij) = temp/a(ii)
                    a(jj) = a(jj) - temp*a(ij)
                endif
200         continue
        endif

300 continue
    
    return

end subroutine factor
    