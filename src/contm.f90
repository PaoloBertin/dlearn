subroutine contm(shg,xl,w,det,elmass,work,constm,imass,nint,nrowsh,nesd,nen,ned,nee,column)
!
! subroutine to form mass matrix for a continuum element
! with "nen" nodes
!
! imass = mass code, eq. 0, consistent mass
!                    eq. 1, lumped mass
!                    otherwise return
!
    implicit double precision (a-h,o-z)

    ! deactivate above card(s) for single-precision operation
    logical column
    dimension shg(nrowsh,nen,1),xl(nesd,1),w(1),det(1),elmass(nee,1),work(1)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five

    if (imass.eq.0) then
        
        ! consistent mass
        do 400 l=1,nint
            temp1 = constm*w(l)*det(l)

            do 300 j=1,nen
                n = (j - 1)*ned

                do 200 i=1,j
                    m = (i - 1)*ned
                    temp2 = temp1*shg(nrowsh,i,l)*shg(nrowsh,j,l)
                
                    do 100 k=1,ned
                        elmass(m + k,n + k) = elmass(m + k,n + k) + temp2
100                 continue

200             continue
300         continue
400     continue

    endif

    if (imass.eq.1) then
        ! lumped mass
        dsum   = zero
        totmas = zero
        call clear(work,nen)
        
        do 600 l=1,nint
            temp1 = constm*w(l)*det(l)
            totmas = totmas + temp1

            do 500 j=1,nen
                temp2 = temp1*shg(nrowsh,j,l)**2
                dsum = dsum + temp2
                work(j) = work(j) + temp2
500         continue

600     continue

        ! scale diagonal to conserve total mass
        temp1 = totmas/dsum

        if (column) then

            ! store terms in first column of matrix
            do 800 j=1,nen
                temp2 = temp1*work(j)
                n = (j - 1)*ned

                do 700 k=1,ned
                    elmass(n + k,1) = elmass(n + k,1) + temp2
700             continue

800         continue

        else
            
            ! store terms along diagonal of matrix
            do 1000 j=1,nen
                temp2 = temp1*work(j)
                n = (j - 1)*ned
                
                do 900 k=1,ned
                    elmass(n + k,n + k) = elmass(n + k,n + k) + temp2
900             continue
1000        continue

        endif
    endif

    return

end subroutine contm
