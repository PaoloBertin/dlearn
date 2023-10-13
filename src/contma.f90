subroutine contma(shg,xl,w,det,al,elmass,work,elresf,constm,imass,nint,nrowsh,nesd,nen,ned,nee)
!
! subroutine to calculate inertial and gravity/body force ("-m*(a-g)")
!            for a continuum element with "nen" nodes
!
! imass = mass code, eq. 0, consistent mass
!                    eq. 1, lumped mass
!                    otherwise return
!
    implicit double precision (a-h,o-z)

    ! deactivate above card(s) for single-precision operation
    dimension shg(nrowsh,nen,1),xl(nesd,1),w(1),det(1),al(ned,1),elmass(nee,1),work(1),elresf(ned,1)
    if (imass.eq.0) then
    
        ! consistent mass
        do 300 l=1,nint
            temp = constm*w(l)*det(l)
        
            do 200 i=1,ned
                acc = rowdot(shg(nrowsh,1,l),al(i,1),nrowsh,ned,nen)
            
                do 100 j=1,nen
                    elresf(i,j) = elresf(i,j) + temp*acc*shg(nrowsh,j,l)
100             continue

200         continue

300     continue

    endif

    if (imass.eq.1) then
        
        ! lumped mass
        call clear(elmass,nee)
        call contm(shg,xl,w,det,elmass,work,constm,imass,nint,nrowsh,nesd,nen,ned,nee,.true.)

        do 500 j=1,nen
            k = (j - 1)*ned

            do 400 i=1,ned
                elresf(i,j) = elresf(i,j) + al(i,j)*elmass(k + i,1)
400         continue

500     continue

    endif

    return
    
end subroutine contma