subroutine genel1(ien,mat,nen)
    !
    ! subroutine to generate element node and material numbers
    !
    dimension ien(nen,1),mat(1)
    common /genelc/ n,nel(3),incel(3),inc(3)
    
    ! set defaults
    call geneld
    
    ! generation algorithm    
    ie = n
    je = n
    ke = n
    
    ii = nel(1)
    jj = nel(2)
    kk = nel(3)
    
    do 300 k=1,kk
    
        do 200 j=1,jj
    
            do 100 i=1,ii
    
                if (i.ne.ii) then
                    le = ie
                    ie = le + incel(1)
                    call geneli(ien(1,ie),ien(1,le),inc(1),nen)
                    mat(ie) = mat(le)
                endif
100         continue
    
            if (j.ne.jj) then
                le = je
                je = le + incel(2)
                call geneli(ien(1,je),ien(1,le),inc(2),nen)
                mat(je) = mat(le)
                ie = je
            endif
200     continue
    
        if (k.ne.kk) then
            le = ke
            ke = le + incel(3)
            call geneli(ien(1,ke),ien(1,le),inc(3),nen)
            mat(ke) = mat(le)
            ie = ke
            je = ke
        endif
300 continue
    
    return
    
end subroutine genel1
    