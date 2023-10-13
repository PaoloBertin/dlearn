subroutine genel(ien,mat,nen)
    !
    ! subroutine to read and generate element node and material numbers
    !
    ! ien(nen,numel) = element node numbers
    ! mat(numel)     = element material numbers
    ! nen            = number of element nodes (le.27)
    ! n              = element number
    ! ng             = generation parameter
    ! nel(i)         = number of elements in direction i
    ! incel(i)       = element number increment for direction i
    ! inc(i)         = node number increment for direction i
    !
    dimension ien(nen,1),mat(1),itemp(27)
    common /iounit/ iin,iout,irsin,irsout
    common /genelc/ n,nel(3),incel(3),inc(3)
    
100 continue
    read(iin,1000) n,m,(itemp(i),i=1,nen),ng
    if (n.eq.0) return
    call imove(ien(1,n),itemp,nen)
    mat(n)=m
    if (ng.ne.0) then

        ! generate data
        read(iin,1000) (nel(i),incel(i),inc(i),i=1,3)
        call genel1(ien,mat,nen)
    endif
    go to 100
    
1000 format(16i5,10x,14i5)
    
end subroutine genel