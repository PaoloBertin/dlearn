subroutine genfl1(a,nra)
    !
    !.... program to generate floating-point nodal data
    !        via isoparametric interpolation
    !
    !         iopt = 1, generation along a line
    !              = 2, generation over a surface
    !              = 3, generation within a volume
    !
    implicit double precision (a-h,o-z)
    
    ! deactivate above card(s) for single-precision operation
    dimension a(nra,1),sh(20)
    common /genflc/ temp(6,20),n,numgp,ninc(3),inc(3)
    common /consts/ zero,pt1667,pt25,pt5,one,two,three,four,five
    
    iopt = 3
    if (ninc(3).eq.0) iopt = 2
    if (ninc(2).eq.0) iopt = 1
    
    dr = zero
    ds = zero
    dt = zero
    
    if (ninc(1).ne.0) dr = two/ninc(1)
    if (ninc(2).ne.0) ds = two/ninc(2)
    if (ninc(3).ne.0) dt = two/ninc(3)
    
    ii = ninc(1)+1
    jj = ninc(2)+1
    kk = ninc(3)+1
    
    ni = n
    nj = n
    nk = n
    
    t = -one
    do 300 k=1,kk
    
        s = -one
        do 200 j=1,jj
    
            r = -one
            do 100 i=1,ii
    
                call gensh(r,s,t,sh,numgp,iopt)
                call multab(temp,sh,a(1,ni),6,20,nra,numgp,nra,1,1)
                ni = ni + inc(1)
                r = r + dr
100         continue
    
            nj = nj + inc(2)
            ni = nj
            s = s + ds
200     continue
    
        nk = nk + inc(3)
        ni = nk
        t = t + dt
300 continue
    
    return
    
end subroutine genfl1
    