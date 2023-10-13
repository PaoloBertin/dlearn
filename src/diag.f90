subroutine diag(idiag,neq,n)
    !
    ! subroutine to compute diagonal addresses of left-hand-side matrix
    !
    dimension idiag(1)
    !
    n = 1
    idiag(1) = 1
    
    if (neq.eq.1) return
    
    do 100 i=2,neq
        idiag(i) = idiag(i) + idiag(i-1) + 1
100 continue
    n = idiag(neq)
    
    return

end subroutine diag