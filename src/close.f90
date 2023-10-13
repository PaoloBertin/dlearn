subroutine close (iunit)
    !
    ! ... interface for DLEARN program (T.J.R. HUGHES)
    !     to close and keep a file if exist
    !
    close (unit=iunit,status='keep')

    return
    
end subroutine close

