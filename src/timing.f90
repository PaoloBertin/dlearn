subroutine timing(time)
    !
    !.... program to determine elapsed cpu time
    !.... **** this is a system-dependent routine ****
    !....     note: can only access clock time on vax/vms
    !     time = secnds(0.0)

    integer*4 hr,min,sec,hund
        
    ! microsoft Fortran PowerStation v.4.0
    ! call gettim(hr,min,sec,hund)
    !time = 3600.*hr + 60.*min + sec + hund * 0.01

    ! Fortran Lahey 77/90
    !       call timer ( iticks )
    !       time = 0.01 * iticks

    ! gnu fortran
    time = time8()

    return
end
