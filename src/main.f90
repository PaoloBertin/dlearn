program main
    !
    !
    !     ************************************************************
    !     *                                                          *
    !     *               * * *   D L E A R N   * * *                *
    !     *                                                          *
    !     *                                                          *
    !     *               A LINEAR STATIC AND DYNAMIC                *
    !     *                                                          *
    !     *             FINITE ELEMENT ANALYSIS PROGRAM              *
    !     *                                                          *
    !     *                                                          *
    !     *               T. J. R.   H U G H E S                     *
    !     *                                                          *
    !     *                  R. M.   F E R E N C Z                   *
    !     *                                                          *
    !     *                  A. M.   R A E F S K Y                   *
    !     *                                                          *
    !     *                                                          *
    !     *                 1 5   M a y   1 9 8 6                    *
    !     *                                                          *
    !     ************************************************************
    !     *                                                          *
    !     *   Reference:  Thomas J. R. Hughes, "The Finite Element   *
    !     *   Method -- Linear Static and Dynamic Finite Element     *
    !     *   Analysis," Prentice-Hall, Englewood Cliffs, N.J. 1987  *
    !     *                                                          *
    !     ************************************************************
    !     *                                                          *
    !     *         NOTICE TO USERS OF THIS PROGRAM PACKAGE          *
    !     *                                                          *
    !     *   No responsibility or legal liability is assumed by     *
    !     *   the authors for the accuracy, completeness, or use-    *
    !     *   fulness of any information or process contained in     *
    !     *   this computer program package, or for any errors,      *
    !     *   mistakes, or misrepresentations that may occur from    *
    !     *   the use of this computer program package.  All soft-   *
    !     *   ware provided are in "as is, with all defects" con-    *
    !     *   dition.  No warranties of any kind, whether statu-     *
    !     *   tory, written, oral, expressed, or implied (includ-    *
    !     *   ing warranties of fitness and merchantability) shall   *
    !     *   apply.                                                 *
    !     *                                                          *
    !     *   Distribution of this program is made possible by the   *
    !     *   authors with the stipulation that the program neither  *
    !     *   be sold in whole or in part for direct profit nor      *
    !     *   royalties or development charges made for its use.     *
    !     *   By acceptance of delivery of this program package,     *
    !     *   the purchaser understands the restrictions on the use  *
    !     *   and distribution of the program.  The fee paid repre-  *
    !     *   sent a charge for materials, duplication, packaging,   *
    !     *   mailing, communications, and assorted labor costs.     *
    !     *   The legal ownership of the program remains with the    *
    !     *   developers.                                            *
    !     *                                                          *
    !     *   Copyright: 1987 by T.J.R. Hughes, all rights reserved. *
    !     *                                                          *
    !     ************************************************************
    !
    !
    !.... program to set storage capacity, precision and input/output units
    !
    common /bpoint/ mfirst,mlast,mtot,iprec
    common /iounit/ iin,iout,irsin,irsout
    common a(50000)

    character(255) :: cwd, finput, foutput,finout

    ! mfirst = address of first available word in blank common
    ! mlast  = address of last available word in blank common
    ! mtot   = total storage allocated to blank common
    ! iprec  = precision flag; eq.1, single precision
    !                          eq.2, double precision
    mfirst = 1
    mlast  = 50000
    mtot   = 50000
    iprec  = 2
    
    ! iin    = input unit number
    ! iout   = output unit number
    ! irsin  = restart input unit number
    ! irsout = restart output unit number
    !
    iin    = 5
    iout   = 6
    irsin  = 7
    irsout = 8
    
    !.... system-dependent unit/file specifications
    !
    !        the following lines are appropriate for the VMS operating
    !        system -- change as necessary for other operating systems
    !
    !ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    !      call assign(   iin,   'input.dat', 9)
    !      call assign(  iout,  'output.dat',10)
    !      call assign( irsin, 'rsinput.dat',11)
    !      call assign(irsout,'rsoutput.dat',12)

    ! working directory
    call getcwd(cwd)
    write(*, *) 'working directory: ', cwd

    ! reading input and output file names
    finout = '/home/paolo-bertin/Documenti/FortranProjets/dlearn/data/finout.txt'
    open(iin, file = finout,  status='OLD')
    read(iin, '(a255)') finput
    read(iin, '(a255)') foutput

    write(*, *) 'file input: ',  finput
    write(*, *) 'file output: ', foutput

    close(5)

    !open(unit= iin,file='input.dat',status='old')
    !open(unit=iout,file='output.dat')
    open(unit= iin,file=finput,status='old')
    open(unit=iout,file=foutput)
    !ZACE MODIF (RD 30 mar 1987) -------------------------------------------
    
    call dlearn
    
    !.... system-dependent unit/file specifications
    !
    !        the following lines are appropriate for the VMS operating
    !        system -- change as necessary for other operating systems
    !
    call close(iin)
    call close(iout)
    
    stop
end
