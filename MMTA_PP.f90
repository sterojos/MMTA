! DULEZITA POZNAMKA
! pro spravny vypis dat po kompilaci v Intel Fortranu
! pouzit moznost -no-wrap-margin

program MMTA_PP
    implicit none

    ! pocitadlo
    integer :: k, l, m

    ! umisteni leveho dolniho rohu ctverce a jeho strana
    real, dimension(2) :: xy_loc
    real :: side

    ! shodny krok v x a y
    real :: dr

    ! parametry site
    integer :: ars
    real, allocatable :: x_c(:,:), y_c(:,:)
    real, allocatable :: u(:,:)

    ! sem zadat hodnoty
    print *, "Zadej velikost site:"
    read(*,*) ars
    xy_loc = (-1, -1)
    side = 2

    ! dal nesahat (leda na poc. podm.)

    dr = side / (ars - 1)

    allocate(x_c(ars, ars))
    allocate(y_c(ars, ars))
    allocate(u(ars, ars))

    open(action = 'write', file = 'data/x_points.dat', &
	& unit = 20, status = 'replace')
    open(action = 'write', file = 'data/y_points.dat', &
	& unit = 30, status = 'replace')
    open(action = 'write', file = 'data/u_values_init.dat', &
	& unit = 40, status = 'replace')
    open(action = 'write', file = 'data/constants.dat', &
	& unit = 50, status = 'replace')

    do l = 1, ars
        do k = 1, ars
            x_c(k,l) = xy_loc(1) + (k-1) * dr
            y_c(k,l) = xy_loc(2) + (l-1) * dr
            u(k,l) = y_c(k,l) * sin(8 * atan(1.) * x_c(k,l))
        end do
        write(20, *) (x_c(m, l), m = 1, ars)
        write(30, *) (y_c(m, l), m = 1, ars)   
        write(40, *) (u(m, l), m = 1, ars)      
    end do

    write(50, *) ars, dr, 0.

end program MMTA_PP
