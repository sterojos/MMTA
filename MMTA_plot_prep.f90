program plot_prep

    integer :: ars, toggle, k, l
    real, allocatable :: x_c(:,:), y_c(:,:), u(:,:)

    open(action = 'read', file = 'data/x_points.dat', &
	& unit = 20, status = 'old')
    open(action = 'read', file = 'data/y_points.dat', &
	& unit = 30, status = 'old')
	
    print *, "0 - priprav pocatecni hodnoty, 1 - priprav aktualni hodnoty"
    read(*, *) toggle
    
    if (toggle == 0) then
        open(action = 'read', file = 'data/u_values_init.dat', &
	    & unit = 40, status = 'old')
    else if (toggle == 1) then
        open(action = 'read', file = 'data/u_values_final.dat', &
	    & unit = 40, status = 'old')
    end if
    
    open(action = 'read', file = 'data/constants.dat', &
	& unit = 50, status = 'old')
    open(action = 'write', file = 'plot_prep.dat', &
	& unit = 60, status = 'new')
	
    read(50, *) ars
    
    allocate(x_c(ars, ars))
    allocate(y_c(ars, ars))
    allocate(u(ars, ars))
    
    do l = 1, ars
        read(20, *) (x_c(k, l), k = 1, ars)
        read(30, *) (y_c(k, l), k = 1, ars)   
        read(40, *) (u(k, l), k = 1, ars)      
    end do
    
    do k = 1, ars
       do l = 1, ars
          write(60, *) x_c(k, l), y_c(k, l), u(k, l)
       end do
    end do
	
end program plot_prep
