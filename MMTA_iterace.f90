! nepouzivat moznost -fast, rychlost naopak zhorsuje

program iterace

    integer :: k, l, toggle

    !velikost site
    integer :: ars
    
    !casovac
    real :: start, finish

    real :: dr, dt, tcfl, sim_time, sim_final_time

    real, allocatable :: x_c(:,:), y_c(:,:)
    real, allocatable :: u(:,:), u_next(:,:)

    ! konstanta casoveho kroku "cfl"
    print *, "Zadej velikost casoveho kroku (od 0 do 1):"
    read(*,*) tcfl

    ! cas ukonceni simulace
    print *, "Zadej cas ukonceni simulace:"
    read(*,*) sim_final_time

    open(action = 'read', file = 'data/x_points.dat', &
	& unit = 20, status = 'old')
    open(action = 'read', file = 'data/y_points.dat', &
	& unit = 30, status = 'old')
    open(action = 'read', file = 'data/constants.dat', &
	& unit = 50, status = 'old')	


    !nejdriv nacist parametry site a dopocitat casovy krok
    read(50, *) ars, dr, sim_time

    dt = dr * dr * tcfl / 4.

    allocate(x_c(ars, ars))
    allocate(y_c(ars, ars))
    allocate(u(ars, ars))
    allocate(u_next(ars, ars))

    if ((sim_time < sim_final_time) .and. (sim_time /= 0)) then
        print *, "Pokracovat z casu ", sim_time, "? 0 - ne, &
           & 1 - ano"
        read (*, *) toggle
        if (toggle == 0) then
            sim_time = 0
            open(action = 'read', file = 'data/u_values_init.dat', &
	         & unit = 40, status = 'old')
	else
	    open(action = 'read', file = 'data/u_values_final.dat', &
	         & unit = 40, status = 'old')
        end if
    else
        sim_time = 0
        open(action = 'read', file = 'data/u_values_init.dat', &
	    & unit = 40, status = 'old')
    end if
    
    do l = 1, ars
        read(20, *) (x_c(k, l), k = 1, ars)
        read(30, *) (y_c(k, l), k = 1, ars)   
        read(40, *) (u(k, l), k = 1, ars)      
    end do

    close(20)
    close(30)
    close(40)
    close(50)

    
    call cpu_time(start)

    do while (sim_time < sim_final_time)
    ! vnitrek oblasti
        do l = 2, (ars - 1)
            do k = 2, (ars - 1)
            u_next(k,l) = (1 - tcfl) * u(k,l) + tcfl / 4. * &
                & (u(k,l-1) + u(k,l+1) + u(k-1,l) + u(k+1,l)) + &
                & dt * y_c(k,l) * exp(-200. * x_c(k,l) ** 2) * &
                & sin(8 * atan(1.) * sim_time)
            end do
        end do
    ! okrajove podminky (derivace je nulova, 1. rad presnosti)
        do l = 2, (ars - 1)
            u_next(1,l) = u_next(2,l)
            u_next(ars, l) = u_next(ars-1, l)
        end do
        do k = 2, (ars - 1)
            u_next(k,1) = u_next(k,2)
            u_next(k,ars) = u_next(k, ars-1) 
        end do
    !rohy (jen pro uplnost)
        u_next(1,1) = (u_next(1,2) + u_next(2,1)) / 2.
        u_next(1,ars) = (u_next(1, ars-1) + u_next(2, ars)) / 2.
        u_next(ars,1) = (u_next(ars-1, 1) + u_next(ars, 2)) / 2.
        u_next(ars,ars) = (u_next(ars-1,ars) + u_next(ars,ars-1)) / 2.
    !inicializace pristi iterace
        u = u_next
        sim_time = sim_time + dt
    end do
    
    call cpu_time(finish)

    print '("Cas = ",f6.3," sekund")',finish-start
    
    deallocate(x_c)
    deallocate(y_c)
    deallocate(u_next)

    open(action = 'write', file = 'data/constants.dat', &
	& unit = 50, status = 'replace')	
    open(action = 'write', file = 'data/u_values_final.dat', &
	& unit = 60, status = 'replace')

    do l = 1, ars  
        write(60, *) (u(k, l), k = 1, ars)      
    end do
    
    write(50, *) ars, dr, sim_time

    end program iterace
