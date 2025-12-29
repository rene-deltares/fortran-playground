program day1
    use m_rotations, only: read_rotations, MAX_LINES
    use m_error, only: error_t
    implicit none

    call main()
contains
    subroutine main()
        implicit none
        integer :: unit, iostat
        integer, allocatable :: rotations(:)
        type(error_t), allocatable :: error

        open(newunit=unit, file="input.txt", status="old", iostat=iostat)
        if (iostat /= 0) then
            print *, "Error opening input file."
            stop 1
        end if

        call read_rotations(unit, rotations, error)
        close(unit)
        if (allocated(error)) then
            print "(A,I0,A,A)", "Failed to read rotations (", error%code, "): ", error%message
            stop 1
        end if

        print *, rotations
        print "(A,I0)", "Rotation count: ", size(rotations)
    end subroutine main
end program day1