program day1
    use m_rotations, only: read_rotations, count_zeroes, count_zero_crossings, MAX_LINES
    use m_error, only: error_t
    implicit none

    call main()
contains
    subroutine main()
        implicit none
        character(len=:), allocatable :: path_to_input_file
        integer :: unit, iostat
        integer, allocatable :: rotations(:)
        type(error_t), allocatable :: error

        call get_command_line_arguments(path_to_input_file)

        open(newunit=unit, file=path_to_input_file, status="old", iostat=iostat)
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
        print "(A,I0)", "Number of zeroes counted: ", count_zeroes(rotations, from=50, positions=100)
        print "(A,I0)", "Number of zero crossings counted: ", count_zero_crossings(rotations, from=50, positions=100)
    end subroutine main

    subroutine get_command_line_arguments(path_to_input_file)
        implicit none
        character(len=:), allocatable, intent(out) :: path_to_input_file
        character(len=1024) :: temp
        integer :: num_args
        integer :: length

        call get_command_argument(0, temp, length=length)
        num_args = command_argument_count()
        if (num_args /= 1) then
            print *, "Usage: " // temp(1:length) // " <path-to-input-file>"
            stop 1
        end if

        call get_command_argument(1, temp, length=length)
        path_to_input_file = temp(1:length)
    end subroutine get_command_line_arguments
end program day1