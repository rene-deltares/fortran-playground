program day2
    use iso_fortran_env, only: int64
    use m_ranges
    use m_error, only: error_t
    implicit none

    call main()
contains
    subroutine main()
        implicit none
        character(len=:), allocatable :: path_to_input_file
        integer :: unit, iostat
        type(range_t), allocatable :: ranges(:)
        type(error_t), allocatable :: error
        integer :: i
        integer(kind=int64) :: silly_sum, total_silly_sum
        integer(kind=int64) :: sillier_sum, total_sillier_sum

        call get_command_line_arguments(path_to_input_file)

        open(newunit=unit, file=path_to_input_file, status="old", iostat=iostat)
        if (iostat /= 0) then
            print *, "Error opening input file."
            stop 1
        end if

        call read_ranges(unit, ranges, error)
        close(unit)
        if (allocated(error)) then
            print "(A,I0,A,A)", "Failed to read ranges (", error%code, "): ", error%message
            stop 1
        end if 

        total_silly_sum = 0
        do i = 1, size(ranges)
            silly_sum = sum(silly_numbers_in_range(ranges(i)))
            print "(A,I0,A,I0,A,I0)", "Silly number sum in ", ranges(i)%from, " - ", ranges(i)%to, ": ", silly_sum
            total_silly_sum = total_silly_sum + silly_sum
        end do
        print "(A, I0)", "Total silly number sum: ", total_silly_sum

        print *, ""

        total_sillier_sum = 0
        do i = 1, size(ranges)
            sillier_sum = sum(sillier_numbers_in_range(ranges(i)))
            print "(A,I0,A,I0,A,I0)", "Sillier number sum in ", ranges(i)%from, " - ", ranges(i)%to, ": ", sillier_sum
            total_sillier_sum = total_sillier_sum + sillier_sum
        end do
        print "(A, I0)", "Total sillier number sum: ", total_sillier_sum
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
end program day2