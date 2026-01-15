program day4
    use m_error, only: error_t
    use m_scrolls
    implicit none

    call main()
contains
    subroutine main()
        implicit none
        character(len=:), allocatable :: path_to_input_file
        integer :: unit, iostat, rows, columns, round, to_remove_count, total_removed
        logical, allocatable :: grid(:,:)
        type(error_t), allocatable :: error
        integer, allocatable :: coordinates(:, :)
        integer :: corrected(2)

        call get_command_line_arguments(path_to_input_file)
        open(newunit=unit, file=path_to_input_file, status="old", iostat=iostat)
        if (iostat /= 0) then
            print *, "Error opening input file."
            stop 1
        end if

        call read_scrolls(unit, grid, rows, columns, error)
        close(unit)
        if (allocated(error)) then
            print *, error%message
            stop 1
        end if

        round = 1
        total_removed = 0
        do
            call accessible_scrolls(grid, coordinates)
            to_remove_count = size(coordinates, 2)
            if (to_remove_count == 0) then
                exit
            end if

            call remove_scrolls(grid, coordinates)
            total_removed = total_removed + to_remove_count

            print "(A,I0,A,I0)", "Removed ", to_remove_count, " scrolls in round ", round
            round = round + 1
        end do

        print "(A,I0)", "Total removed scrolls: ", total_removed
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
end program day4