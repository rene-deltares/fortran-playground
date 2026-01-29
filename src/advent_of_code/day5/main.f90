program day5
    use m_error, only: error_t
    use m_inventory
    implicit none

    call main()
contains
    subroutine main()
        implicit none
        character(len=:), allocatable :: path_to_input_file
        integer :: unit, iostat, fresh_items, i, j

        type(error_t), allocatable :: error
        type(inventory_t), allocatable :: inventory
        type(range_t) :: range

        call get_command_line_arguments(path_to_input_file)
        open(newunit=unit, file=path_to_input_file, status="old", iostat=iostat)
        if (iostat /= 0) then
            print *, "Error opening input file."
            stop 1
        end if

        call read_inventory(unit, inventory, error)
        close(unit)
        if (allocated(error)) then
            print "(A,A)", "Error reading input file: ", error%message
            stop 1
        end if

        do i = 1, size(inventory%ranges)
            range = inventory%ranges(i)
            print "(I0,'-',I0)", range%begin, range%end
        end do

        fresh_items = 0
        do i = 1, size(inventory%items)
            do j = 1, size(inventory%ranges)
                if (inventory%ranges(j)%in(inventory%items(i))) then
                    fresh_items = fresh_items + 1
                    exit
                end if
            end do
        end do
        print *, "Number of fresh items found: ", fresh_items
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
end program