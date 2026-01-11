program day3
    use iso_fortran_env, only: int64
    use m_joltage
    use m_error, only: error_t
    implicit none

    call main()
contains
    subroutine main()
        implicit none
        character(len=:), allocatable :: path_to_input_file
        integer(kind=int64), allocatable :: two_battery_joltages(:)
        integer(kind=int64), allocatable :: twelve_battery_joltages(:)
        integer :: i, unit, iostat
        character(len=BANK_SIZE), allocatable :: banks(:)
        type(error_t), allocatable :: error

        call get_command_line_arguments(path_to_input_file)

        open(newunit=unit, file=path_to_input_file, status="old", iostat=iostat)
        if (iostat /= 0) then
            print *, "Error opening input file."
            stop 1
        end if

        call read_battery_banks(unit, banks, error)
        close(unit)
        if (allocated(error)) then
            print "(A,A)", "Error reading input: ", error%message
            stop 1
        end if

        print *, "Two battery joltages:"
        two_battery_joltages = joltage(banks, 2)
        do i = 1, size(banks)
            print "(A,I0,A,I0)", "Bank ", i, " two battery joltage: ", two_battery_joltages(i)
        end do
        print "(A,I0)", "Total two battery joltage sum: ", sum(two_battery_joltages)

        print *, ""
        print *, "Twelve battery joltages:"
        twelve_battery_joltages = joltage(banks, 12)
        do i = 1, size(banks)
            print "(A,I0,A,I0)", "Bank ", i, " twelve battery joltage: ", twelve_battery_joltages(i)
        end do
        print "(A,I0)", "Total twelve battery joltage sum: ", sum(twelve_battery_joltages)
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
end program day3