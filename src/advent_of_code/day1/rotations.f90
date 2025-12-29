module m_rotations
    use m_error, only: error_t
    implicit none
    private

    integer, parameter, public :: MAX_LINES = 8192
    integer, parameter, public :: MAX_LINE_LENGTH = 128

    integer, parameter, public :: ERROR_READ_FAILED = -1
    integer, parameter, public :: ERROR_INVALID_DIRECTION = -2

    public :: read_rotations, count_zeroes

contains

    subroutine read_rotations(unit, rotations, error)
        implicit none
        integer, intent(in) :: unit
        integer, allocatable, intent(out) :: rotations(:)
        type(error_t), allocatable, intent(out) :: error
        
        character(len=1) :: direction
        character(len=120) :: message
        integer, allocatable :: temp(:)
        integer :: tick_count
        integer :: iostat
        integer :: i

        allocate(temp(MAX_LINES))
        do i=1, MAX_LINES
            read(unit, '(A1,I10)', iostat=iostat) direction, tick_count
            if (iostat < 0) then
                exit ! End of file. Exit loop and assign rotations
            else if (iostat > 0) then
                write(message, "(A,I0)") "Invalid input format on line ", i
                error = error_t(ERROR_READ_FAILED, message)
                return
            end if

            if (direction == "L") then
                temp(i) = -tick_count
            else if (direction == "R") then
                temp(i) = tick_count
            else
                write(message, "(A,A1,A)") "Invalid direction: '", direction, "'"
                error = error_t(ERROR_INVALID_DIRECTION, message)
                return
            end if
        end do

        rotations = temp(1:i-1)
    end subroutine read_rotations

    pure function count_zeroes(rotations, from) result(res)
        implicit none
        integer, intent(in) :: rotations(:)
        integer, intent(in) :: from
        integer :: res
        integer :: i

        integer :: current

        res = 0
        current = from
        do i=1, size(rotations)
            current = current + rotations(i)
            if (current == 0) then
                res = res + 1
            end if
        end do
    end function count_zeroes
end module m_rotations