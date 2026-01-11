module m_rotations
    use m_error, only: error_t
    implicit none
    private

    integer, parameter, public :: MAX_LINES = 8192
    integer, parameter, public :: MAX_LINE_LENGTH = 128

    integer, parameter, public :: ERROR_READ_FAILED = -1
    integer, parameter, public :: ERROR_INVALID_DIRECTION = -2

    public :: read_rotations, count_zeroes, divrem, count_zero_crossings

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
            read(unit, "(A1,I10)", iostat=iostat) direction, tick_count
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

    pure function count_zeroes(rotations, from, positions) result(res)
        implicit none
        integer, intent(in) :: rotations(:)
        integer, intent(in) :: from
        integer, intent(in) :: positions
        integer :: res
        integer :: i

        integer :: current

        current = modulo(from, positions)
        res = 0
        do i=1, size(rotations)
            current = modulo(current + rotations(i), positions)
            if (current == 0) then
                res = res + 1
            end if
        end do
    end function count_zeroes

    pure function count_zero_crossings(rotations, from, positions) result(res)
        implicit none
        integer, intent(in) :: rotations(:)
        integer, intent(in) :: from
        integer, intent(in) :: positions
        integer :: res
        integer :: i

        integer :: current
        integer :: full_rotations
        integer :: remaining_ticks
        logical :: zero_crossing

        current = modulo(from, positions)
        res = 0
        do i=1, size(rotations)
            full_rotations = abs(rotations(i)) / positions
            remaining_ticks = mod(rotations(i), positions)

            res = res + full_rotations
            zero_crossing = .not. (0 < current + remaining_ticks .and. current + remaining_ticks < positions)
            if (current /= 0 .and. remaining_ticks /= 0 .and. zero_crossing) then
                res = res + 1
            end if

            current = modulo(current + remaining_ticks, positions)
        end do
    end function count_zero_crossings

    pure subroutine divrem(dividend, divisor, quotient, remainder)
        integer, intent(in) :: dividend, divisor
        integer, intent(out) :: quotient, remainder

        if (dividend < 0) then
            quotient = -(-dividend / divisor) - 1
        else
            quotient = dividend / divisor
        end if
        remainder = modulo(dividend, divisor)
    end subroutine divrem
end module m_rotations