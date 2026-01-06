module m_ranges
    use iso_fortran_env, only: int64, real64
    use m_error, only: error_t
    implicit none
    private

    integer, parameter, public :: MAX_RANGES = 256
    integer, parameter, public :: ERROR_READ_FAILED = -1
    integer, parameter, public :: ERROR_INVALID_RANGE = -2

    integer, parameter :: MAX_STRING_SIZE = 120
    integer, parameter :: MAX_LINE_LENGTH = 4096

    type :: range_t
        integer(kind=int64) :: from
        integer(kind=int64) :: to
    end type range_t

    public :: range_t, range_from_string, read_ranges, is_silly_number, next_silly_number, silly_number_sum, &
        is_extra_silly_number, next_extra_silly_number, extra_silly_number_sum, extra_silly_numbers_in_range
contains
    function range_from_string(str, error) result(range_)
        implicit none
        character(len=*), intent(in) :: str
        type(error_t), allocatable, intent(out) :: error
        type(range_t) :: range_
        integer :: iostat, hyphen_pos
        integer(kind=int64) :: from, to

        hyphen_pos = index(str, "-")
        if (.not. (1 < hyphen_pos .and. hyphen_pos < len(str))) then
            error = error_t(ERROR_READ_FAILED, "Leading, trailing or missing hyphen: " // trim(str))
            return
        end if

        read(str(1:hyphen_pos-1), "(I10)", iostat=iostat) from
        if (iostat /= 0) then
            error = error_t(ERROR_READ_FAILED, "Failed to read `from` in range: " // trim(str(1:hyphen_pos-1)))
            return
        end if

        read(str(hyphen_pos+1:), "(I10)", iostat=iostat) to
        if (iostat /= 0) then
            error = error_t(ERROR_READ_FAILED, "Failed to read `to` in range: " // trim(str(hyphen_pos+1:)))
            return
        end if

        range_ = range_t(from=from, to=to)
    end function range_from_string

    subroutine read_ranges(unit, ranges, error)
        implicit none
        integer, intent(in) :: unit
        type(range_t), allocatable, intent(out) :: ranges(:)
        type(error_t), allocatable, intent(out) :: error

        character(len=MAX_STRING_SIZE) :: message
        character(len=MAX_LINE_LENGTH) :: line
        type(range_t), allocatable :: temp(:)
        type(range_t) :: range_
        integer :: iostat, range_count, current, stride, last

        allocate(temp(MAX_RANGES))
        read(unit, "(A)", iostat=iostat) line
        if (iostat /= 0) then
            error = error_t(ERROR_READ_FAILED, "Read failed")
            return
        end if

        range_count = 0
        current = 1
        last = len_trim(line)
        do while(current < last)
            stride = index(line(current:last), ",")
            if (stride == 0) then
                stride = last - current + 2
            end if
            range_ = range_from_string(line(current:current + stride - 2), error)
            if (allocated(error)) then
                return
            end if
            current = current + stride
            range_count = range_count + 1
            temp(range_count) = range_
        end do
        ranges = temp(1:range_count)
    end subroutine read_ranges

    pure function silly_number_sum(range_) result(res)
        implicit none
        type(range_t), intent(in) :: range_
        integer(kind=int64) :: res
        integer(kind=int64) :: current, next

        res = 0
        if (is_silly_number(range_%from)) then
            res = range_%from
        end if
        current = next_silly_number(range_%from)
        do while (current <= range_%to)
            res = res + current
            current = next_silly_number(current)
        end do
    end function silly_number_sum

    pure function extra_silly_number_sum(range_) result(res)
        implicit none
        type(range_t), intent(in) :: range_
        integer(kind=int64) :: res

        res = sum(extra_silly_numbers_in_range(range_))
    end function extra_silly_number_sum

    pure function next_silly_number(n) result(res)
        implicit none
        integer(kind=int64), intent(in) :: n
        integer(kind=int64) :: res

        integer(kind=int64) :: digits, half_shift, high_nibble, low_nibble

        digits = digit_count(n)
        half_shift = 10 ** (digits / 2)

        ! Odd number of digits, so can't be silly. Return the first silly number with even number of digits.
        if (mod(digits, 2) == 1) then
            res = 10 ** digits + half_shift
            return
        end if
        
        ! Split `n` into "high order" and "low order" nibbles.
        high_nibble = n / half_shift
        low_nibble = mod(n, half_shift)

        if (low_nibble < high_nibble) then
            res = high_nibble * half_shift + high_nibble
            return
        end if

        ! Edge case: `n = 10 ** digits - 1` (even number of repeated 9's) 
        if (high_nibble + 1 == half_shift) then
            res = 10 ** (digits + 1) + half_shift
            return
        end if

        res = (high_nibble + 1) * half_shift + (high_nibble + 1)
    end function next_silly_number

    pure function is_silly_number(n) result(res)
        implicit none
        integer(kind=int64), intent(in) :: n
        logical :: res

        integer(kind=int64) :: digits, half_shift, high_nibble, low_nibble

        digits = digit_count(n)
        ! Odd number of digits, so can't be silly.
        if (mod(digits, 2) == 1) then
            res = .false.
            return
        end if
        
        ! Split `n` into "high order" and "low order" nibbles.
        half_shift = 10 ** (digits / 2)
        high_nibble = n / half_shift
        low_nibble = mod(n, half_shift)

        res = (high_nibble == low_nibble)
    end function is_silly_number

    pure function extra_silly_numbers_in_range(range_) result(res)
        implicit none
        integer, parameter :: MAX_SILLY_NUMBERS = 1024
        type(range_t), intent(in) :: range_
        integer(kind=int64), allocatable :: res(:)

        integer(kind=int64) :: current, next
        integer(kind=int64) :: temp(MAX_SILLY_NUMBERS)
        integer :: count

        count = 0
        if (is_extra_silly_number(range_%from)) then
            count = count + 1
            temp(count) = range_%from
        end if
        current = next_extra_silly_number(range_%from)
        do while (current <= range_%to .and. count < MAX_SILLY_NUMBERS)
            count = count + 1
            temp(count) = current
            current = next_extra_silly_number(current)
        end do
        res = temp(1:count)
    end function extra_silly_numbers_in_range

    pure function next_extra_silly_number(n) result(res)
        implicit none
        integer(kind=int64), intent(in) :: n
        integer(kind=int64) :: res
        
        integer, parameter :: FIRST_SILLY_NUMBER = 11
        integer(kind=int64), parameter :: factors(*) = [2, 3, 5, 7]
        integer(kind=int64) :: digits, high_nibble, min_silly
        integer :: i

        digits = digit_count(n)

        ! First check repeated nines edge case (e.g. 999)
        if (n + 1 == 10 ** digits) then
            do i = 1, size(factors)
                if (mod(digits + 1, factors(i)) == 0) then
                    high_nibble = 10 ** ((digits + 1) / factors(i) - 1)
                    res = repeat_nibble(high_nibble, factors(i))
                    return
                end if
            end do
        end if


        min_silly = huge(0_int64)
        do i = 1, size(factors)
            if (mod(digits, factors(i)) == 0) then
                min_silly = min(min_silly, next_silly_nibbles(n, factors(i)))
            end if
        end do

        res = min_silly
        if (min_silly == huge(0_int64)) then
            res = FIRST_SILLY_NUMBER ! `n` must be a single digit number.
        end if
    end function next_extra_silly_number

    pure function is_extra_silly_number(n) result(res)
        implicit none
        integer(kind=int64), intent(in) :: n
        logical :: res
        
        integer, parameter :: FIRST_SILLY_NUMBER = 11
        integer(kind=int64), parameter :: factors(*) = [2, 3, 5, 7]
        integer(kind=int64) :: shift, digits
        integer :: i

        digits = digit_count(n)
        do i = 1, size(factors)
            if (mod(digits, factors(i)) == 0) then
                res = nibbles_are_silly(n, factors(i))
                if (res) then
                    return
                end if
            end if
        end do
    end function is_extra_silly_number
    
    pure function nibbles_are_silly(n, part_count) result(res)
        implicit none
        integer(kind=int64), intent(in) :: n
        integer(kind=int64), intent(in) :: part_count
        logical :: res

        integer(kind=int64), allocatable :: nibbles(:)
        integer(kind=int64) :: i

        nibbles = split_nibbles(n, part_count)
        do i = 2, size(nibbles)
            if (nibbles(i) /= nibbles(1)) then
                res = .false.
                return
            end if
        end do
        res = .true.
    end function nibbles_are_silly

    pure function next_silly_nibbles(n, part_count) result(res)
        implicit none
        integer(kind=int64), intent(in) :: n
        integer(kind=int64), intent(in) :: part_count
        integer(kind=int64) :: res

        integer(kind=int64), allocatable :: nibbles(:)
        integer(kind=int64) :: shift
        integer :: i

        nibbles = split_nibbles(n, part_count)
        do i = 2, part_count
            if (nibbles(i) < nibbles(1)) then
                res = repeat_nibble(nibbles(1), part_count)
                return
            else if (nibbles(i) > nibbles(1)) then
                res = repeat_nibble(nibbles(1) + 1, part_count)
                return
            end if
        end do
        res = repeat_nibble(nibbles(1) + 1, part_count)
    end function next_silly_nibbles

    pure function split_nibbles(n, part_count) result(nibbles)
        implicit none
        integer(kind=int64), intent(in) :: n
        integer(kind=int64), intent(in) :: part_count
        integer(kind=int64), allocatable :: nibbles(:)

        integer(kind=int64) :: shift, remainder
        integer(kind=int64) :: i

        allocate(nibbles(part_count))

        ! Split n into nibbles
        shift = 10 ** (digit_count(n) / part_count)
        remainder = n
        do i = part_count, 1, -1
            nibbles(i) = mod(remainder, shift)
            remainder = remainder / shift
        end do
    end function split_nibbles

    pure function repeat_nibble(nibble, times) result(res)
        implicit none
        integer(kind=int64), intent(in) :: nibble
        integer(kind=int64), intent(in) :: times
        integer(kind=int64) :: res

        integer :: i
        integer(kind=int64) :: shift

        shift = 10 ** digit_count(nibble)
        res = nibble
        do i = 1, times-1
            res = res * shift + nibble
        end do
    end function repeat_nibble

    pure function join_nibbles(nibbles) result(res)
        implicit none
        integer(kind=int64), intent(in) :: nibbles(:)
        integer(kind=int64) :: res

        integer(kind=int64) :: shift
        integer :: i

        shift = 10 ** digit_count(nibbles(1))
        res = nibbles(1)
        do i = 2, size(nibbles)
            res = res * shift + nibbles(i)
        end do
    end function join_nibbles 

    pure function digit_count(n) result(res)
        implicit none
        integer(kind=int64), intent(in) :: n
        integer(kind=int64) :: res
        res = floor(log10(abs(real(n, kind=real64)))) + 1
    end function digit_count

end module m_ranges