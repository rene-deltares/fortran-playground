module m_joltage
    use iso_fortran_env, only: int64
    use m_error, only: error_t
    implicit none

    integer, parameter, public :: BANK_SIZE = 100
    integer, parameter, public :: MAX_BANKS = 1024
    integer, parameter, public :: ERROR_READ_FAILED = -1
    integer, parameter, public :: ERROR_INVALID_INPUT = -2

    public :: read_battery_banks

contains
    subroutine read_battery_banks(unit, banks, error)
        implicit none
        integer, intent(in) :: unit
        character(len=BANK_SIZE), allocatable, intent(out) :: banks(:)
        type(error_t), allocatable, intent(out) :: error

        character(len=BANK_SIZE) :: temp(MAX_BANKS)
        character(len=BANK_SIZE) :: line
        character(len=120) :: message
        integer :: bank_count, iostat, i

        bank_count = 0
        do while (bank_count < MAX_BANKS)
            read(unit, "(A)", iostat=iostat) line
            if (iostat < 0) then
                exit
            else if (iostat > 0) then
                write(message, "(A,I0)") "Read failed on line: ", bank_count + 1
                error = error_t(ERROR_READ_FAILED, message)
                return
            end if

            if (len_trim(line) /= 100) then
                write(message, "(A,I0,A,I0,A)") "Invalid number of batteries in bank: ", len_trim(line), " (Expected ", BANK_SIZE, ")"
                error = error_t(ERROR_INVALID_INPUT, message)
                return
            end if

            if (.not. all([(line(i:i) >= '1' .and. line(i:i) <= '9', i=1, BANK_SIZE)])) then
                write(message, "(A,I0,A,A)") "Line ", bank_count + 1, " contains invalid character ", line(i:i)
                error = error_t(ERROR_INVALID_INPUT, message)
                return
            end if
            bank_count = bank_count + 1
            temp(bank_count) = line
        end do
        banks = temp(1:bank_count)
    end subroutine read_battery_banks

    pure elemental function joltage(bank, battery_count) result(res)
        implicit none
        character(len=*), intent(in) :: bank
        integer, intent(in) :: battery_count
        integer(kind=int64) :: res

        integer, parameter :: MAX_BATTERIES = 16
        integer :: digit_stack(MAX_BATTERIES)
        integer :: start_index
        integer :: i, batteries_left, current_joltage, max_joltage, max_index

        res = 0
        start_index = 1
        batteries_left = battery_count
        do while (batteries_left > 0)
            max_joltage = 0
            max_index = 0
            do i = start_index, len(bank) - batteries_left + 1
                current_joltage = iachar(bank(i:i)) - iachar('0')
                if (current_joltage <= max_joltage) then
                    cycle
                end if

                ! New maximum joltage.
                max_index = i
                max_joltage = current_joltage
                if (current_joltage == 9) then
                    exit ! Early exit if the max possible joltage is found.
                end if
            end do

            digit_stack(battery_count - batteries_left + 1) = max_joltage

            batteries_left = batteries_left - 1
            start_index = max_index + 1
        end do

        res = 0_int64
        do i = 1, battery_count
            res = res * 10_int64 + int(digit_stack(i), kind=int64)
        end do
    end function

end module m_joltage