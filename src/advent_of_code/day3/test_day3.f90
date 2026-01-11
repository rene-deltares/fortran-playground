module test_day3
    use iso_c_binding, only: c_null_char
    use iso_fortran_env, only: int64, int32
    use assertions_gtest
    use m_joltage
    use m_error, only: error_t
    implicit none
contains
    !$f90tw TESTCODE(TEST, test_day3, test_read_battery_banks, test_read_battery_banks,
    subroutine test_read_battery_banks() bind(C)
        implicit none
        integer :: unit
        character(len=BANK_SIZE), allocatable :: banks(:)
        type(error_t), allocatable :: error

        ! Arrange
        open(newunit=unit, status="scratch")
        write(unit, "(A)") "1234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891"
        write(unit, "(A)") "9876543219876543219876543219876543219876543219876543219876543219876543219876543219876543219876543219"
        rewind(unit)

        ! Act
        call read_battery_banks(unit, banks, error)
        close(unit)

        call F90_ASSERT_TRUE(allocated(banks), "Expected `banks` to be allocated" // c_null_char)
        call F90_ASSERT_FALSE(allocated(error), "Expected no error" // c_null_char)
        call F90_ASSERT_EQ(size(banks), 2, "Expected two banks")
        call F90_ASSERT_EQ(len(banks(1)), BANK_SIZE)
        call F90_ASSERT_EQ(len(banks(2)), BANK_SIZE)
        call F90_ASSERT_STREQ(banks(1) // c_null_char, "1234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891" // c_null_char)
        call F90_ASSERT_STREQ(banks(2) // c_null_char, "9876543219876543219876543219876543219876543219876543219876543219876543219876543219876543219876543219" // c_null_char)
    end subroutine test_read_battery_banks
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day3, test_read_battery_banks__bank_not_full, test_read_battery_banks__bank_not_full,
    subroutine test_read_battery_banks__bank_not_full() bind(C)
        implicit none
        integer :: unit
        character(len=BANK_SIZE), allocatable :: banks(:)
        type(error_t), allocatable :: error

        ! Arrange
        open(newunit=unit, status="scratch")
        write(unit, "(A)") "1234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891"
        write(unit, "(A)") "9876543219876543219876543219876543219876543219"
        rewind(unit)

        ! Act
        call read_battery_banks(unit, banks, error)
        close(unit)

        call F90_ASSERT_FALSE(allocated(banks), "Expected `banks` not to be allocated" // c_null_char)
        call F90_ASSERT_TRUE(allocated(error), "Expected an error" // c_null_char)
        call F90_ASSERT_EQ(error%code, ERROR_INVALID_INPUT)
    end subroutine test_read_battery_banks__bank_not_full
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day3, test_read_battery_banks__invalid_joltage, test_read_battery_banks__invalid_joltage,
    subroutine test_read_battery_banks__invalid_joltage() bind(C)
        implicit none
        integer :: unit
        character(len=BANK_SIZE), allocatable :: banks(:)
        type(error_t), allocatable :: error

        ! Arrange
        open(newunit=unit, status="scratch")
        write(unit, "(A)") "1234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891"
        write(unit, "(A)") "987654321987654321987654321987654321987A543219876543219876543219876543219876543219876543219876543219"
        rewind(unit)

        ! Act
        call read_battery_banks(unit, banks, error)
        close(unit)

        call F90_ASSERT_FALSE(allocated(banks), "Expected `banks` not to be allocated" // c_null_char)
        call F90_ASSERT_TRUE(allocated(error), "Expected an error" // c_null_char)
        call F90_ASSERT_EQ(error%code, ERROR_INVALID_INPUT)
    end subroutine test_read_battery_banks__invalid_joltage
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day3, test_joltage, test_joltage,
    subroutine test_joltage() bind(C)
        implicit none

        call F90_ASSERT_EQ(98, int(joltage("987654321111111", 2), kind=int32))
        call F90_ASSERT_EQ(89, int(joltage("811111111111119", 2), kind=int32))
        call F90_ASSERT_EQ(78, int(joltage("234234234234278", 2), kind=int32))
        call F90_ASSERT_EQ(92, int(joltage("818181911112111", 2), kind=int32))

        call F90_ASSERT_TRUE(987654321111 == joltage("987654321111111", 12), "Expected joltage to be 987654321111" // c_null_char)
        call F90_ASSERT_TRUE(811111111119 == joltage("811111111111119", 12), "Expected joltage to be 811111111119" // c_null_char)
        call F90_ASSERT_TRUE(434234234278 == joltage("234234234234278", 12), "Expected joltage to be 434234234278" // c_null_char)
        call F90_ASSERT_TRUE(888911112111 == joltage("818181911112111", 12), "Expected joltage to be 888911112111" // c_null_char)
    end subroutine
    !$f90tw )

end module test_day3