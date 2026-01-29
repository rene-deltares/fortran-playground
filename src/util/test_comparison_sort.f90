module test_comparison_sort
    use assertions_gtest
    use m_comparison_sort
    implicit none

    type, extends(comparable_t) :: integer_t
        integer :: value
    contains
        procedure :: compare => integer_compare
    end type
contains

    function integer_compare(a, b) result(res)
        implicit none
        class(integer_t), intent(in) :: a
        class(*), intent(in) :: b
        integer :: res

        select type(b)
        type is (integer_t)
            res = a%value - b%value
        class default
            error stop "Type error: Must compare an integer to another integer."
        end select
    end function integer_compare

    elemental function to_int_ref(n) result(res)
        implicit none
        integer, intent(in) :: n
        type(comparable_ref_t) :: res
        res = comparable_ref_t(integer_t(n))
    end function to_int_ref

    function to_int_refs(array) result(res)
        implicit none
        integer, intent(in) :: array(:)
        type(comparable_ref_t), allocatable :: res(:)
        integer :: i
        res = [(to_int_ref(array(i)), i = 1, size(array))]
    end function to_int_refs

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge_sort__reverse, test_merge_sort__reverse,
    subroutine test_merge_sort__reverse() bind(C)
        implicit none
        type(comparable_ref_t), allocatable :: result(:)

        result = merge_sort(to_int_ref([10, 9, 8, 7, 6, 5, 4, 3, 2, 1]))

        call assert_integer_array_eq(result, to_int_ref([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    end subroutine test_merge_sort__reverse
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge_sort__butterfly, test_merge_sort__butterfly,
    subroutine test_merge_sort__butterfly() bind(C)
        implicit none
        type(comparable_ref_t) :: result(16)
        integer :: i

        call merge_sort_light(to_int_ref([1, 9, 5, 13, 3, 11, 7, 15, 2, 10, 6, 14, 4, 12, 8, 16]), result)

        call assert_integer_array_eq(result, to_int_ref([(i, i=1, 16)]))
    end subroutine test_merge_sort__butterfly
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge_sort_light__reverse, test_merge_sort_light__reverse,
    subroutine test_merge_sort_light__reverse() bind(C)
        implicit none
        type(comparable_ref_t) :: result(10)

        call merge_sort_light(to_int_ref([10, 9, 8, 7, 6, 5, 4, 3, 2, 1]), result)

        call assert_integer_array_eq(result, to_int_ref([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    end subroutine test_merge_sort_light__reverse
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge_sort_light__butterfly, test_merge_sort_light__butterfly,
    subroutine test_merge_sort_light__butterfly() bind(C)
        implicit none
        type(comparable_ref_t), allocatable :: result(:)
        integer :: i

        result = merge_sort(to_int_ref([1, 9, 5, 13, 3, 11, 7, 15, 2, 10, 6, 14, 4, 12, 8, 16]))

        call assert_integer_array_eq(result, to_int_ref([(i, i=1, 16)]))
    end subroutine test_merge_sort_light__butterfly
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge__zip, test_merge__zip,
    subroutine test_merge__zip() bind(C)
        implicit none
        type(comparable_ref_t), allocatable :: left(:)
        type(comparable_ref_t), allocatable :: right(:)
        type(comparable_ref_t), allocatable :: result(:)

        ! Arrange
        left = to_int_ref([1, 3, 5, 7, 9])
        right = to_int_ref([2, 4, 6, 8, 10])
        ! Act
        result = merge(left, right)

        ! Assert
        call assert_integer_array_eq(result, to_int_ref([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    end subroutine test_merge__zip
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge__append_left, test_merge__append_left,
    subroutine test_merge__append_left() bind(C)
        implicit none
        type(comparable_ref_t), allocatable :: left(:)
        type(comparable_ref_t), allocatable :: right(:)
        type(comparable_ref_t), allocatable :: result(:)

        ! Arrange
        left = to_int_ref([6, 7, 8, 9, 10])
        right = to_int_ref([1, 2, 3, 4, 5])
        ! Act
        result = merge(left, right)

        ! Assert
        call assert_integer_array_eq(result, to_int_ref([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    end subroutine test_merge__append_left
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge__append_right, test_merge__append_right,
    subroutine test_merge__append_right() bind(C)
        implicit none
        type(comparable_ref_t), allocatable :: left(:)
        type(comparable_ref_t), allocatable :: right(:)
        type(comparable_ref_t), allocatable :: result(:)

        ! Arrange
        left = to_int_ref([1, 2, 3, 4, 5])
        right = to_int_ref([6, 7, 8, 9, 10])

        ! Act
        result = merge(left, right)

        ! Assert
        call assert_integer_array_eq(result, to_int_ref([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    end subroutine test_merge__append_right
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge_light__zip, test_merge_light__zip,
    subroutine test_merge_light__zip() bind(C)
        implicit none
        type(comparable_ref_t) :: result(10)
        type(comparable_ref_t) :: scratch(10)

        ! Arrange
        result = to_int_ref([1, 3, 5, 7, 9, 2, 4, 6, 8, 10])

        ! Act
        call merge_light(result, scratch, 1, 5, 10)

        ! Assert
        call assert_integer_array_eq(result, to_int_ref([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    end subroutine test_merge_light__zip
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge_light__append_left, test_merge_light__append_left,
    subroutine test_merge_light__append_left() bind(C)
        implicit none
        type(comparable_ref_t) :: result(10)
        type(comparable_ref_t) :: scratch(10)

        ! Arrange
        result = to_int_ref([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

        ! Act
        call merge_light(result, scratch, 1, 5, 10)

        ! Assert
        call assert_integer_array_eq(result, to_int_ref([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    end subroutine test_merge_light__append_left
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_comparison_sort, test_merge_light__append_right, test_merge_light__append_right,
    subroutine test_merge_light__append_right() bind(C)
        implicit none
        type(comparable_ref_t) :: result(10)
        type(comparable_ref_t) :: scratch(10)

        ! Arrange
        result = to_int_ref([6, 7, 8, 9, 10, 1, 2, 3, 4, 5])

        ! Act
        call merge_light(result, scratch, 1, 5, 10)

        ! Assert
        call assert_integer_array_eq(result, to_int_ref([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
    end subroutine test_merge_light__append_right
    !$f90tw )

    subroutine assert_integer_array_eq(a, b)
        implicit none
        type(comparable_ref_t), intent(in) :: a(:)
        type(comparable_ref_t), intent(in) :: b(:)

        integer :: i

        call F90_ASSERT_EQ(size(a), size(b), "Arrays are not the same size." // c_null_char)
        do i = 1, size(a)
            select type(a_item => a(i)%ref)
            type is (integer_t)
                select type(b_item => b(i)%ref)
                type is (integer_t)
                    call F90_ASSERT_EQ(a_item%value, b_item%value)
                class default
                    call F90_ASSERT_FALSE(.true., "Item in right array is not an integer" // c_null_char)
                end select
            class default
                call F90_ASSERT_FALSE(.true., "Item in left array is not an integer" // c_null_char)
            end select
        end do
    end subroutine assert_integer_array_eq
end module test_comparison_sort