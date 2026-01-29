module m_comparison_sort
    implicit none
    private
    public :: merge, merge_sort, comparable_t, comparable_ref_t, merge_light, merge_sort_light

    type, abstract :: comparable_t
    contains
        procedure(compare_func), deferred :: compare
    end type
    
    
    abstract interface
    !> Result is negative if `a < b`, `0` if `a == b`, and positive if `a > b`
    function compare_func(a, b) result(res)
        import comparable_t
        class(comparable_t), intent(in) :: a
        !> A `type select` should verify that `b` has the same type as `a`.
        class(*), intent(in) :: b
        integer :: res
    end function
    end interface

    type :: comparable_ref_t
        class(comparable_t), allocatable :: ref
    end type
contains

    recursive function merge_sort(array) result(res)
        class(comparable_ref_t), dimension(:), intent(in) :: array
        class(comparable_ref_t), dimension(:), allocatable :: res

        integer :: pivot

        if (size(array) <= 1) then
            res = array
        else
            pivot = size(array) / 2
            res = merge(merge_sort(array(:pivot)), merge_sort(array(pivot+1:)))
        end if
    end function merge_sort

    function merge(left, right) result(res)
        class(comparable_ref_t), intent(in) :: left(:)
        class(comparable_ref_t), intent(in) :: right(:)
        class(comparable_ref_t), allocatable :: res(:)

        integer :: i, j, k

        allocate(res(size(left) + size(right)))

        i = 1
        j = 1
        k = 1
        do while (i <= size(left) .and. j <= size(right))
            associate (left_item => left(i)%ref, right_item => right(j)%ref)
                if (left_item%compare(right_item) <= 0) then
                    res(k)%ref = left_item
                    i = i + 1
                else
                    res(k)%ref = right_item
                    j = j + 1
                end if
                k = k + 1
            end associate
        end do

        do while (i <= size(left))
            res(k)%ref = left(i)%ref
            i = i + 1
            k = k + 1
        end do

        do while (j <= size(right))
            res(k)%ref = right(j)%ref
            j = j + 1
            k = k + 1
        end do
    end function merge

    subroutine merge_sort_light(array, result)
        implicit none
        class(comparable_ref_t), intent(in) :: array(:)
        class(comparable_ref_t), intent(inout) :: result(:)

        class(comparable_ref_t), allocatable :: scratch(:)

        allocate(scratch(size(array)))
        call merge_sort_light_(array, result, scratch, begin=1, end=size(array))
    end subroutine merge_sort_light

    recursive subroutine merge_sort_light_(array, result, scratch, begin, end)
        implicit none
        class(comparable_ref_t), intent(in) :: array(:)
        class(comparable_ref_t), intent(inout) :: result(:)
        class(comparable_ref_t), intent(inout) :: scratch(:)
        integer, intent(in) :: begin
        integer, intent(in) :: end

        integer :: pivot

        if (begin >= end) then
            result(end)%ref = array(end)%ref
            return
        end if

        pivot = begin + (end - begin + 1) / 2 - 1
        call merge_sort_light_(array, result, scratch, begin, pivot)
        call merge_sort_light_(array, result, scratch, pivot + 1, end)
        call merge_light(result, scratch, begin, pivot, end)
    end subroutine merge_sort_light_

    subroutine merge_light(result, scratch, begin, pivot, end)
        class(comparable_ref_t), intent(inout) :: result(:)
        class(comparable_ref_t), intent(inout) :: scratch(:)
        integer, intent(in) :: begin
        integer, intent(in) :: pivot
        integer, intent(in) :: end

        integer :: i, j, k
        i = begin
        j = pivot + 1
        k = 1
        do while (i <= pivot .and. j <= end)
            associate (left_item => result(i)%ref, right_item => result(j)%ref)
                if (left_item%compare(right_item) <= 0) then
                    scratch(k)%ref = left_item
                    i = i + 1
                else
                    scratch(k)%ref = right_item
                    j = j + 1
                end if
                k = k + 1
            end associate
        end do

        do i = i, pivot
            scratch(k)%ref = result(i)%ref
            k = k + 1
        end do

        do j = j, end
            scratch(k)%ref = result(j)%ref
            k = k + 1
        end do

        do i = 1, end - begin + 1
            result(begin + i - 1)%ref = scratch(i)%ref
        end do
    end subroutine
end module m_comparison_sort