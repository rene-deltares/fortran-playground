module m_inventory
    use iso_fortran_env, only: int64
    use m_error, only: error_t
    use m_comparison_sort, only: comparable_ref_t, comparable_t, merge_sort_light
    implicit none

    public :: read_inventory

    integer, parameter :: MAX_RANGES = 512
    integer, parameter :: MAX_ITEMS = 1024
    
    integer, public, parameter :: ERROR_FILE_TRUNCATED = -2
    integer, public, parameter :: ERROR_NOT_A_RANGE = -3
    integer, public, parameter :: ERROR_NOT_AN_ITEM = -4

    type, public, extends(comparable_t) :: range_t
        integer(kind=int64) :: begin, end
    contains
        procedure :: in => range_in
        procedure :: compare => range_compare
    end type

    type, public :: inventory_t
        type(range_t), allocatable :: ranges(:)
        integer(kind=int64), allocatable :: items(:)
    end type
contains

    pure elemental function sgn(n) result(res)
        implicit none
        integer(kind=int64), intent(in) :: n
        integer :: res

        res = 0
        if (n < 0_int64) then
            res = -1
        else if (n > 0_int64) then
            res = 1
        end if
    end function

    function range_compare(a, b) result(res)
        implicit none
        class(range_t), intent(in) :: a
        class(*), intent(in) :: b
        integer :: res

        select type(b)
        type is (range_t)
            res = sgn(a%begin - b%begin)
            if (res /= 0) then
                return
            end if
            res = sgn(a%end - b%end)
        class default
            error stop "Type error: Must compare a range to another range."
        end select
    end function range_compare

    elemental function to_range_ref(range) result(res)
        implicit none
        type(range_t), intent(in) :: range
        type(comparable_ref_t) :: res
        res = comparable_ref_t(range)
    end function to_range_ref

    elemental function from_range_ref(comparable) result(res)
        implicit none
        type(comparable_ref_t), intent(in) :: comparable
        type(range_t) :: res

        select type(range => comparable%ref)
        type is (range_t)
            res = range
        class default
            error stop "Type error: Comparable must be a range_t."
        end select
    end function

    subroutine read_inventory(unit, inventory, error)
        implicit none

        integer, intent(in) :: unit
        type(inventory_t), allocatable, intent(out) :: inventory
        type(error_t), allocatable, intent(out) :: error
        type(range_t), allocatable :: range
        type(range_t) :: temp_ranges(MAX_RANGES)
        integer(kind=int64) :: temp_items(MAX_ITEMS)

        integer :: range_count, item_count, iostat
        integer(kind=int64) :: begin, end, item
        character(len=80) :: line

        range_count = 0
        do while(range_count < MAX_RANGES)
            read(unit, "(A)", iostat=iostat), line
            if (iostat < 0) then
                error = error_t(ERROR_FILE_TRUNCATED, "Failed to read a range")
                return
            end if

            if (len_trim(line) == 0) then
                exit
            end if

            call parse_range(line, range, error)
            if (allocated(error)) then
                return
            end if

            range_count = range_count + 1
            temp_ranges(range_count) = range
        end do

        item_count = 0
        do while(item_count < MAX_ITEMS)
            read(unit, "(I16)", iostat=iostat) item
            if (iostat < 0) then
                exit ! End of file
            else if (iostat > 0) then
                error = error_t(ERROR_NOT_AN_ITEM, "Failed to parse integer item")
                return
            end if

            item_count = item_count + 1
            temp_items(item_count) = item
        end do

        inventory = inventory_t( &
            ranges=sort_ranges(temp_ranges(:range_count)), &
            items=temp_items(:item_count) &
        )
    end subroutine

    function sort_ranges(ranges) result(res)
        implicit none
        type(range_t), intent(in) :: ranges(:)
        type(range_t), allocatable :: res(:)

        type(comparable_ref_t), allocatable :: comparables(:)
        type(comparable_ref_t), allocatable :: sorted_comparables(:)

        allocate(comparables(size(ranges)), sorted_comparables(size(ranges)))
        comparables = to_range_ref(ranges)

        call merge_sort_light(comparables, sorted_comparables)

        res = from_range_ref(sorted_comparables)
    end function sort_ranges

    subroutine parse_range(line, range, error)
        implicit none
        character(len=*), intent(in) :: line
        type(range_t), allocatable, intent(out) :: range
        type(error_t), allocatable, intent(out) :: error

        integer :: hyphen_index, iostat
        integer(kind=int64) :: begin, end

        hyphen_index = index(line, "-")
        if (hyphen_index == 0) then
            error = error_t(ERROR_NOT_A_RANGE, "Hyphen not found in range")
            return
        end if

        read(line(:hyphen_index-1), "(I16)", iostat=iostat) begin
        if (iostat /= 0) then
            error = error_t(ERROR_NOT_A_RANGE, "Failed to parse 'begin' integer in range")
            return
        end if

        read(line(hyphen_index+1:), "(I16)", iostat=iostat) end
        if (iostat /= 0) then
            error = error_t(ERROR_NOT_A_RANGE, "Failed to parse 'end' integer in range")
            return
        end if

        if (begin > end) then
            error = error_t(ERROR_NOT_A_RANGE, "Begin must be greater than end")
            return 
        end if

        range = range_t(begin=begin, end=end)
    end subroutine parse_range

    pure elemental function range_in(self, n) result(res)
        implicit none
        class(range_t), intent(in) :: self
        integer(kind=int64), intent(in) :: n
        logical :: res
        res = self%begin <= n .and. n <= self%end
    end function range_in
end module m_inventory