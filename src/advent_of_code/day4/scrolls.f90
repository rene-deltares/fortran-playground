module m_scrolls
    use m_error, only: error_t
    implicit none

    private
    integer, public, parameter :: MAX_COLUMNS = 256
    integer, public, parameter :: MAX_ROWS = 256

    integer, public, parameter :: MAX_ERROR_MESSAGE_LENGTH = 120
    integer, public, parameter :: ERROR_READ_FAILED = -1
    integer, public, parameter :: ERROR_INVALID_INPUT = -2

    public :: read_scrolls
contains
    subroutine read_scrolls(unit, grid, rows, columns, error)
        implicit none
        integer, intent(in) :: unit
        logical, allocatable, intent(out) :: grid(:,:)
        integer, intent(out) :: rows, columns
        type(error_t), allocatable, intent(out) :: error

        logical :: temp_grid(MAX_ROWS, MAX_COLUMNS)
        character(len=MAX_COLUMNS - 2) :: line
        character(len=MAX_ERROR_MESSAGE_LENGTH) :: message
        integer :: line_count, iostat, i, j

        line_count = 1
        read(unit, "(A)", iostat=iostat) line
        if (iostat < 0) then
            error = error_t(ERROR_INVALID_INPUT, "File empty.")
            return
        else if (iostat > 0) then
            write(message, "(A,I0)") "Read failed on line: ", line_count
            error = error_t(ERROR_READ_FAILED, message)
            return
        end if

        columns = len_trim(line) + 2  ! Two extra columns for padding before the first and after the last column.
        if (columns <= 2) then
            write(message, "(A,I0,A)") "Invalid input: Line ", line_count, " is empty."
            error = error_t(ERROR_INVALID_INPUT, message)
            return
        else if (columns > MAX_COLUMNS) then
            write(message, "(A,I0,A)") "Invalid input: Line ", line_count, " contains too many columns."
            error = error_t(ERROR_INVALID_INPUT, message)
            return
        end if

        temp_grid(:,:) = .false. ! Initialize temp grid to all `.false.` values. 
        temp_grid(2, 2:columns - 1) = [(line(i:i) == "@", i=1, len_trim(line))]
        do while(line_count + 2 < MAX_ROWS)
            read(unit, "(A)", iostat=iostat) line
            if (iostat < 0) then
                exit
            else if (iostat > 0) then
                write(message, "(A,I0)") "Read failed on line: ", line_count + 1
                error = error_t(ERROR_READ_FAILED, message)
                return
            end if
            
            if (len_trim(line) + 2 /= columns) then
                write(message, "(A,I0)") "Wrong number of columns on line: ", line_count + 1
                error = error_t(ERROR_INVALID_INPUT, message)
                return
            end if
            line_count = line_count + 1

            ! Fill temporary grid
            temp_grid(line_count + 1, 2:columns - 1) = [(line(i:i) == "@", i=1, len_trim(line))]
        end do
        rows = line_count + 2 ! Two extra rows for padding before the first and after the last row.

        ! Allocate just enough space to hold the grid, and copy `temp_grid` into it.
        allocate(grid(rows,columns))
        do i=1, columns
            grid(1:rows, i) = temp_grid(1:rows, i)
        end do
    end subroutine read_scrolls
end module m_scrolls