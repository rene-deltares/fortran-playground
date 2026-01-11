module m_error
    implicit none
    private
    public :: error_t

    type :: error_t
        integer :: code
        character(len=:), allocatable :: message
    end type error_t
end module m_error