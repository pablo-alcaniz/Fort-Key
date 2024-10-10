module data_type
    implicit none
    public

    type :: pass
        character(len=40) :: service, user
        character(:), allocatable :: password
        integer :: order
    end type
    
end module data_type

program pass_gen
    use data_type
    implicit none

    type(pass) :: prueba

    prueba%order = 20
    prueba%service = 'Amazon'
    prueba%user = 'Pablo'

    allocate(character(prueba%order) :: prueba%password)
    prueba%password = '01234567890123456789'

    print*, prueba%order
    print*, prueba%service
    print*, prueba%user
    print*, prueba%password

end program pass_gen