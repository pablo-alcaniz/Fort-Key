module data_type
    implicit none
    public

    type :: pass_struct
        character(len=40) :: service, user
        character(:), allocatable :: password
        integer :: order
        logical :: output = .true.
    end type

end module data_type

program pass_gen
    use data_type
    implicit none

    type(pass_struct) :: pass
    character(*), parameter :: dictionary = &
    'abcdefghijklmnñopqrstuvwxyzABCDEFGHIJKLMNÑOPQRSTUVWXYZ000111222333444555666777888999!"#$%&()*+,-./:;<=>?@[\]^_`{|}~'
    character(*), parameter :: special = ',;'

    print*, 'Type the service name:'
    read*, pass%service
    print*, 'Type your user:'
    read*, pass%user
    print*,'Type how long you want your password to be:'
    read*, pass%order

    allocate(character(pass%order) :: pass%password)

end program pass_gen