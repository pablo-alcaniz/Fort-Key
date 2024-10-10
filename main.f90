module data_type
    implicit none
    public

    type :: pass_struct
        character(len=40) :: service, user
        character, allocatable :: password(:)
        integer :: order
        logical :: output = .true.
    end type

end module data_type

program pass_gen
    use iso_fortran_env
    use data_type
    implicit none

    type(pass_struct) :: pass
    real(real64) :: random
    integer :: i, a, ind
    character(*), parameter :: dictionary = &
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0000111122223333444455556666777788889999!"#$%&()*+,-./:;<=>?@[\]^_`{|}~'
    character(*), parameter :: special = ',;'

    print*, 'Type the service name:'
    read*, pass%service
    print*, 'Type your user:'
    read*, pass%user
    print*,'Type how long you want your password to be:'
    read*, pass%order

    allocate(pass%password(pass%order))
    
    a = len(dictionary)

    do i=1,pass%order
        call random_number(random)
        ind = 1+floor(a*random)
        pass%password(i) = dictionary(ind:ind)
    enddo

    print*, pass%password

end program pass_gen