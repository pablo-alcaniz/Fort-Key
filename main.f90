module data_type
    implicit none
    public

    type :: pass_struct
        character(len=100) :: service, user, save_file
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
    integer :: i, a, ind, j, k, csv_protection
    character(*), parameter :: dictionary = &
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0000111122223333444455556666777788889999!"#$%&()*+-./:<=>?@[\]^_`{|}~'
    character(*), parameter :: special = ',;', file_extension = '.txt'

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

    csv_protection = floor(real(pass%order)/10)
    do i=1,csv_protection
        call random_number(random)
        j = 1 + floor(pass%order*random)
        k = 1 + floor(len(special)*random)
        pass%password(j) = special(k:k)
    enddo

    print*, 'The password is:', pass%password 

    pass%save_file = trim(trim(pass%service)//file_extension)
    open(10, file = pass%save_file)
    write(10,*) 'Service: ', pass%service
    write(10,*) 'User: ', pass%user
    write(10,*) 'Password: ', pass%password
    
    print*, 'Press ENTER to exit'
    read*, 
end program pass_gen