program engine
    implicit none
    
    character, allocatable :: a1(:),a2(:),a3(:)
    character, allocatable :: b1(:),b2(:),b3(:)
    character, allocatable :: c1(:),c2(:),c3(:)
    character(len=2) :: selLoc

    allocate(a1(1)); allocate(a2(1)); allocate(a3(1));
    allocate(b1(1)); allocate(b2(1)); allocate(b3(1)); 
    allocate(c1(1)); allocate(c2(1)); allocate(c3(1));





    do while (.true.)

    call printBoard
    write(*,'(A)', advance='no')"Enter location: "
    read(*,*) selLoc

    end do
    
    contains

    pure function pushStone(location, stone) result(res)
        character, allocatable :: res(:)
        character, intent(in) :: location(:), stone
        integer :: resSize

        resSize = size(location)
        allocate(res(resSize + 1))

        res(1:resSize) = location
        res(resSize + 1) = stone

    end function

     subroutine printBoard()
        write(*,'(A)', advance='no') a1(size(a1))
        write(*,'(A)', advance='no') "|"
        write(*,'(A)', advance='no') a2(size(a2))
        write(*,'(A)', advance='no') "|" 
        write(*,'(A)', advance='no') a3(size(a3))
        write(*,'(A)')
        write(*,'(A)', advance='no') b1(size(b1))
        write(*,'(A)', advance='no') "|"
        write(*,'(A)', advance='no') b2(size(b2))
        write(*,'(A)', advance='no') "|" 
        write(*,'(A)', advance='no') b3(size(b3))
        write(*,'(A)')
        write(*,'(A)', advance='no') c1(size(c1))
        write(*,'(A)', advance='no') "|"
        write(*,'(A)', advance='no') c2(size(c2))
        write(*,'(A)', advance='no') "|" 
        write(*,'(A)', advance='no') c3(size(c3))
        write(*,'(A)')



    end subroutine

end program engine
