
program engine

    implicit none
    
    character, allocatable :: a1(:),a2(:),a3(:)
    character, allocatable :: b1(:),b2(:),b3(:)
    character, allocatable :: c1(:),c2(:),c3(:)
    character(len=2) :: selLoc,currPlayer
    character :: selPiece
    logical :: selWrongLoc, game, allGood, selWrongPiece
    integer :: i, timesPlayed
    real :: round

    type :: Inventory
        integer :: bAmnt = 10
        integer :: yAmnt = 10
        integer :: gAmnt = 10
        integer :: pAmnt = 10
        integer :: rAmnt = 10
    end type Inventory

    type :: Player
        integer :: points = 0
        type(Inventory) :: inventory
    end type Player

    type(Inventory) :: p1Inv,p2Inv

    type(Player),target :: p1,p2
    type(Player), pointer :: currP

    p1%Inventory = p1Inv
    p2%Inventory = p2Inv

    game = .true.

    allocate(a1(1)); allocate(a2(1)); allocate(a3(1));
    allocate(b1(1)); allocate(b2(1)); allocate(b3(1)); 
    allocate(c1(1)); allocate(c2(1)); allocate(c3(1));

    


    selWrongPiece = .false.
    selWrongLoc = .false.
    round = 0.5
    do while (game)
        if ((selWrongPiece.eqv. .false.) .and. (selWrongLoc.eqv..false.)) then 
            if (round == 0.5) then 
                currPlayer = "p1"
                currP => p1
                timesPlayed = 2
            else 
                if (timesPlayed == 2) then 
                    if (currPlayer=="p2") then 
                        currPlayer = "p1"
                        currP => p1
                        timesPlayed = 1
                    else
                        currPlayer = "p2"
                        currP => p2
                        timesPlayed = 1
                    end if
                else
                    timesPlayed = timesPlayed + 1
                end if
            end if
            
            
        end if
        

        call system("clear")
        
        write(*,'(A)')currPlayer
        call printInv
        if (selWrongLoc .eqv. .true.)  then
            write(*,'(A)') "Error: invalid location"
            selWrongLoc = .false.
        end if
        if (selWrongPiece .eqv. .true.)  then
            write(*,'(A)') "Error: invalid piece"
            selWrongPiece = .false.
        end if
        call printBoard
        write(*,'(A)', advance='no')"Enter location: "
        read(*,*) selLoc
        write(*,'(A)', advance='no')"Enter piece: "
        read(*,*) selPiece
        allGood = HasPieceUpdate(currP,selPiece)
        
        if (allGood) then 
            call resolveInput(selLoc,selPiece)
            round = round + 0.5
        end if

        
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

    function HasPieceUpdate(playr, piece) result(res)
        type(Player) :: playr
        character :: piece
        logical :: res


        select case(piece)
            case ("b")
                if (playr%inventory%bAmnt > 0) then 
                    playr%inventory%bAmnt = playr%inventory%bAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    return 
                else 
                    selWrongPiece = .true.
                    res = .false.
                    return
                end if
            case("y") 
                if (playr%inventory%yAmnt > 0) then 
                    playr%inventory%yAmnt = playr%inventory%yAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    return 
                else 
                    selWrongPiece = .true.
                    res = .false.
                    return
                end if     
            case("g") 
                if (playr%inventory%gAmnt > 0) then 
                    playr%inventory%gAmnt = playr%inventory%gAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    return 
                else 
                    selWrongPiece = .true.
                    res = .false.
                    return
                end if
            case("p") 
                if (playr%inventory%pAmnt > 0) then 
                    playr%inventory%pAmnt = playr%inventory%pAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    return 
                else 
                    selWrongPiece = .true.
                    res = .false.
                    return
                end if   
            case("r") 
                if (playr%inventory%rAmnt > 0) then 
                    playr%inventory%rAmnt = playr%inventory%rAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    return 
                else 
                    selWrongPiece = .true.
                    res = .false.
                    return
                end if
            
            case default
                selWrongPiece = .true.
                res = .false.
                return

        end select
    end function

    subroutine resolveInput(location, stone)
        character(len=2) :: location 
        character :: stone

        select case(location)
            case("a1")
                a1 = pushStone(a1,stone)
            case("a2")
                a2 = pushStone(a2,stone)    
            case("a3")
                a3 = pushStone(a3,stone)    
            case("b1")
                b1 = pushStone(b1,stone)
            case("b2")
                b2 = pushStone(b2,stone)    
            case("b3")
                b3 = pushStone(b3,stone)
            case("c1")
                c1 = pushStone(c1,stone)
            case("c2")
                c2 = pushStone(c2,stone)    
            case("c3")
                c3 = pushStone(c3,stone)
            case default
                selWrongLoc = .true.
        end select

    end subroutine

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

    subroutine printInv()
        write(*,'(A)',advance='no') "b: "
        write(*,'(I0)',advance='no') currP%Inventory%bAmnt
        write(*,'(A)',advance='no') " y: "
        write(*,'(I0)',advance='no') currP%Inventory%yAmnt
        write(*,'(A)',advance='no') " g: "
        write(*,'(I0)',advance='no') currP%Inventory%gAmnt
        write(*,'(A)',advance='no') " p: "
        write(*,'(I0)',advance='no') currP%Inventory%pAmnt
        write(*,'(A)',advance='no') " r: "
        write(*,'(I0)') currP%Inventory%rAmnt




    end subroutine

end program engine
