program engine

    implicit none
    
    character, allocatable :: a1(:),a2(:),a3(:)
    character, allocatable :: b1(:),b2(:),b3(:)
    character, allocatable :: c1(:),c2(:),c3(:)
    character(len=2) :: selLoc,currPlayer
    character :: selPiece, currBlockedPiece
    logical :: selWrongLoc, game, allGood, selWrongPiece
    logical :: Blocked1, Blocked2, Blocked3,Blocked4,Blocked5,Blocked6,Blocked7,Blocked8
    character :: Blocked1char, Blocked2char, Blocked3char,Blocked4char,Blocked5char,Blocked6char,Blocked7char,Blocked8char
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
    
    a1(1) = "X"; a2(1) = "X"; a3(1) = "X"
    b1(1) = "X"; b2(1) = "X"; b3(1) = "X"    
    c1(1) = "X"; c2(1) = "X"; c3(1) = "X"

    Blocked1 = .false.
    Blocked2 = .false.
    Blocked3 = .false.
    Blocked4 = .false.
    Blocked5 = .false.
    Blocked6 = .false.
    Blocked7 = .false.
    Blocked8 = .false.

    currBlockedPiece = ""
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
    
        if (checkForEnd()) then 
            game = .false.
            cycle
        end if

        call system("clear")
        
        write(*,'(A)',advance = 'no')currPlayer
        write(*,'(A)',advance = 'no')": "
        write(*,'(I0)') currP%points

        write(*,'(A)',advance='no') "Blocked piece: "
        write(*,'(A)') currBlockedPiece
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
            call checkEveryUnblock
            call checkForConnection
            
            round = round + 0.5
        end if

        
    end do

    print*,'Game end'

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
                if ((playr%inventory%bAmnt > 0) .and. (currBlockedPiece/="b")) then 
                    playr%inventory%bAmnt = playr%inventory%bAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    currBlockedPiece = "b"
                    return 
                else 
                    selWrongPiece = .true.
                    res = .false.
                    return
                end if
            case ("y")
                if ((playr%inventory%yAmnt > 0) .and. (currBlockedPiece/="y")) then 
                    playr%inventory%yAmnt = playr%inventory%yAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    currBlockedPiece = "y"
                    return 
                else 
                    selWrongPiece = .true.
                    res = .false.
                    return
                end if
            case ("g")
                if ((playr%inventory%gAmnt > 0) .and. (currBlockedPiece/="g")) then 
                    playr%inventory%gAmnt = playr%inventory%gAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    currBlockedPiece = "g"
                    return 
                else 
                    selWrongPiece = .true.
                    res = .false.
                    return
                end if
            case ("p")
                if ((playr%inventory%pAmnt > 0) .and. (currBlockedPiece/="p")) then 
                    playr%inventory%pAmnt = playr%inventory%pAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    currBlockedPiece = "p"
                    return 
                else 
                    selWrongPiece = .true.
                    res = .false.
                    return
                end if
            case ("r")
                if ((playr%inventory%rAmnt > 0) .and. (currBlockedPiece/="r")) then 
                    playr%inventory%rAmnt = playr%inventory%rAmnt - 1
                    res = .true.
                    selWrongPiece = .false.
                    currBlockedPiece = "r"
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
    subroutine checkForConnection
        if ((a1(size(a1)))==(b1(size(b1))).and.(c1(size(c1)))==(a1(size(a1))).and.(blocked1 .eqv. .false.).and.(a1(size(a1))/="X")) then 
            currP%points = currP%points + 1 
            blocked1 = .true.
            Blocked1char = a1(size(a1))
        end if


        if ((a2(size(a2)))==(b2(size(b2))).and.(c2(size(c2)))==(a2(size(a2))).and.(blocked2 .eqv. .false.).and.(a2(size(a2))/="X")) then 
            currP%points = currP%points + 1 
            blocked2 = .true.
            Blocked2char = a2(size(a2))
        end if


        if ((a3(size(a3)))==(b3(size(b3))).and.(c3(size(c3)))==(a3(size(a3))).and.(blocked3 .eqv. .false.).and.(a3(size(a3))/="X")) then 
            currP%points = currP%points + 1 
            Blocked3 = .true.
            Blocked3char = a3(size(a3))
        end if

        if ((a3(size(a3)))==(b2(size(b2))).and.(c1(size(c1)))==(a3(size(a3))).and.(blocked4 .eqv. .false.).and.(a3(size(a3))/="X")) then 
            currP%points = currP%points + 1 
            Blocked4= .true.
            Blocked4char = a3(size(a3))
        end if


        if ((a1(size(a1)))==(a2(size(a2))).and.(a3(size(a3)))==(a2(size(a2))).and.(blocked5 .eqv. .false.).and.(a1(size(a1))/="X")) then 
            currP%points = currP%points + 1 
            blocked5 = .true.
            Blocked5char = a1(size(a1))
        end if


        if ((b1(size(b1)))==(b2(size(b2))).and.(b3(size(b3)))==(b2(size(b2))).and.(blocked6 .eqv. .false.).and.(b1(size(b1))/="X")) then 
            currP%points = currP%points + 1 
            blocked6 = .true.
            Blocked6char = b2(size(b2))
        end if

        if ((c1(size(c1)))==(c2(size(c2))).and.(c3(size(c3)))==(c2(size(c2))).and.(blocked7 .eqv. .false.).and.(c1(size(c1))/="X")) then 
            currP%points = currP%points + 1 
            blocked7 = .true.
            Blocked7char = c1(size(c1))
        end if

        if ((a1(size(a1)))==(b2(size(b2))).and.(c3(size(c3)))==(a1(size(a1))).and.(blocked8 .eqv. .false.).and.(a1(size(a1))/="X")) then 
            currP%points = currP%points + 1 
            Blocked8 = .true.
            Blocked8char = a1(size(a1))
        end if
    end subroutine

    function checkUnblock(diagonal) result(res)

        character, allocatable :: first(:),second(:),third(:)
        integer :: diagonal
        logical :: res

        integer :: total
        character :: charToCheck

        if (diagonal==1) then 
            charToCheck = Blocked1char
            first = a1 
            second = b1 
            third = c1
        end if

        if (diagonal==2) then 
            charToCheck = Blocked2char
            first = a2 
            second = b2 
            third = c2
        end if

        if (diagonal==3) then 
            charToCheck = Blocked3char
            first = a3 
            second = b3 
            third = c3
        end if

        if (diagonal==4) then 
            charToCheck = Blocked4char
            first = a3 
            second = b2 
            third = c1
        end if

        if (diagonal==5) then 
            charToCheck = Blocked5char
            first = a1 
            second = a2 
            third = a3
        end if

        if (diagonal==6) then 
            charToCheck = Blocked6char
            first = b1 
            second = b2 
            third = b3
        end if


        if (diagonal==7) then 
            charToCheck = Blocked7char
            first = c1 
            second = c2 
            third = c3
        end if

        if (diagonal==8) then 
            charToCheck = Blocked8char
            first = a1 
            second = b2 
            third = c3
        end if


        if (first(size(first)) == charToCheck) then 
            total = total + 1
        end if

        if (second(size(second)) == charToCheck) then 
            total = total + 1
        end if

        if (third(size(third)) == charToCheck) then 
            total = total + 1
        end if

        if (total <= 2) then 
            res = .true.
        end if 
        if (total >= 3) then 
            res = .false.
        end if
        
    end function

    subroutine checkEveryUnblock
        
        if (checkUnblock(1)) then
            blocked1 = .false.
            Blocked1char = ""
        end if
        if (checkUnblock(2)) then
            blocked2 = .false.
            Blocked2char = ""
        end if
        if (checkUnblock(3)) then
            blocked3 = .false.
            Blocked3char = ""
        end if
        if (checkUnblock(4)) then
            blocked4 = .false.
            Blocked4char = ""
        end if
        if (checkUnblock(5)) then
            blocked5 = .false.
            Blocked5char = ""
        end if
        if (checkUnblock(6)) then
            blocked6 = .false.
            Blocked6char = ""
        end if
        if (checkUnblock(7)) then
            blocked7 = .false.
            Blocked7char = ""
        end if
        if (checkUnblock(8)) then
            blocked8 = .false.
            Blocked8char = ""
        end if


    end subroutine


    function checkForEnd() result(res)
        logical :: res

        res = .false.
        
        if ((currP%inventory%pAmnt == 0).and.(currBlockedPiece=="p")) then 
            res = .true.
            return
        end if
        if ((currP%inventory%bAmnt == 0).and.(currBlockedPiece=="b")) then 
            res = .true.
            return
        end if
        if ((currP%inventory%gAmnt == 0).and.(currBlockedPiece=="g")) then 
            res = .true.
            return
        end if
        if ((currP%inventory%yAmnt == 0).and.(currBlockedPiece=="y")) then 
            res = .true.
            return
        end if
        if ((currP%inventory%rAmnt == 0).and.(currBlockedPiece=="r")) then 
            res = .true.
            return
        end if
    end function
end program engine
