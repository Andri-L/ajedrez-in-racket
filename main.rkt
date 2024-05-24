#lang racket
( require graphics/graphics )

; config
( define boardSize 8  )
( define boxSize   75 )
( define pieceSize 60 ) 
( define boardXResolution 818 )
( define boardYResolution 600 )

; debug
( define prev "♜♞♝♛♚♝♞♜♟♟♟♟♟♟♟♟☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐♙♙♙♙♙♙♙♙♖♘♗♕♔♗♘♖" )

( define actual "♜♞♝♛♚♝♞♜♟♟♟♟♟♟♟☐☐☐☐☐☐☐☐♟☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐♙♙♙♙♙♙♙♙♖♘♗♕♔♗♘♖" )

#| "
♜♞♝♛♚♝♞♜
♟♟♟♟♟♟♟♟
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
♙♙♙♙♙♙♙♙
♖♘♗♕♔♗♘♖
" )
|#

( define ( getPieceStrCoords pos boardSize )
    ; pos: int (posicion de valor entero dentro del tablero)
    ; return: (values x y) (equivalencia de la posición en x y)

    ( values
        ( remainder pos boardSize )
        ( quotient pos boardSize )
    ) ; end values
) ; end define getpieceCoords

( define ( getPieceStrPos x y )
    ; x: int ( coordenada horizontal )
    ; y: int ( coordenada vertical )
    ; return: (values x y) (posición en el chess)

   ( + x ( * y 8 ) )
) ; end getPiecePos

( define ( getBoxPos strX strY boxSize #:values? ( values? #f ) )
    ; strX: int ( Coords from the char in the string )
    ; strY: int ( Coords from the chat in the string )
    ; boxSize: int ( Size by default in the cofig )

    ( define xBoxPos ( * strX boxSize ) )
    ( define yBoxPos ( * strY boxSize ) )

    ( if ( boolean=? values? #t ) 
        ( values xBoxPos yBoxPos )
        ( make-posn xBoxPos yBoxPos )
    ) ; end if
) ; end define getBoxPos

( define ( getPiecePos strX strY boxSize pieceSize getBoxPos ) 
    ; strX: int (Coords from the board)
    ; strY: int (Coords from the board)
    ; boxSize: int (Size in pixels of the box)
    ; getBoxPos: procedure (function that return boxPos)
    ; return: (values xPiecePos yPiecePos)

    ( define-values ( xBoxPos yBoxPos ) ( getBoxPos strX strY boxSize #:values? #t ) )

    ( make-posn 
        ; this operation has explication in the docs
        ( + xBoxPos ( + boxSize ( - ( / boxSize 2 ) ) ( - ( / pieceSize 2 ) ) ) ) 
        ( + yBoxPos ( + boxSize ( - ( / boxSize 2 ) ) ( - ( / pieceSize 2 ) ) ) )
    )
) ; end define getPiecePos

( define ( getBoxColor strPos getPieceStrCoords boardSize ) 
    ; strPos: int
    ; getPieceCoords: procedure
    ; return str ("black" or "white")

    ( define-values ( column row ) ( getPieceStrCoords strPos boardSize ) )

    ( if ( even? column )
        (if ( even? strPos ) 
            "white"
            "black"
        )
        (if ( even? strPos ) 
            "black"
            "white"
        )    
    )
) ; end define getBoxColor

( define ( getFileName chr ) 
    ; character: str (carácter)

    ( if ( char=? #\♜ chr )
        "resources/bRook.png"
        ( void )
    )

    ( if ( char=? #\♞ chr )
        "resources/bKnight.png"
        ( void )
    )

    ( if ( char=? #\♝ chr ) 
        "resources/bBishop.png"
        ( void )
    )

    ( if ( char=? #\♛ chr )
        "resources/bQueen.png"
        ( void )
    )

    ( if ( char=? #\♚ chr ) 
        "resources/bKing.png"
        ( void )
    )

    ( if ( char=? #\♟ chr )
        "resources/bPawn.png"
        ( void )
    )

    ( if ( char=? #\♖ chr )
        "resources/wRook.png"
        ( void )
    )

    ( if ( char=? #\♘ chr )
        "resources/wKnight.png"
        ( void )
    )

    ( if ( char=? #\♗ chr )
        "resources/wBishop.png"
        ( void )
    )

    ( if ( char=? #\♕ chr )
        "resources/wQueen.png"
        ( void )
    )

    ( if ( char=? #\♔ chr )
        "resources/wKing.png"
        ( void )
    )

    ( if ( char=? #\♙ chr )
        "resources/wPawn.png"
        ( void )
    )

    ( if ( char=? #\☐ chr )
        "resources/emptyBox.png"
        ( void )
    )

    #|
    ( if ( string=? "TN" chr )
        "resources/bRook.png"
        ( void )
    )

    ( if ( string=? "CN" chr )
        "resources/bKnight.png"
        ( void )
    )

    ( if ( string=? "AN" chr ) 
        "resources/bBishop.png"
        ( void )
    )

    ( if ( string=? "RN" chr )
        "resources/bKing.png"
        ( void )
    )

    ( if ( string=? "PN" chr ) 
        "resources/bPawn.png"
        ( void )
    )

    ( if ( string=? "PB" chr )
        "resources/wPawn.png"
        ( void )
    )

    ( if ( string=? "TB" chr )
        "resources/wRook.png"
        ( void )
    )

    ( if ( string=? "CB" chr )
        "resources/wBishop.png"
        ( void )
    )

    ( if ( string=? "AB" chr )
        "resources/wKnight.png"
        ( void )
    )

    ( if ( string=? "RB" chr )
        "resources/wQueen.png"
        ( void )
    )

    ( if ( string=? "rB" chr )
        "resources/wKing.png"
        ( void )
    )

    "resources/emptyBox.png"
    |#
) ; end define getFileName

( define ( graphBoard viewport prev actual )   
    ; viewport: window struc
    ; prev: str ( previous game ) 
    ; actual: str ( actual game )

    ( define boardLength ( - (string-length actual) 1 ) ; minus 1 because the string starts at 0
    ) ; end define boardLength

    ( define ( graphBoardAux counter )
        ( define-values ( strX strY ) ( getPieceStrCoords counter boardSize ) )

        ( if ( > counter boardLength ) 
            ; if true:
            ( void )
            ; if false:
            ( if ( char=? ( string-ref prev counter ) ( string-ref actual counter ) )
                ( graphBoardAux ( + counter 1 ) )
                ( begin
                    ( ( draw-rectangle
                        viewport )
                        ( getBoxPos strX strY boxSize )
                        boxSize boxSize 
                        ( getBoxColor counter getPieceStrCoords boardSize ) 
                    )
                    ( display ( string-ref actual counter ) )
                    ( display counter )
                    ( sleep 5 )
                    ( ( ( draw-pixmap-posn ( getFileName ( string-ref actual counter ) ) ) viewport ) ( getPiecePos strX strY boxSize pieceSize getBoxPos ) )
                    ( graphBoardAux ( + counter 1 ) )
                ) ; end begin 
            ) ; end if
        ) ; end if
    ) ; end graphBoardAux
    ( graphBoardAux 0 )
) ; end graphBoard

( define ( main )
    ( open-graphics )
    ( define viewport ( open-viewport "Ajedrez - Andrés Felipe Londoño Bedoya" boardXResolution boardYResolution ) )
    ( graphBoard viewport prev actual )
) ; end define main

( main )