#lang racket
( require graphics/graphics )

; config
( define boardSize 8  )
( define boxSize   75 )
( define pieceSize 60 ) 
( define boardXResolution 818 )
( define boardYResolution 600 )
( define defaultEmptyCharacter #\☐ )

; debug
( define prev "♜♞♝♛♚♝♞♜☐♟♟♟♟♟♟♟☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐☐♙♙♙♙♙♙♙♙♖♘♗♕♔♗♘♖" )

( define actual "♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙♙" )

#| "
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
☐☐☐☐☐☐☐☐
" )
|#

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

; TOOLS:

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

    ( if ( even? row )
        (if ( even? strPos ) 
            "white"
            "Dim Gray"
        )
        (if ( even? strPos ) 
            "Dim Gray"
            "white"
        )    
    )
) ; end define getBoxColor

( define ( getFileName chr ) 
    ; character: str (carácter)

    ( cond 
        ( ( char=? #\♜ chr ) "resources/bRook.png" )
        ( ( char=? #\♞ chr ) "resources/bKnight.png" )
        ( ( char=? #\♝ chr ) "resources/bBishop.png" )
        ( ( char=? #\♛ chr ) "resources/bQueen.png" )
        ( ( char=? #\♚ chr ) "resources/bKing.png" )
        ( ( char=? #\♟ chr ) "resources/bPawn.png" )
        ( ( char=? #\♖ chr ) "resources/wRook.png" )
        ( ( char=? #\♘ chr ) "resources/wKnight.png" )
        ( ( char=? #\♗ chr ) "resources/wBishop.png" )
        ( ( char=? #\♕ chr ) "resources/wQueen.png" )
        ( ( char=? #\♔ chr ) "resources/wKing.png" )
        ( ( char=? #\♙ chr ) "resources/wPawn.png" )
        ( ( char=? #\☐ chr ) "resources/emptyBox.png" )
    ) ; end cond
) ; end define getFileName

( define ( itsWhite? chr )
    ; chr: str (character)
    ; return: boolean

    ( or 
        ( char=? #\♖ chr )
        ( char=? #\♘ chr )
        ( char=? #\♗ chr )
        ( char=? #\♕ chr )
        ( char=? #\♔ chr )
        ( char=? #\♙ chr )
    )
) ; end define itsWhite?

; GRAPHICS:

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
                    ( 
                        ( draw-solid-rectangle
                        viewport )
                        ( getBoxPos strX strY boxSize )
                        boxSize boxSize 
                        ( getBoxColor counter getPieceStrCoords boardSize ) 
                    )
                    ( 
                        ( ( draw-pixmap-posn ( getFileName ( string-ref actual counter ) ) ) viewport )
                        ( getPiecePos strX strY boxSize pieceSize getBoxPos )
                    )
                    ( graphBoardAux ( + counter 1 ) )
                ) ; end begin 
            ) ; end if
        ) ; end if
    ) ; end graphBoardAux
    ( graphBoardAux 0 )
) ; end graphBoard

; MOVEMENTS:

#| ( define ( checkRookMove prev strPosTarget strPosPrev ) 
    ; prev: str ( last game )
    ; strPosTarget: int ( position of the target in the string )
    ; strPosPrev: int ( position of the previous pos of the piece in the string )

    ( define-values ( xStrPosTarget yStrPosTarget ) ( getPieceStrCoords strPosTarget boardSize ) )
    ( define-values ( xStrPosPrev yStrPosPrev ) ( getPieceStrCoords strPosPrev boardSize ) )

    ( define ( checkRookMoveAux strPosPrev strPosTarget )
        ( define-values ( xStrPosPrev yStrPosPrev ) ( getPieceStrCoords strPosPrev boardSize ) )
        ( define-values ( xStrPosTarget yStrPosTarget ) ( getPieceStrCoords strPosTarget boardSize ) )

        ( if ( = xStrPosPrev xStrPosTarget )
            ( if ( = yStrPosPrev yStrPosTarget )
                ( void )
                ( if ( < yStrPosPrev yStrPosTarget )
                    ( checkRookMoveAux ( + strPosPrev boardSize ) strPosTarget )
                    ( checkRookMoveAux ( - strPosPrev boardSize ) strPosTarget )
                ) ; end if
            ) ; end if
            ( if ( < xStrPosPrev xStrPosTarget )
                ( checkRookMoveAux ( + strPosPrev 1 ) strPosTarget )
                ( checkRookMoveAux ( - strPosPrev 1 ) strPosTarget )
            ) ; end if
        ) ; end if
    ) ; end define checkRookMoveAux

    ; check if the target position of at least one coord is equal to x or y of the prev
    ( if ( or ( = xStrPosTarget xStrPosPrev ) ( = yStrPosTarget yStrPosPrev ) )
        
        ; make a recursive function that checks in the x-coordinate if it coincides with the target row, if so, start a recursive function that searches frame by frame until it finds if there is a piece (other than the last one) crossed, also with the y-coordinate.
        ( begin

            ( checkRookMoveAux strPosPrev strPosTarget )
        ) ; end begin
        #f 
    )
) |#

#| #| I need to make a function called “checkRookMove” where I check if it is possible for the tower to move to the next address. It returns a boolean value. (point-1) Evaluate yes even if it is in the same x or in the same y, otherwise it is not a valid move (returns false) (point-2) It has an auxiliary recursive function that goes looking, first in the x axis, then in the y axis where the similarity is, and always looks that in the current iteration there is not a piece that blocks the way to the target. |#
( define ( checkRookMove prev strPosPrev strPosTarget color)
    
    ; if that search in the pos of the target if there is a piece of the same color
    ( if ( string=? color "white" ) 
        ( if ( itsWhite? ( string-ref prev strPosTarget ) )
            #f
            #t
        ) ; end if
        ( if ( itsWhite? ( string-ref prev strPosTarget ) )
            #t
            #f
        ) ; end if
    )

    ( define ( checkRookMoveAux strPosPrev strPosTarget counter )
        ( define-values ( xStrPosPrev yStrPosPrev ) ( getPieceStrCoords strPosPrev boardSize ) )
        ( define-values ( xStrPosTarget yStrPosTarget ) ( getPieceStrCoords strPosTarget boardSize ) )



        ( if ( = xStrPosPrev xStrPosTarget )
            ( if ( = yStrPosPrev yStrPosTarget )
                #t
                ( if ( < yStrPosPrev yStrPosTarget)
                    ( if ( char=? ( string-ref prev ( + strPosPrev boardSize ) ) defaultEmptyCharacter )
                        ( checkRookMoveAux (+ strPosPrev boardSize) strPosTarget ( + counter 1 ) )
                        #f
                    )
                    ( if ( char=? ( string-ref prev ( - strPosPrev boardSize ) ) defaultEmptyCharacter )
                        ( checkRookMoveAux (- strPosPrev boardSize) strPosTarget ( + counter 1 ) )
                        #f
                        
                    )
                )
            )
            ( if ( = yStrPosPrev yStrPosTarget )
                ( if ( < xStrPosPrev xStrPosTarget )
                    ( if ( char=? ( string-ref prev ( + strPosPrev 1 ) ) defaultEmptyCharacter )
                        ( checkRookMoveAux ( + strPosPrev 1 ) strPosTarget ( + counter 1 ) )
                        #f
                    )
                    ( if ( char=? ( string-ref prev ( - strPosPrev 1 ) ) defaultEmptyCharacter )
                        ( checkRookMoveAux ( - strPosPrev 1 ) strPosTarget ( + counter 1 ) )
                        #f
                    )
                )
                #f
            )
        )
    )

    ( checkRookMoveAux strPosPrev strPosTarget 0 )
) ; end define checkRookMove |#

(define (can-move-to? board prev-color strPosTarget)
  ;; Verifica si la pieza puede moverse a la posición objetivo
  (cond
    ((char=? (string-ref board strPosTarget) defaultEmptyCharacter) #t) ; Casilla vacía, movimiento válido
    ((char=? (string-ref board strPosTarget) prev-color) #f) ; Misma pieza, movimiento inválido
    (else #t) ; Pieza del color contrario, movimiento válido (y comer)
    )
  )

(define (checkRookMove board strPosPrev strPosTarget prev-color)
  ;; Verifica si la torre puede moverse a la posición objetivo
  (define-values (xStrPosPrev yStrPosPrev) (getPieceStrCoords strPosPrev boardSize))
  (define-values (xStrPosTarget yStrPosTarget) (getPieceStrCoords strPosTarget boardSize))

  (if (or (= xStrPosPrev xStrPosTarget) (= yStrPosPrev yStrPosTarget))
      (let ((can-move? (can-move-to? board prev-color strPosTarget)))
        (if can-move?
            (checkRookMoveAux board strPosPrev strPosTarget 0)
            #f))
      #f)
  )

(define (checkRookMoveAux board strPosPrev strPosTarget counter prev-color)
  ;; Función auxiliar para verificar si la torre puede moverse a la posición objetivo
  (define-values (xStrPosPrev yStrPosPrev) (getPieceStrCoords strPosPrev boardSize))
  (define-values (xStrPosTarget yStrPosTarget) (getPieceStrCoords strPosTarget boardSize))

  (if (or (= xStrPosPrev xStrPosTarget) (= yStrPosPrev yStrPosTarget))
      (if (and (<= 0 xStrPosTarget) (< xStrPosTarget boardSize)
               (<= 0 yStrPosTarget) (< yStrPosTarget boardSize))
          (if (can-move-to? board prev-color strPosTarget)
              (if (= xStrPosPrev xStrPosTarget)
                  (if (< yStrPosPrev yStrPosTarget)
                      (if (char=? (string-ref board (+ strPosPrev boardSize)) defaultEmptyCharacter)
                          (checkRookMoveAux board (+ strPosPrev boardSize) strPosTarget (add1 counter))
                          #f)
                      (if (char=? (string-ref board (- strPosPrev boardSize)) defaultEmptyCharacter)
                          (checkRookMoveAux board (- strPosPrev boardSize) strPosTarget (add1 counter))
                          #f))
                  (if (< xStrPosPrev xStrPosTarget)
                      (if (char=? (string-ref board (+ strPosPrev 1)) defaultEmptyCharacter)
                          (checkRookMoveAux board (+ strPosPrev 1) strPosTarget (add1 counter))
                          #f)
                      (if (char=? (string-ref board (- strPosPrev 1)) defaultEmptyCharacter)
                          (checkRookMoveAux board (- strPosPrev 1) strPosTarget (add1 counter))
                          #f)))
              #f)
          #f)
      #f))

; MAIN:

( define ( main )
    ( open-graphics )
    ( define viewport ( open-viewport "Ajedrez - Andrés Felipe Londoño Bedoya" boardXResolution boardYResolution ) )
    ( graphBoard viewport prev actual ) 
    #| ( define-values (m n) ( getPieceStrCoords 32 boardSize ) )
    ( printf "x: ~a y: ~a\n" m n )
    ( display ( checkRookMove prev 0 7 "white" ) )
    ( newline ) |#

) ; end define main

( main )