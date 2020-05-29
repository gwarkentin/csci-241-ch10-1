TITLE Drunkards_Walk_Enhanced 		(Walk2.asm)

; Program: Drunkards_Walk_Enhanced (Chapter 10)
; Description: Improved Irvine's Drunkard's Walk program
; Student:     Gabriel Warkentin
; Date:        04/29/2020
; Class:       CSCI 241
; Instructor:  Mr. Ding

INCLUDE Irvine32.inc
WalkMax = 50
StartX = 39
StartY = 10

DrunkardWalk STRUCT
	path COORD WalkMax DUP(<0,0>)
	pathsUsed WORD 0
DrunkardWalk ENDS

DisplayPosition2 PROTO currX:WORD, currY:WORD
GetStepCount PROTO pathsUsed: PTR WORD

.data
aWalk DrunkardWalk <>
msg BYTE "How many steps for the Drunkard to move (1 - 50): ",0

.code
main14 PROC
	Invoke GetStepCount, OFFSET aWalk.pathsUsed
	mov	esi,OFFSET aWalk
	call TakeDrunkenWalk2
	call CrLf
	call WaitMsg
	call ShowPath
	call CrLf
	call WaitMsg
	exit
main14 ENDP

;-------------------------------------------------------
GetStepCount PROC pathsUsed: PTR WORD
; Get's number of steps to move from user, retrying
; for out of range entries
; Returns: entered value into pathsUsed variable
;-------------------------------------------------------
	mov edx, offset msg
L1:	
	call WriteString
	call ReadDec
	call CrLf
    jc L1
	cmp eax, WalkMax
	ja L1
	cmp eax, 0
	je L1
	mov esi, pathsUsed
	mov [esi], ax
	ret
GetStepCount ENDP

;-------------------------------------------------------
TakeDrunkenWalk2 PROC
;
; Take a walk in random directions (north, south, east,
; west).
; Receives: ESI points to a DrunkardWalk structure
; Returns:  the structure is initialized with random values
;-------------------------------------------------------
	pushad
	movzx ecx,  (DrunkardWalk PTR [esi]).pathsUsed
	mov	edi, esi
	add	edi,OFFSET DrunkardWalk.path
	mov	ebx,StartX
	mov	edx,StartY

Again:
	; Insert current location in array.
	mov	(COORD PTR [edi]).X,bx
	mov	(COORD PTR [edi]).Y,dx

	INVOKE DisplayPosition2, bx, dx
	
Try:
	mov	bx, (COORD PTR [edi]).X				; less dangerous than the
	mov	dx, (COORD PTR [edi]).Y				; pushs and pops I did initially

	mov	  eax,4
	call  RandomRange

	.IF eax == 0		; North
	  dec edx
	.ELSEIF eax == 1	; South
	  inc edx
	.ELSEIF eax == 2	; West
	  dec ebx
	.ELSE				; East (EAX = 3)
	  inc ebx
	.ENDIF

	cmp ebx,StartX
	jne L1
	cmp edx,StartY
	jne L1									; if origin, try again
	jmp Try
L1:
	add	edi,TYPE COORD
	loop	Again

Finish:
	popad
	ret
TakeDrunkenWalk2 ENDP

;-------------------------------------------------------
DisplayPosition2 PROC currX:WORD, currY:WORD
; Display the current X and Y positions.
;-------------------------------------------------------
.data
parenOpen BYTE "(",0
parenClose BYTE ") ",0
commaStr BYTE ",",0
.code
	pushad
	mov edx, OFFSET parenOpen
	call WriteString
	movzx eax,currX
	call WriteDec
	mov	 edx,OFFSET commaStr
	call WriteString
	movzx eax,currY
	call WriteDec
	mov edx, OFFSET parenClose
	call WriteString
	popad
	ret
DisplayPosition2 ENDP

;-------------------------------------------------------
ShowPath PROC
; Displays the path taken by drunken walk graphically
; Receives: esi - points to a DrunkardWalk stucture
;-------------------------------------------------------
	movzx ecx,  (DrunkardWalk PTR [esi]).pathsUsed
	mov	edi, esi
	add	edi,OFFSET DrunkardWalk.path
	mov bl, 'O'
L1:
	mov	DL,BYTE PTR (COORD PTR [edi]).X		; this is terrible. Wish that
	mov	DH,BYTE PTR (COORD PTR [edi]).Y		; coord was byte type
	call DispRevColorChar
	mov eax, 400
	call Delay
	call DispRevColorChar

	add	edi,TYPE COORD						; point to next COORD
	mov bl, '*'								; less efficient but cleaner code than
											; repeating several lines for origin
	loop L1
	ret
ShowPath ENDP

;-------------------------------------------------------
DispRevColorChar PROC
; Display's the given char in opposite color of terminal
; Receives: bl - char to use
; Returns: nothing
;-------------------------------------------------------
	call GotoXY
	call GetTextColor
	ror AL, 4
	call SetTextColor
	mov al, bl
	call WriteChar
	call GotoXY								;looks ugly without this
	ret
DispRevColorChar ENDP
END; main14