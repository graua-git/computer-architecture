TITLE Project 6     (Proj6_graua.asm)

; Author: Alejandro Grau
; Last Modified: 12/5/2021
; OSU email address: graua@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number:   Project 6         Due Date: 12/5/2021
; Description: This program receives input from the user, validates that they are signed integers, and prints out
;				the list, the sum, and the average. This is done using string primitives and ascii values.

INCLUDE Irvine32.inc

; --------------------------------------------------------------------------------------------------------------
; Name: mGetString
;
; Description: Prompts the user to enter a string, stores string in destination
; 
; Preconditions: Do not use EAX, EDX as arguments
; 
; Receives: 
;		prompt		= text prompt to display for the user 
;		destination = memory location to store the received string
;
; Returns: string stored in destination,
;			length of string stored in ECX
; --------------------------------------------------------------------------------------------------------------
mGetString MACRO prompt, destination
	push	EAX
	push	EDX

	mov		EDX, prompt
	call	WriteString

	mov		EDX, destination
	mov		ECX, 1000
	call	ReadString
	mov		ECX, EAX

	pop		EDX
	pop		EAX
ENDM

; --------------------------------------------------------------------------------------------------------------
; Name: mDisplayString
;
; Description: Prints string given by user
; 
; Receives: 
;		memoryLocation: offset of string to be written
; --------------------------------------------------------------------------------------------------------------
mDisplayString MACRO memoryLocation
	push	EDX

	mov		EDX, memoryLocation
	call	WriteString

	pop		EDX
ENDM

FACTOR		= 10
NUM_UNITS	= 10

.data

; DISPLAY
titleDisplay1		byte	"PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures", 13,10,0
titleDisplay2		byte	"Written by: Alejandro Grau", 13,10,0
initialInstruction1	byte	"Please provide 10 signed decimal integers.", 13,10,0
initialInstruction2 byte	"I will display a list of the integers, their sum, and their average value.", 13,10,0
prompt1				byte	"Please enter a signed number: ", 0
prompt2				byte	"Please try again: ", 0
errorMessage		byte	"ERROR: You did not enter a signed number, or the number was too big", 13,10,0
resultMessage		byte	"You entered the following numbers:", 13,10,0
comma				byte	", ", 0
sumMessage			byte	"The sum of these numbers is: ", 0
averageMessage		byte	"The truncated average is: ", 0
goodbyeMessage		byte	"Farewell!", 13,10,0
; DATA
stringOffset		byte	10 DUP(?)
output				sdword	0
negativeFlag		byte	1
dataArray			sdword	NUM_UNITS DUP(0)
sum					dword	0
average				dword	0

.code
main PROC
	;Display title and instrcutions
		mDisplayString	offset titleDisplay1
		mDisplayString	offset titleDisplay2
		call			CrLf
		mDisplayString	offset initialInstruction1
		mDisplayString	offset initialInstruction2
		call			CrLf

	;----------------------------
	; GET DATA
	;----------------------------
	; Set up loop and register indirect addressing
		mov				ECX, NUM_UNITS
		mov				EDI, offset	dataArray
	_GetDataLoop:
		; Push Parameters
		push			offset  output
		push			offset	stringOffset
		push			offset	errorMessage
		push			offset	prompt2 
		push			offset	prompt1
		call			ReadVal

		; Store output in array
		mov				EAX, output
		mov				[EDI], EAX
		add				EDI, TYPE dataArray
		loop			_GetDataLoop

		call			CrLf
	;----------------------------
	; WRITE DATA & CALCULATE SUM
	;----------------------------
	; Set up loop and reigster indirect addressing
		mov				ECX, NUM_UNITS
		mov				ESI, offset dataArray
		mDisplayString	offset resultMessage
		call			CrLf
	_WriteDataLoop:
		; Display
		push			[ESI]
		push			offset	stringOffset
		call			WriteVal
		; Add to sum
		mov				EAX, [ESI]
		add				sum, EAX
		; Increment and display comma if needed 
		add				ESI, TYPE dataArray
		cmp				ECX, 1
		je				_NoComma
		mDisplayString	offset	comma
		_NoComma:
		loop			_WriteDataLoop

	; Display sum
		call			CrLf
		mDisplayString	offset	sumMessage
		push			sum
		push			offset	stringOffset
		call			WriteVal

	; Calculate and display average
		call			CrLf
		mDisplayString  offset	averageMessage
		mov				EAX, sum
		cdq 
		mov				EBX, NUM_UNITS
		idiv			EBX

		push			EAX
		push			offset	stringOffset
		call			WriteVal

	; Display farewell message
		call			CrLf
		call			CrLf
		mDisplayString	offset	goodbyeMessage
	
	Invoke ExitProcess,0	; exit to operating system
main ENDP

; --------------------------------------------------------------------------------------------------------------
; Name: ReadVal
;
; Description: ReadVal prompts the user to enter a signed integer that would fit in a 32 bit register.
;				It then validates the integer, converts the string to an integer, and stores the int in output.
; 
; Preconditions: Output must be a SDWORD.
;
; Postconditions: Changes to EAX, EBX, ECX, EDX, EBP, ESI, EDI
;					Restores ALL registers once procedure is complete
; 
; Receives: 
;		[EBP + 8] = prompt1
;		[EBP + 12] = prompt2
;		[EBP + 16] = errorMessage
;		[EBP + 20] = input (Memory location to store user string input)
;		[EBP + 24] = output (Memory location and return for the signed integer)
;
; Returns: Output
; --------------------------------------------------------------------------------------------------------------
ReadVal PROC
	push	EBP
	mov		EBP, ESP
	pushad

	; Prompt user and receive input
	mGetString	[EBP + 8], [EBP + 20]
	; Set output
	mov		EDI, [EBP + 24]

	;Validate the value is a number that is fits in a 32bit register 
		mov				EAX, 0				; Clear EAX
	_Validate:
		; Clear output
		mov				[EDI], EAX
		; Set up loop to traverse string
		mov				ESI, [EBP + 20]
		cmp				ECX, 10
		ja				_Error				; Number entered is too big

		lodsb
		cmp				AL, 43				; First digit: +
		je				_Positive
		cmp				AL, 45				; First digit: -
		je				_Negative
		jmp				_StringLoop			; First digit: Anything else
		_Positive:
			sub				ECX, 1
			mov				EDX, 0
			lodsb
			jmp				_StringLoop
		_Negative:
			sub				ECX, 1
			mov				EDX, 1				; EDX = 1: Number is negative
			lodsb
			jmp				_StringLoop

	;Convert string to SDWORD and store it in ouptut
	_StringLoop:
		; Validate that digit's ASCII value is number 0-9
		cmp				AL, 48				; ASCII less than 0
		jb				_Error
		cmp				AL, 57				; ASCII greater than 9
		ja				_Error
		
		; Convert ASCII value to digit
		sub				AL, 48
		
		; Multiply until we get appropriate power of 10
		push			ECX

		sub				ECX, 1
		cmp				ECX, 0				; Edge case last digit
		je				_SkipLoop
		push			EDX
		_PowerOf10Loop:
			mov				EBX, 10
			mul				EBX
			loop			_PowerOf10Loop
			pop				EDX
		_SkipLoop:
			pop				ECX
			; Add digit * 10^(ECX) to output
			add				[EDI], EAX
			; Clear EAX and load next digit
			mov				EAX, 0
			lodsb
		loop			_StringLoop

		jmp				_NoError
		
	;Error, prompt user again
	_Error:
		mDisplayString	[EBP + 16]
		mGetString		[EBP + 12], [EBP + 20]
		
		jmp				_Validate

	_NoError:
	; Edit output if value is negative
	cmp		EDX, 1
	jne		_Finished						; Value is positive, procedure is done
	mov		EDX, [EDI]
	sub		[EDI], EDX						; output = 0
	sub		[EDI], EDX						; output = -output

	_Finished:

	popad
	pop		EBP
	ret		20
ReadVal ENDP

; --------------------------------------------------------------------------------------------------------------
; Name: WriteVal
;
; Description: Receives a numeric value and converts it to a string to print
;				using the macro mDisplayString
; 
; Preconditions: Signed numeric value must fit in a 32 bit register
;
; Postconditions: Changes to EAX, ECX, EBX, EDX, EBP, EDI
;					Restores ALL registers once procedure is complete
; 
; Receives: 
;		[EBP + 8] = stringOffset
;		[EBP + 12] = numeric value to be printed
;
; Returns: None
; --------------------------------------------------------------------------------------------------------------
WriteVal PROC
	push	EBP
	mov		EBP, ESP
	pushad

	; Set up destination
	mov		EDI, [EBP + 8]
	mov		EBX, [EBP + 12]

	; Clear offset
	push		EDI

	mov		ECX, 10
	mov		AL, 0
	rep		stosb

	pop			EDI

	; Check for + or -
	cmp		EBX, 0
	jge		_SkipMinusSign
	mov		AL, 45
	stosb

	; If negative, String now has '-', set value to positive version
	mov		EDX, EBX
	sub		EBX, EDX				; Value = 0
	sub		EBX, EDX				; Value = +Value
	_SkipMinusSign:

	; Determine how long the string needs to be, store length in ECX

	; EAX = Power of 10, EBX = Value, ECX = Length/counter, EDX = multiplicand (10)
	mov		EAX, 10
	mov		ECX, 1
	cmp		EBX, EAX
	jl		_SkipLengthLoop
	_LengthLoop:
		inc		ECX
		mov		EDX, 10
		mul		EDX
		cmp		EBX, EAX
		jg		_LengthLoop
	_SkipLengthLoop:

	; Iterate through string storing values
	; Set up loop, going in reverse order
	add		EDI, ECX 
	std
	; EAX = current value, EBX = divisor (10), ECX = length/counter, EDX = remainder/digit to store in string
	; Continuously divide by 10 to add digits to the string
	mov		EAX, EBX
	mov		EBX, 10
	dec		EDI

	_StoreStringLoop:
		mov		EDX, 0
		div		EBX
		add		EDX, 48					; Get ASCII value
		push	EAX
		mov		EAX, EDX
		stosb

		pop		EAX
		loop	_StoreStringLoop

	; Write String
	mDisplayString [EBP + 8]

	popad
	pop		EBP
	ret		8
WriteVal ENDP

END main
