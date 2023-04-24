STACK SEGMENT PARA STACK
	DB 64 DUP(' ')
STACK ENDS

DATA SEGMENT PARA 'DATA'

	WINDOW_WIDTH DW 140h	;the width of the window
	WINDOW_HEIGHT DW 0C8h	;the heuth of the window
		
	VELOCITY_DIRECTION DW 00h ;variable to know the direction of moving, (1 -> move up) (0 -> move down)
	TIME_AUX DB 0 ;variable used when checking if the time is changed
	TIME_AUX_SECOND DB 0 ;variable used when checking if the time is changed
	
	BALL_FIRST_Y DW 0AH ;Y position (line) of the ball when we start at first
	JUMP_HEIGHT DW 01Ah; height the ball jumps
	JUMP DW 0h ;how much jumped
	BALL_SIZE DW 010h;size of the ball (how many pixels does the ball have in width and heigh)
	
	BALL_VELOCITY_X DW 04h ;x (horizontal) velocity of the ball
	BALL_VELOCITY_Y DW 09h ;y (vertical) velocity of the ball. maximum is 9 and minimum is 1
	
	
	PADDLE_LEFT_X DW 32h ; x position of left corner of the paddle
	PADDLE_LEFT_Y DW 96h ; y position of left corner of the paddle
	
	
	PADDLE_WIDTH DW 30h
	PADDLE_HEIGHT DW 5h
	PADDLE_X_ARRAY DW 6 DUP(0)
	PADDLE_Y_ARRAY DW 6 DUP(0)
	ARRAY_START_INDEX DW 0
	ARRAY_END_INDEX DW 0
	delete_paddle_flag DW 0
	
	COLLISION_WITH_PADDLE DW 00h ; to check if ball has collision with ball. (1 -> there is a collision) (0 -> there is no collision)
	
	PADDLE_GENERATE_COUNTER DW 0h ; a counter to generate new paddle after 6 scroll
	
	GAME_OVER_FLAG DW 0; a flag to check if the game is over
	
	SCORE DW 0d; a variable to keep scores
	SCORE_1 DB 0d
	SCORE_2 DB 0d 
	
	RADIUS DW 08h
	X DW 0h
	Y DW 10h
	BALL_CENTER_X DW 0A0h
	BALL_CENTER_Y DW 64h
	DP DW 1h
	
	insect_X DW 0h
	insect_Y DW 0h
	insect_size DW 0Bh
	
	Broken_Paddle_X DW 0h
	Broken_Paddle_Y DW 0h
	
	BROKEN_COLLISION DW 0h
	
	COIL_x DW 0h
	COIL_Y DW 0h

	

DATA ENDS

CODE SEGMENT PARA 'CODE'

	MAIN PROC FAR
	ASSUME CS:CODE,DS:DATA,SS:STACK ;assume as code, data and stack segment the repective registers
	PUSH DS		;push to the stack DS segment
	SUB AX,AX	;clean the AX register
	PUSH AX		;push AX to the stack
	MOV AX,DATA	;save on the AX register the contente of the DATA segment
	MOV DS, AX	;save on the DS segment the contents of the DATA
	POP AX		;release the top item from the stack to the AX register
	POP AX		;release the top item from the stack to the AX register
	CALL CLEAR_SCREEN
		
		call INITIAL_PADDLE
		call Generate_insect
		call Generate_Broken_Paddle
		
		CHECK_TIME_SECOND:
		
			MOV AH, 2Ch		;get the system time
			INT 21h	
			CMP DH,TIME_AUX_SECOND
			JE CHECK_TIME
			MOV TIME_AUX_SECOND,DH	;update time
			
			

			CHECK_TIME:
			MOV AH, 2Ch		;get the system time
			INT 21h			;CH = hour CL = minute DH = second DL = 1/100 seconds	
			CMP DL,TIME_AUX;is the current time equal to the previous one(TIME_AUX)
			JE CHECK_TIME	;if it is the same, check again
			;if it's different, then draw, move ,ec
			MOV TIME_AUX,DL	;update time
			MOV AX,GAME_OVER_FLAG
			CMP AX, 1
			JE GAME_OVER_PAGE
			CALL ERASE_SCORE
			CALL Collision_Insect
			CALL CLEAR_ALL_PADDLES
			CALL CLEAR_BROKEN_PADDLE
			CALL CLEAR_BALL
			CALL Clear_insect
			CALL Move_insect
			CALL MOVE_PADDLES
			CALL MOVE_BROKEN_PADDLE
			CALL DELETE_PADDLES
			CALL MOVE_BALL_HORIZONTALLY
			CALL MOVE_BALL_VERTICALLY
			CALL DRAW_ALL_PADDLES
			CALL DRAW_BALL
			CALL Draw_insect
			CALL SHOW_SCORE
			MOV AX, BROKEN_COLLISION
			CMP BROKEN_COLLISION, 1
			JE CHECK_TIME_SECOND
			CALL DRAW_BROKEN_PADDLE
		   
			
			JMP CHECK_TIME_SECOND
			GAME_OVER_PAGE:
		     CALL GAMINGG_OVER
		
		RET
	MAIN ENDP
	
	
	MOVE_BALL_VERTICALLY PROC NEAR
		
		MOV AX, VELOCITY_DIRECTION ;move up or down the ball
		CMP AX, 0
		JE MOVE_DOWN
		JMP MOVE_UP
		
	
	
		MOVE_UP:       ;ball moves with acceleration
		MOV AX,BALL_VELOCITY_Y
		CMP AX,3 ;min speed
		JNG VELOCITY_MIN
		DEC AX ; if we dont recive the 3 (min speed) again we decrease the speed
		MOV BALL_VELOCITY_Y, AX  ;update BALL_VELOCITY 
		VELOCITY_MIN:   ;if the speed is min
		MOV CX,BALL_CENTER_Y
		SUB CX, AX			;move the ball verticaly ; we decrease the BALL_CENTER_Y in mount of speed
		MOV BALL_CENTER_Y, CX
		MOV BX, JUMP ;in every time that we jump we increase the jump variable beacuse the ball jumps in a certain extend
		INC BX
		MOV JUMP,BX ; update jump
		CMP BX, JUMP_HEIGHT ; we compare the jump with limitation if they are equal we move down
		JG SET_VELOCITY_ZERO	;if the ball reached to it's height then go down
		MOV DX,RADIUS
		CMP BALL_CENTER_Y,0Ah      ; we are in the top of the page
		JNG SET_VELOCITY_ZERO
		
		JMP END_MOVE
		
	  MOVE_DOWN:      
	  ; that is exactly like previous except firs the speed is min then it will be max
		CALL Collision_Broken_Paddle 
		MOV AX,BALL_VELOCITY_Y
		CMP AX, 9
		JGE VELOCITY_MAX
		INC AX
		MOV BALL_VELOCITY_Y, AX
		VELOCITY_MAX:
		ADD BALL_CENTER_Y, AX
		CALL COLLISION_PADDLE
		MOV AX,COLLISION_WITH_PADDLE  ;if we touch the paddle
		CMP AX,1
		JE SET_VELOCITY_ONE ;If we jump as high we go to SET_VELOCITY_ONE
		MOV AX, BALL_CENTER_Y ;if the ball_y is in down of the page we game over
		CMP AX, WINDOW_HEIGHT
		JGE GAME_OVER
		JMP END_MOVE
		
	SET_VELOCITY_ZERO:
		MOV BALL_VELOCITY_Y,1
		MOV VELOCITY_DIRECTION,0
		JMP END_MOVE
		
	SET_VELOCITY_ONE:
		MOV AX,1
		MOV JUMP,0
		MOV VELOCITY_DIRECTION,AX
		MOV BALL_VELOCITY_Y,9
		JMP END_MOVE
	GAME_OVER:
	MOV GAME_OVER_FLAG,1
	
	END_MOVE:
	RET
	MOVE_BALL_VERTICALLY ENDP
	
	MOVE_BALL_HORIZONTALLY PROC NEAR ; move the ball left or right
	
	;check if any key(k or j)  is being pressed (if not exit proc)
	MOV AH,01h
	INT 16h
	JZ END_PROC
	;check which key is being pressed (AL = ascii character)
	MOV AH,00h
	INT 16h 
	;if 'K' is pressed move right
	CMP AL,6Bh
	JE MOVE_BALL_RIGHT
	;if 'J' is pressed move left
	CMP AL,6Ah
	JE MOVE_BALL_LEFT
	
	
	MOVE_BALL_RIGHT:
	MOV AX,BALL_VELOCITY_X
	ADD BALL_CENTER_X, AX ; We increase it according to the acceleration
	MOV BX,WINDOW_WIDTH ;check if the ball is going out of page do nothing
	SUB BX,08h
	CMP BALL_CENTER_X,BX
	JG RIGHT_COLLISION
	JMP END_PROC
	
	MOVE_BALL_LEFT:
	MOV AX,BALL_VELOCITY_X
	SUB BALL_CENTER_X, AX   ; We decrease it according to the acceleration
	MOV BX,08h
	CMP BALL_CENTER_X,BX	;check if the ball is going out of page do nothing
	JL LEFT_COLLISION
	JMP END_PROC
	
	RIGHT_COLLISION:
	MOV AX,BALL_VELOCITY_X ;if we touch the right of the page go back
	SUB BALL_CENTER_X,AX
	JMP END_PROC
	
	LEFT_COLLISION:  ;if we touch the left of the page go back
	MOV AX,BALL_VELOCITY_X
	ADD BALL_CENTER_X,AX
	JMP END_PROC
	
	END_PROC:
		RET
	MOVE_BALL_HORIZONTALLY ENDP
	
	COLLISION_PADDLE PROC NEAR ; we check if we touch the paddle or not
	MOV SI,0
	                   ;we have 6 paddle in page every seconds
	PADDLE_LOOP:  ; its a loop that we get x and y of paddles and check  them.ball has 4 position according to paddle
	MOV AX,PADDLE_X_ARRAY[SI] 
	MOV BX,BALL_CENTER_X
	ADD BX,04h;Ball radius/2
	CMP BX,AX  ;if the down of the ball is above of paddle no collision
	JL NO_COLLISION
	
	MOV BX,BALL_CENTER_X
	SUB BX, 04h ;Ball radius/2
	ADD AX,PADDLE_WIDTH
	CMP BX,AX
	JG NO_COLLISION
	
	MOV AX, PADDLE_Y_ARRAY[SI]
	MOV BX, BALL_CENTER_Y
	ADD BX, 04h ;Ball radius/2
	CMP BX,AX
	JL NO_COLLISION
	
	ADD AX, PADDLE_HEIGHT
	MOV BX,BALL_CENTER_Y
	SUB BX, 04h
	CMP BX,AX
	JG NO_COLLISION
	  ;if all of the coditions are not happend the the ball has collision
	MOV COLLISION_WITH_PADDLE, 1 ;so the the ball has collision to paddles then COLLISION_WITH_PADDLE= 1
	MOV AX, SCORE
	INC AX
	MOV SCORE,AX
	JMP END_PROC3
	NO_COLLISION:
	MOV COLLISION_WITH_PADDLE, 0
	INC SI  ; we increase the index of array of paddles to get position of next paddle to check 
	INC SI
	CMP SI, 0BH ;if we check all array
	JL PADDLE_LOOP
	END_PROC3:
	RET
	COLLISION_PADDLE ENDP
	
	DRAW_BALL PROC NEAR
	MOV SI, 08h ;radius of the ball is 8
	; we call 8 times DRAW_CIRCLE and every time with lower radius
	DRAWING:
	 MOV AX,SI
	 MOV RADIUS,AX
	 MOV Y, AX
	 MOV AX,0
	 MOV X,0
	 MOV AX,1
	 MOV DP, AX
	 CALL DRAW_CIRCLE
	 DEC SI
	 CMP SI,0
	JG DRAWING
	RET
	DRAW_BALL ENDP
	
	
	
	CLEAR_BALL PROC NEAR
	MOV SI, 08h
	; we call 8 times DRAW_CIRCLE and every time with lower radius but with black color
	DRAWING1:
	 MOV AX,SI
	 MOV RADIUS,AX
	 MOV Y, AX
	 MOV AX,0
	 MOV X,0
	 MOV AX,1
	 MOV DP, AX
	 CALL CLEAR_CIRCLE
	 DEC SI
	 CMP SI,0
	JG DRAWING1
	RET
	CLEAR_BALL ENDP
	
	DRAW_PIXEL PROC NEAR ;x is stored in cx and y in DX
		MOV AH,0Ch ;SET CONFIGURATION TO WRITING A PIXEL
		MOV AL,0Ch ;CHOOSE BLUE AS COLOR
		MOV BH,00h ;SET THE PAGE NUMBER
		INT 10h    ;EXECUTE
	RET
	DRAW_PIXEL ENDP
	
	CLEAR_PIXEL PROC NEAR ;x is stored in cx and y in DX
		MOV AH,0Ch ;SET CONFIGURATION TO WRITING A PIXEL
		MOV AL,00h ;CHOOSE BLUE AS COLOR
		MOV BH,00h ;SET THE PAGE NUMBER
		INT 10h    ;EXECUTE
	RET
	CLEAR_PIXEL ENDP
	
	;its a fuction to draw the ball with the midpoint formula
	DRAW_CIRCLE PROC NEAR
	MOV AX,DP
	SUB AX, RADIUS
	MOV DP,AX
	DRAW:
	MOV CX, BALL_CENTER_X
	ADD CX,X
	MOV DX,BALL_CENTER_Y
	ADD DX, Y
	CALL DRAW_PIXEL
	MOV CX, BALL_CENTER_X
	ADD CX, Y
	MOV DX, BALL_CENTER_Y
	ADD DX, X
	CALL DRAW_PIXEL
	MOV CX,BALL_CENTER_X
	SUB CX,Y
	MOV DX, BALL_CENTER_Y
	ADD DX, X
	CALL DRAW_PIXEL
	MOV CX,BALL_CENTER_X
	SUB CX,X
	MOV DX, BALL_CENTER_Y
	ADD DX, Y
	CALL DRAW_PIXEL
	MOV CX,BALL_CENTER_X
	SUB CX,X
	MOV DX, BALL_CENTER_Y
	SUB DX, Y
	CALL DRAW_PIXEL
	MOV CX,BALL_CENTER_X
	SUB CX,Y
	MOV DX, BALL_CENTER_Y
	SUB DX, X
	CALL DRAW_PIXEL
	MOV CX,BALL_CENTER_X
	ADD CX,Y
	MOV DX, BALL_CENTER_Y
	SUB DX,X
	CALL DRAW_PIXEL
	MOV CX,BALL_CENTER_X
	ADD CX,X
	MOV DX, BALL_CENTER_Y
	SUB DX, Y
	CALL DRAW_PIXEL
	MOV CX , DP
	CMP CX,0
	JGE GREATER
	MOV BX, X
	ADD BX,BX
	ADD BX, 1
	ADD CX, BX
	MOV DP, CX
	JMP INC_X
	GREATER:
	MOV BX, Y
	DEC BX
	MOV Y,BX
	ADD BX,BX
	MOV DX, X
	ADD DX,DX
	SUB DX, BX
	INC DX
	ADD CX, DX
	MOV DP, CX
	INC_X:
	MOV BX, X
	INC BX
	MOV X,BX
	MOV DX ,Y
	CMP DX, BX
	JG SHOULD_JUMP 
	JMP END_DRAW
	SHOULD_JUMP:
	JMP FAR PTR DRAW
	END_DRAW:
	
	RET
	DRAW_CIRCLE ENDP
	
	CLEAR_CIRCLE PROC NEAR
	MOV AX,DP
	SUB AX, RADIUS
	MOV DP,AX
	DRAW1:
	MOV CX, BALL_CENTER_X
	ADD CX,X
	MOV DX,BALL_CENTER_Y
	ADD DX, Y
	CALL CLEAR_PIXEL
	MOV CX, BALL_CENTER_X
	ADD CX, Y
	MOV DX, BALL_CENTER_Y
	ADD DX, X
	CALL CLEAR_PIXEL
	MOV CX,BALL_CENTER_X
	SUB CX,Y
	MOV DX, BALL_CENTER_Y
	ADD DX, X
	CALL CLEAR_PIXEL
	MOV CX,BALL_CENTER_X
	SUB CX,X
	MOV DX, BALL_CENTER_Y
	ADD DX, Y
	CALL CLEAR_PIXEL
	MOV CX,BALL_CENTER_X
	SUB CX,X
	MOV DX, BALL_CENTER_Y
	SUB DX, Y
	CALL CLEAR_PIXEL
	MOV CX,BALL_CENTER_X
	SUB CX,Y
	MOV DX, BALL_CENTER_Y
	SUB DX, X
	CALL CLEAR_PIXEL
	MOV CX,BALL_CENTER_X
	ADD CX,Y
	MOV DX, BALL_CENTER_Y
	SUB DX,X
	CALL CLEAR_PIXEL
	MOV CX,BALL_CENTER_X
	ADD CX,X
	MOV DX, BALL_CENTER_Y
	SUB DX, Y
	CALL CLEAR_PIXEL
	MOV CX , DP
	CMP CX,0
	JGE GREATER1
	MOV BX, X
	ADD BX,BX
	ADD BX, 1
	ADD CX, BX
	MOV DP, CX
	JMP INC_X1
	GREATER1:
	MOV BX, Y
	DEC BX
	MOV Y,BX
	ADD BX,BX
	MOV DX, X
	ADD DX,DX
	SUB DX, BX
	INC DX
	ADD CX, DX
	MOV DP, CX
	INC_X1:
	MOV BX, X
	INC BX
	MOV X,BX
	MOV DX ,Y
	CMP DX, BX
	JG SHOULD_JUMP1 
	JMP END_DRAW1
	SHOULD_JUMP1:
	JMP FAR PTR DRAW1
	END_DRAW1:
	
	RET
	CLEAR_CIRCLE ENDP
	
	INITIAL_PADDLE PROC NEAR ;we draw first 6 paddles in page 
	mov si,0h
	mov ax, 20h
	mov bx,0c0h
	MOV PADDLE_X_ARRAY[si],ax
	mov PADDLE_Y_ARRAY[si],bx
	
	mov si,2h
	mov ax, 0A0h
	mov bx,0A0h
	MOV PADDLE_X_ARRAY[si],ax
	mov PADDLE_Y_ARRAY[si],bx
	
	mov si,4h
	mov ax, 30h
	mov bx,090h
	MOV PADDLE_X_ARRAY[si],ax
	mov PADDLE_Y_ARRAY[si],bx
	
	mov si,6h
	mov ax, 0C0h
	mov bx,070h
	MOV PADDLE_X_ARRAY[si],ax
	mov PADDLE_Y_ARRAY[si],bx
	
	mov si,8h
	mov ax, 100h
	mov bx,050h
	MOV PADDLE_X_ARRAY[si],ax
	mov PADDLE_Y_ARRAY[si],bx
	
	mov si,0Ah
	mov ax, 05h
	mov bx,030h
	MOV PADDLE_X_ARRAY[si],ax
	mov PADDLE_Y_ARRAY[si],bx
	
	
	RET
	INITIAL_PADDLE ENDP
	
	
	DRAW_ALL_PADDLES PROC NEAR
	
		MOV SI, 0
		DRAW_ONE_PADDLE:
		MOV CX,PADDLE_X_ARRAY[SI]
		MOV DX,PADDLE_Y_ARRAY[SI]
		 ; in two arrays we draw the paddles
		DRAW_PADDLE_HORIZONTAL:
			MOV AH,0Ch ;SET CONFIGURATION TO WRITING A PIXEL
			MOV AL,09h ;CHOOSE WHITE AS COLOR
			MOV BH,00h ;SET THE PAGE NUMBER
			INT 10h    ;EXECUTE
	  
			INC CX
			MOV AX,CX ;CX-PADDLE_LEFT>PADDLE_WI -> GO TO NEXT LINE
			SUB AX,PADDLE_X_ARRAY[SI]
			CMP AX,PADDLE_WIDTH
			JNG DRAW_PADDLE_HORIZONTAL
	  
			MOV CX,PADDLE_X_ARRAY[SI]
			INC DX
			MOV AX,DX
			SUB AX,PADDLE_Y_ARRAY[SI]
			CMP AX,PADDLE_HEIGHT
			JNG DRAW_PADDLE_HORIZONTAL
		
		
		INC SI
		INC SI
		CMP SI, 0Bh
		JL DRAW_ONE_PADDLE
		RET
	DRAW_ALL_PADDLES ENDP
	
	CLEAR_ALL_PADDLES PROC NEAR
	    ; like drawing paddle but with color of background
		MOV SI, 0h
		CLEAR_ONE_PADDLE:
		MOV CX,PADDLE_X_ARRAY[SI]
		MOV DX,PADDLE_Y_ARRAY[SI]
		CLEAR_PADDLE_HORIZONTAL:
			MOV AH,0Ch ;SET CONFIGURATION TO WRITING A PIXEL
			MOV AL,00h ;CHOOSE WHITE AS COLOR
			MOV BH,00h ;SET THE PAGE NUMBER
			INT 10h    ;EXECUTE
	  
			INC CX
			MOV AX,CX ;CX-PADDLE_LEFT>PADDLE_WI -> GO TO NEXT LINE
			SUB AX,PADDLE_X_ARRAY[SI]
			CMP AX,PADDLE_WIDTH
			JNG CLEAR_PADDLE_HORIZONTAL
	  
			MOV CX,PADDLE_X_ARRAY[SI]
			INC DX
			MOV AX,DX
			SUB AX,PADDLE_Y_ARRAY[SI]
			CMP AX,PADDLE_HEIGHT
			JNG CLEAR_PADDLE_HORIZONTAL
		
		INC SI
		INC SI
		CMP SI, 0Bh
		JL CLEAR_ONE_PADDLE
		RET
	CLEAR_ALL_PADDLES ENDP
	
	NEW_PADDLE_TOP PROC NEAR ;from the top of page draw new paddle that y is 0 but x is random
	MOV CX , 140h
	sub cx, PADDLE_WIDTH ; we set cx =page width-PADDLE_WIDTH that paddle not draw in corner of page
	call GENERATE_RANDOM ;this funcion is make random number in range of 0 to cx ; 
	MOV PADDLE_X_ARRAY[SI], DX
	mov bx, 0 
	mov PADDLE_Y_ARRAY[si],bx
	
	RET
	NEW_PADDLE_TOP ENDP
	
	NEW_PADDLE_RANDOM PROC NEAR ;a paddle with random x , y
	mov cx, 140h
	sub cx, PADDLE_WIDTH
	call GENERATE_RANDOM
	mov PADDLE_X_ARRAY[si],DX
	mov cx,PADDLE_Y_ARRAY[di]
	call GENERATE_RANDOM
	mov PADDLE_Y_ARRAY[si],DX
	
	RET
	NEW_PADDLE_RANDOM ENDP
	
	
	MOVE_PADDLES PROC NEAR    ;we navigate the paddle array and decraese thier y's 
	mov si, 0
	MOVE:
	MOV ax, PADDLE_Y_ARRAY[si]
	inc ax
	mov PADDLE_Y_ARRAY[si],ax
	inc si
	inc si
	cmp si,0Bh
	jl MOVE
	
	RET
	MOVE_PADDLES ENDP
	
	DELETE_PADDLES PROC NEAR ; in this function we check thath the paddle that is in the down delete it and draw new one from up
	mov si, 0
	CHECK:  ;we navigate the paddle array and compare their y's with page y(WINDOW_HEIGHT)
	MOV ax, PADDLE_Y_ARRAY[si]
	mov bx, WINDOW_HEIGHT
	sub bx,PADDLE_HEIGHT
	cmp ax, bx ; paddle y <page y => nothing =>no delete
	jl NO_DLETE
	push si   ;if paddle y > page y => delete ,save si
	CALL NEW_PADDLE_TOP ;we  set new paddle in si index(instead of deleting paddle)
	pop si
	NO_DLETE:
	inc si
	inc si
	cmp si,0Bh
	jl CHECK
	
	RET
	DELETE_PADDLES ENDP
	 
	 GENERATE_RANDOM PROC NEAR   ; CX INPUT AND DX OUTPUT, DX = A random number in [0, CX) Interval
      PUSH CX
      MOV AH,2Ch 					 ;get the system time
	  INT 21h    					 ;CH = hour CL = minute DH = second DL = 1/100 seconds
      MOV AL, DH
      MOV AH, 0
      MOV BL, DL
      MUL BL          ; AX = PSEDUO-RANDOM NUMBER
      POP CX
      CMP CX, 0
	 JE GENERATE_RANDOM_RET
     MOV DX, 0 
     DIV CX          ; DX = RANDOM % CX 
     
    GENERATE_RANDOM_RET:
     RET
	GENERATE_RANDOM ENDP
	
	Generate_insect PROC NEAR
	
		MOV insect_Y, 0h
		MOV CX, WINDOW_WIDTH
		SUB CX, insect_size
		CALL GENERATE_RANDOM
		MOV insect_X,DX
		RET
	Generate_insect ENDP
	
	Move_insect PROC NEAR  ;for die_insect => draw new insect with random x from the top of page .its like drawing paddles
	                       ; move the insect to down
		MOV AX, insect_Y
		INC AX
		MOV insect_Y, AX
		MOV BX, WINDOW_HEIGHT
		SUB BX, 10h
		CMP AX, BX
		JL end_prc
		CALL Generate_insect
		end_prc:
	RET
	Move_insect ENDP

	Draw_insect PROC NEAR
	
		MOV CX,insect_X
		MOV DX,insect_Y
		DRAW_INSECT_HORIZONTAL:
			MOV AH,0Ch ;SET CONFIGURATION TO WRITING A PIXEL
			MOV AL,0Dh ;CHOOSE COLOR
			MOV BH,00h ;SET THE PAGE NUMBER
			INT 10h    ;EXECUTE
	  
			INC CX
			MOV AX,CX
			SUB AX,insect_X
			CMP AX,insect_size
			JNG DRAW_INSECT_HORIZONTAL
	  
			MOV CX,insect_X
			INC DX
			MOV AX,DX
			SUB AX,insect_Y
			CMP AX,insect_size
			JNG DRAW_INSECT_HORIZONTAL
	
	
	RET
	Draw_insect ENDP
	
	Clear_insect PROC NEAR
	
		MOV CX,insect_X
		MOV DX,insect_Y
		CLEAR_INSECT_HORIZONTAL:
			MOV AH,0Ch ;SET CONFIGURATION TO WRITING A PIXEL
			MOV AL,00h ;CHOOSE COLOR
			MOV BH,00h ;SET THE PAGE NUMBER
			INT 10h    ;EXECUTE
	  
			INC CX
			MOV AX,CX
			SUB AX,insect_X
			CMP AX,insect_size
			JNG CLEAR_INSECT_HORIZONTAL
	  
			MOV CX,insect_X
			INC DX
			MOV AX,DX
			SUB AX,insect_Y
			CMP AX,insect_size
			JNG CLEAR_INSECT_HORIZONTAL
	
	
	RET
	Clear_insect ENDP
	
	Collision_Insect PROC NEAR
	
	MOV BX,insect_Y
	ADD BX,insect_size
	MOV CX, BALL_CENTER_Y
	SUB CX, RADIUS
	CMP CX, BX
	JG NO_COLL
	
	MOV BX,insect_Y
	MOV CX, BALL_CENTER_Y
	ADD CX, RADIUS
	CMP CX, BX
	JL NO_COLL
	
	MOV BX, insect_X
	ADD BX, insect_size
	MOV CX, BALL_CENTER_X
	SUB CX, RADIUS
	CMP CX, BX
	JG NO_COLL
	
	MOV BX, insect_X
	MOV CX, BALL_CENTER_X
	ADD CX, RADIUS
	CMP CX, BX
	JL NO_COLL
	MOV GAME_OVER_FLAG, 1
	
	NO_COLL:
	
	RET
	Collision_Insect ENDP
	
	Generate_Broken_Paddle PROC
	MOV BROKEN_COLLISION,0
	MOV CX , 140h
	SUB CX , PADDLE_WIDTH
	CALL GENERATE_RANDOM
	MOV Broken_Paddle_X,DX
	MOV BX, 0h
	MOV Broken_Paddle_Y,BX
	 
	
	RET
	Generate_Broken_Paddle ENDP
	
	DRAW_BROKEN_PADDLE PROC
	
		MOV CX,Broken_Paddle_X
		MOV DX,Broken_Paddle_Y
		DRAW_PADDLE:
			MOV AH,0Ch ;SET CONFIGURATION TO WRITING A PIXEL
			MOV AL,06h ;CHOOSE WHITE AS COLOR
			MOV BH,00h ;SET THE PAGE NUMBER
			INT 10h    ;EXECUTE
	  
			INC CX
			MOV AX,CX ;CX-PADDLE_LEFT>PADDLE_WI -> GO TO NEXT LINE
			SUB AX,Broken_Paddle_X
			CMP AX,PADDLE_WIDTH
			JNG DRAW_PADDLE
	  
			MOV CX,Broken_Paddle_X
			INC DX
			MOV AX,DX
			SUB AX,Broken_Paddle_Y
			CMP AX,PADDLE_HEIGHT
			JNG DRAW_PADDLE
		RET
	DRAW_BROKEN_PADDLE ENDP
	
	CLEAR_BROKEN_PADDLE PROC
	
		MOV CX,Broken_Paddle_X
		MOV DX,Broken_Paddle_Y
		CLEAR_BROKEN:
			MOV AH,0Ch ;SET CONFIGURATION TO WRITING A PIXEL
			MOV AL,00h ;CHOOSE WHITE AS COLOR
			MOV BH,00h ;SET THE PAGE NUMBER
			INT 10h    ;EXECUTE
	  
			INC CX
			MOV AX,CX ;CX-PADDLE_LEFT>PADDLE_WI -> GO TO NEXT LINE
			SUB AX,Broken_Paddle_X
			CMP AX,PADDLE_WIDTH
			JNG CLEAR_BROKEN
	  
			MOV CX,Broken_Paddle_X
			INC DX
			MOV AX,DX
			SUB AX,Broken_Paddle_Y
			CMP AX,PADDLE_HEIGHT
			JNG CLEAR_BROKEN
		RET
	CLEAR_BROKEN_PADDLE ENDP
	
	MOVE_BROKEN_PADDLE PROC
	MOV AX, Broken_Paddle_Y
		INC AX
		MOV Broken_Paddle_Y, AX
		MOV BX, WINDOW_HEIGHT
		SUB BX, PADDLE_HEIGHT
		CMP AX, BX
		JL endProc
		CALL Generate_Broken_Paddle
		
		endProc:
	RET
	MOVE_BROKEN_PADDLE ENDP
	
	Collision_Broken_Paddle PROC
	
	MOV BX,Broken_Paddle_Y
	ADD BX,PADDLE_HEIGHT
	MOV CX, BALL_CENTER_Y
	SUB CX, RADIUS
	CMP CX, BX
	JG NO_COLLI
	
	MOV BX,Broken_Paddle_Y
	MOV CX, BALL_CENTER_Y
	ADD CX, RADIUS
	CMP CX, BX
	JL NO_COLLI
	
	MOV BX, Broken_Paddle_X
	ADD BX, PADDLE_WIDTH
	MOV CX, BALL_CENTER_X
	SUB CX, RADIUS
	CMP CX, BX
	JG NO_COLLI
	
	MOV BX, Broken_Paddle_X
	MOV CX, BALL_CENTER_X
	ADD CX, RADIUS
	CMP CX, BX
	JL NO_COLLI
	MOV BROKEN_COLLISION, 1
	
	NO_COLLI:
	
	RET
	Collision_Broken_Paddle ENDP
	
	GENERATE_COIL PROC
	RET
	GENERATE_COIL ENDP
	
	DRAW_COIL PROC NEAR
	RET
	DRAW_COIL ENDP
	
	CLEAR_COIL PROC
	
	RET
	CLEAR_COIL ENDP
	
	MOV_COIL PROC
	
	RET
	MOV_COIL ENDP
	
	
	
	CLEAR_SCREEN PROC NEAR
	
			MOV AH, 00h ;set the configuration to video mode
			MOV AL,13h  ;choose the video mode
			INT 10h
			
			MOV AH, 6 ;set the configuration to the background color
			MOV AL, 0
			MOV BH, 00h;choose black as background
			MOV CX, 0
			MOV DX, 0FFC8h 
			INT 10h    					 ;execute the configuration    
   
			
		RET
	CLEAR_SCREEN ENDP
  
    SHOW_SCORE PROC NEAR
      MOV AH,02h
	  MOV DH,00h   ;set the cursor
	  MOV DL,36d
	  INT 10h
	  
	  
	  MOV AX,00d
	  MOV DX,00d   ;reseting registers
	  MOV BX,0d
	  
	
          ;converting score to ascii code to show
		
      MOV AX,SCORE
	   MOV BX ,10d
	   DIV BX
	   MOV BX,DX
	;  
       mov dl,AL
       add dl, 48 
	   MOV SCORE_1,dl
       mov ah,02h
       int 21h
	 
	  MOV AX,00d
	  MOV DX,00d
	  
	  
      MOV dl,BL
      add dl, 48 
	  MOV SCORE_2,dl
      mov ah,02h
      int 21h	
	 
	  
	
	 RET
    SHOW_SCORE ENDP	
   

   ERASE_SCORE PROC NEAR
        MOV AH,02h
	    MOV DH,00h
	    MOV DL,00h
	    INT 10h 
        
        MOV dl,9
        mov ah,2
        int 21h	
        
        MOV dl,9
        mov ah,2
        int 21h	 

        RET
    ERASE_SCORE ENDP	

  GAMINGG_OVER PROC NEAR
    CALL CLEAR_SCREEN
	; DL :X
	
	 MOV AH,02h ;NEXT LINE
	 MOV DH,39h
     MOV DL,70h
	 INT 10h 
	 
	MOV dl,71 ;G
    mov ah,2
    int 21h 

    MOV dl,65 ;A
    mov ah,2
    int 21h 
	
	MOV dl,77 ;M
    mov ah,2
    int 21h 
	
	MOV dl,69 ;E
    mov ah,2
    int 21h 
	
	MOV dl,32 ;BACKSPACE
    mov ah,2
    int 21h 
	
	MOV dl,79 ;O
    mov ah,2
    int 21h 
	
	MOV dl,86 ;v
    mov ah,2
    int 21h 
	
	MOV dl,69 ;E
    mov ah,2
    int 21h 
	
	MOV dl,82 ;R
    mov ah,2
    int 21h 
	
	MOV dl,32 ;BACKSPACE
    mov ah,2
    int 21h 
	
	MOV dl,58 ;;
    mov ah,2
    int 21h 
	
	MOV dl,40 ;(
    mov ah,2
    int 21h 
	
	MOV AH,02h
	MOV DH,3Ch
    MOV DL,70h
	INT 10h
	
	MOV dl,83 ;S
    mov ah,2
    int 21h
	
	MOV dl,67 ;C
    mov ah,2
    int 21h
	
	MOV dl,79 ;O
    mov ah,2
    int 21h
	
	MOV dl,82 ;R
    mov ah,2
    int 21h
	
	MOV dl,69 ;E
    mov ah,2
    int 21h

	
	
	MOV dl,58 ;:
    mov ah,2
    int 21h
	
	
	  
	  MOV dl,SCORE_1
      mov ah,2
      int 21h
	  
      MOV dl,BL
      add dl, 48 
	  
	  MOV dl,SCORE_2
      mov ah,2
      int 21h
	  
	  MOV AH,02h
	  MOV DH,00d   ;set the cursor
	  MOV DL,00d
	  INT 10h
	
    RET
  GAMINGG_OVER ENDP	
   CODE ENDS
END