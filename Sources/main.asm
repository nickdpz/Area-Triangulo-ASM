	INCLUDE 'MC9S08JM16.INC'

;Para ubicar el apuntador en la direccion de memoria 0b0H, utilizar la instruccion mov
	ORG 0B0H 
;Define variables
;Dos byte mas significativos parte entera y el menos significativo parte decimal
INTACC1     DS    4            ;32-bit integer accumulator #1
INTACC2     DS    4  
AUX			DS 	  11
M 			DS    7
SUMA		DS    3
CONTADOR 	DS 	  1
;Desactivar WatchDog Timer Evita que el microcontrolador se reinicie
	ORG 0C000H
	
INICIO: CLRA;Limpia registro A 
		STA SOPT1;Carga registro SOPT1 con ceros para Deshabilitar Modulo COP
		LDHX #4B0H;Carga #4B0H para reubicar la pila al final de la RAM 
		TXS; SP = 4AFH o  (HX - 1) -> SP
		CLR SUMA+0;LIMPIAR VARIABLES
		CLR SUMA+1;
		CLR SUMA+2;
		CLR AUX;
		CLR AUX+1;
		CLR AUX+2;
		CLR AUX+3;
		CLR AUX+4;
		CLR INTACC1+0;
		CLR	INTACC1+1;
		CLR INTACC1+2;
		CLR	INTACC1+3;
		CLR INTACC2+0;
		CLR	INTACC2+1;
		CLR INTACC1+2;
		CLR	INTACC1+3;
OPERACIONES:
		LDHX	#0H				;Se le asigna la posicion de la tabla que quiere leer 
CARGA:	LDA		TABLA,	X		;Se accede a la tabla direccionando en la posicion x
		PSHA					;Se carga dato a la pila								
		AND 	#0FH			;Se emascara la parte baja del dato
		STA 	M,X		    	;Se carga el dato En M+1 Parte mas baja entera			
		PULA					;Se saca dato de la pila		
		AND 	#0F0H			;Se emascara la parte alta del dato
		LSRA					;Se desplaza a la derecha, Cuatro posiciones en total
		LSRA					;""
		LSRA					;""
		LSRA					;""
		PSHX					;
		LDX		#0AH			;Se carga X con 10
		MUL						;Se X*A parte baja en A y parte alta en X 
		PULX					;
		ADD 	M,X				;Se suma el dato almacenado en la posicion entera mas baja con A sin bit de acarreo  
		STA 	M,X				;Se carga el resultado de la suma a la parte entera mas baja
		INCX					;Incrementa X	
		CBEQX 	#04H,SUMAS
		JMP 	CARGA			;
SUMAS:  LDA		M+0				; Se carga en el registro A la parte entera de a  
		ADD     M+2				; se suma al registro a la parte entera de b
		STA     SUMA+1		 	; se guarda el contenidp del registro A en suma+1(parte entera baja de s)
		LDA		M+3             ; Se carga la parte entera de c
		ADD 	SUMA+1			; se suma el regitsro a con la parte entera de c
		STA		SUMA+1			; se guarda el contenido de a en suma +1(parte entera baja de s)
		CLRA					; Limpia a para la suma 
		ADC		SUMA+0			; se usa la bandera de acarreo si la suma anterior es mayor a 256(parte baja)
		STA		SUMA+0			; en el registro A queda 0 o 1 dependiendo de la bandera de carry(suma anterior)
		MOV     M+1,SUMA+2      ; Se carga la parte decimal de a en suma+2 s=a+b+c
		LSR		SUMA+2			;Desplaza a la derecha SUMA+2 0 -> A(7)
		LSR		SUMA+1			;SUMA+2(0)->c
		BCC		ENTERA			;Pregunta si el bit de acarreo esta en 0  
		LDA		#32H			;Si (c==0) carga a con 50
		ADD		SUMA+2			;Suma la parte decimal de a con 50
		STA		SUMA+2			;Cargar la parte decimal con el resultado 
ENTERA:	BRCLR 	0H,SUMA+0,RESTAS;Pregunta SUMA+0(0)==1 (Parte entera alta)
		BSET	7d,SUMA+1		;Poner el bit 7 de Suma en 1 
RESTAS:	LDA		SUMA+1			;Carga S entero
		SUB		M+2				;Resta S-B entero
		STA		M+2				;Guarda S-B entero en m+2
		LDA		SUMA+1			;Carga S entero
		SUB		M+3				;Resta S-C entero
		STA		M+4				;guarda S-C entero en m+4
		MOV		SUMA+2,M+3      ;Asigna decimal de S a m+3
		MOV		SUMA+2,M+5		;Asigna decimal de S a m+5		
		LDA		SUMA+1			;Se carga a para resta
		SUB		M+0				;Resta S-A entera
		STA		M+0				;Se carga en M+0 (S-A)entero
		CLRX					;SE LIMPIA
		LDA		SUMA+2			;Carga decimal de s
		SUB		M+1				;Se resta las partes decimales de s y a (S-A)
		STA		M+1				;GUARDA DECIMAL DE S-A
		BGT		MULTI			;Pregunta A-S>0
		LDX		M+0				;CARGA EN EL REGISTRO X LA PARTE ENTERA DE S			
		DECX					;X=X-1 Nuevo calor de MA
		STX		M+0				;CARGA NUEVO VALOR DE MA(S-A) ENTERO
		LDA		#64H			;CARGA 100 EN A
		ADD		M+1				;RESTA S+100-A Decimal	
		STA		M+1				;GUARDA DECIMAL DE S-A
		CLRX					;LIMPIA X PARA DIRECIONAMIENTO
MULTI:	LDA     SUMA+1          ;----------------------------
		LDX     M+0
		MUL                     ; MULTIPLICACION S ENTERO CON S-A ENTERO
		STX     AUX+0
		STA     AUX+1           ;----------------------------
		LDA     SUMA+1
		LDX     M+1
		MUL
		PSHX					;
		PULH					;
		LDX	    #64H            ; MULTIPLICACION S ENTERO CON S-A DECIMAL
		DIV
		STA     AUX+2
		PSHH
		PULA
		STA     AUX+3             ;-----------------------------
		LDA     SUMA+2
		LDX     M+0
		MUL
		PSHX
		PULH
		LDX     #64H              ; MULTIPLICACION S DECIMAL CON S-A ENTERO
		DIV
		STA     AUX+4
		PSHH
		PULA
		STA     AUX+5             ;------------------------
		LDA     SUMA+2
		LDX     M+1
		MUL
		PSHX
		PULH
		LDX     #64H              ; MULTIPLICACION S DECIMAL CON S-A DECIMAL
		DIV
		CLRH
		DIV
		PSHH
		PULA
		STA    AUX+6             ;------------------------------
		
		LDA   AUX+1              ;-------------------------------
		ADD   AUX+2
		STA   AUX+1
		CLRA                     
		ADC   AUX+0
		STA   AUX+0              ; SUMA PARTES ENTERAS
		LDA   AUX+1
		ADD   AUX+4
		STA   AUX+1
		CLRA
		ADC   AUX+0             
		STA   AUX+0              ;-----------------------------------
		
SUM_D_1:LDA   AUX+3
		ADD   AUX+5
		PSHA
		SUB   #64H
		BMI   SUM_D_2            ; SUMA PARTES DECIMALES
		PULX
		CLRX
		PSHA
		LDA   AUX+1
		ADD   #1H
		STA   AUX+1
		CLRA
		ADC   AUX+0
		STA   AUX+0
SUM_D_2:
		PULA 
		STA   AUX+2              ; SUMA PARTES DECIMALES
		ADD   AUX+6
		PSHA
		SUB   #64H
		BMI   MUL_2
		PULX
		CLRX
		PSHA
		LDA   AUX+1
		ADD   #1H
		STA   AUX+1              ; SUMA PARTE DECIMALES
		CLRA
		ADC   AUX+0
		STA   AUX+0
MUL_2:
		PULA 
		STA   	AUX+2          ;--------------------------------------------
		MOV		AUX+0,SUMA+0	;
		MOV		AUX+1,SUMA+1	;
		MOV		AUX+2,SUMA+2	;	
		LDA     M+4          	;----------------------------
		LDX     M+2
		MUL                     ; MULTIPLICACION S ENTERO CON S-A ENTERO
		STX     AUX+0			;CARGA AUX+0
		STA     AUX+1           ;CARGA AUX+1----------------------------
		LDA     M+2				;CARGA PARTE ENTERA 
		LDX     M+5				;CARGA PARTE DECIMAL DEL SEGUNDO
		MUL						;MULTIPLICA
		PSHX					;MUEVE X
		PULH					;GUARDA EN H PARTE ALTA NUMERADOR
		LDX	    #64H            ;MULTIPLICACION S ENTERO CON S-A DECIMAL
		DIV						;DIVIDE SOBRE 100
		STA     AUX+2			;GUARDA PARTE ENTERA DE DIVISION 
		PSHH					;RECIDUO QUEDA EN H 
		PULA					;SE SACA DE LA PILA EL RECIDUO
		STA     AUX+3           ;-----------------------------CARGA DATO 
		LDA     M+3				;CARGA PARTE DECIMAL PRIMERO
		LDX     M+4				;CARGA PARTE ENTERA SEGUNDO
		MUL						;MULTIPLICA
		PSHX					;PONE EN LA PILA X
		PULH					;PASA A H
		LDX     #64H            ;CARGA DENOMINADOR
		DIV						;DIVIDE
		STA     AUX+4			;CARGA EL DATO
		PSHH					;GUARDA RESIDUO EN PILA
		PULA					;SACA EL RESIDUO EN EL REGISTRO A
		STA     AUX+5           ;------------------------
		LDA     M+3          	;CARGA DECIMAL 1
		LDX     M+5				;CARGA DECIMAL 5
		MUL						;MULTIPLICA DECIMAL POR DECIMAL
		PSHX					;GUARDA EN LA PILA PARTA ALTA
		PULH					;CARGA PARTE ALTA
		LDX     #64H            ;CARAGA 100
		DIV						;DIVIDE SOBRE 100
		CLRH					;LIMPIA H
		DIV						;DIVIDE 10.000
		PSHH					;RESIDUO EN H
		PULA					;LUEGO SE PASA A A
		STA    AUX+6             ;------------------------------SE CARGA EL DATO
		
		LDA   AUX+1             ;-------------------------------SE CARGA PARTE BAJA DE E*E 
		ADD   AUX+2				;SUMA CON PARTE ENTERA DE E*D	
		STA   AUX+1				;SE GUARDA EL DATO BAJO
		CLRA                    ;LIMPIA A 
		ADC   AUX+0				;SUMA EL CARRY
		STA   AUX+0             ;CARGA PARTE ALTA
		LDA   AUX+1				;CARGA PARTE BAJO
		ADD   AUX+4				;SE SUMA CON ENTERO DE D*E
		STA   AUX+1				;SE GUARDA DATO BAJO
		CLRA					;SE SUMA CARRY
		ADC   AUX+0             ;
		STA   AUX+0             ;SE CARGA DATO ALTO-----------------------------------
		
SUM_D_12:LDA   AUX+3				;
		ADD   AUX+5				;
		PSHA					;
		SUB   #64H				;
		BMI   SUM_D_22           ; SUMA PARTES DECIMALES
		PULX					;
		CLRX					;
		PSHA
		LDA   AUX+1
		ADD   #1H
		STA   AUX+1
		CLRA
		ADC   AUX+0
		STA   AUX+0
SUM_D_22:
		PULA 
		STA   AUX+2              ; SUMA PARTES DECIMALES
		ADD   AUX+6
		PSHA
		SUB   #64H
		BMI   MUL_22
		PULX
		CLRX
		PSHA
		LDA   AUX+1
		ADD   #1H
		STA   AUX+1              ; SUMA PARTE DECIMALES
		CLRA
		ADC   AUX+0
		STA   AUX+0
MUL_22:	PULA 
		STA   	AUX+2			;MULTIPLICACION 3
		MOV		SUMA+0,INTACC1+0;CARGA ENTERO1
		MOV		SUMA+1,INTACC1+1;CARGA ENTERO2			
		MOV		AUX+0,INTACC2+0	;CARGA ENTERO1
		MOV		AUX+1,INTACC2+1	;CARGA ENTERO2	
		JSR		UMULT16         ;MULTIPLICACION S ENTERO CON S-A ENTERO LLAMADA A SUBRUTINA
		MOV     INTACC1+0,M+0	;CARGA M+0
		MOV     INTACC1+1,M+1	;CARGA M+1
		MOV     INTACC1+2,M+2	;CARGA M+2
		MOV     INTACC1+3,M+3	;CARGA M+3
		
		MOV		SUMA+0,INTACC1+0	;CARGA ENTERO1
		MOV		SUMA+1,INTACC1+1	;CARGA ENTERO2			
		MOV		#0H,INTACC2+0	;CARGA CEROS
		MOV		AUX+2,INTACC2+1	;CARGA DECIMAL	
		JSR		UMULT16         ;MULTIPLICACION S ENTERO CON S-A ENTERO LLAMADA A SUBRUTINA
		MOV     #0H,INTACC2
		MOV     #64H,INTACC2+1
		JSR     UDVD32
		MOV     INTACC2+1,M+4   ;SE CARGA DATO DECIMAL
		MOV     INTACC1+0,AUX+3
		MOV     INTACC1+1,AUX+4
		MOV     INTACC1+2,AUX+5
		MOV     INTACC1+3,AUX+6 ;-------------------------------------------------------------------
		
		MOV		AUX+0,INTACC1+0	;CARGA ENTERO1
		MOV		AUX+1,INTACC1+1	;CARGA ENTERO2			
		MOV		#0H,INTACC2+0	;CARGA CEROS
		MOV		SUMA+2,INTACC2+1;CARGA DECIMAL	
		JSR		UMULT16         ;MULTIPLICACION S ENTERO CON S-A ENTERO LLAMADA A SUBRUTINA
		MOV     #0H,INTACC2
		MOV     #64H,INTACC2+1
		JSR     UDVD32
		MOV     INTACC2+1,M+5		;SE CARGA DATO DECIMAL 
		MOV     INTACC1+0,AUX+7
		MOV     INTACC1+1,AUX+8
		MOV     INTACC1+2,AUX+9
		MOV     INTACC1+3,AUX+10 ;-------------------------------------------------------------------			

		LDA     SUMA+2          ;CARGA DECIMAL 1
		LDX     AUX+2			;CARGA DECIMAL 5
		MUL						;MULTIPLICA DECIMAL POR DECIMAL
		PSHX					;GUARDA EN LA PILA PARTA ALTA
		PULH					;CARGA PARTE ALTA
		LDX     #64H            ;CARAGA 100
		DIV						;DIVIDE SOBRE 100
		CLRH					;LIMPIA H
		DIV						;DIVIDE 10.000
		PSHH					;RESIDUO EN H
		PULA					;LUEGO SE PASA A A
		STA    M+6             	;------------------------------SE CARGA EL DATO DECIMAL
		
		LDA   M+3            	;-------------------------------SE CARGA PARTE BAJA ENTERA1 
		ADD   AUX+6				;SUMA CON PARTE ENTERA DE E*D	
		STA   M+3				;SE GUARDA EL DATO BAJO
		LDA   M+2               ;CARGA SEGUNDO BYTE 
		ADC   AUX+5				;SUMA EL CARRY
		STA   M+2             ;CARGA PARTE ALTA
		LDA   M+1				;CARGA PARTE BAJO
		ADC   AUX+4				;SE SUMA CON ENTERO DE D*E
		STA   M+1				;SE GUARDA DATO BAJO
		LDA	  M+0				;
		ADC   AUX+3             ;
		STA   M+0             	;SE CARGA DATO ALTO-----------------------------------
		
		LDA   M+3            	 ;-------------------------------SE CARGA PARTE BAJA ENTERA1 
		ADD   AUX+10				;SUMA CON PARTE ENTERA DE E*D	
		STA   M+3				;SE GUARDA EL DATO BAJO
		LDA   M+2               ;CARGA SEGUNDO BYTE 
		ADC   AUX+9				;SUMA EL CARRY
		STA   M+2             ;CARGA PARTE ALTA
		LDA   M+1				;CARGA PARTE BAJO
		ADC   AUX+8				;SE SUMA CON ENTERO DE D*E
		STA   M+1				;SE GUARDA DATO BAJO
		LDA	  M+0				;
		ADC   AUX+7             ;
		STA   M+0             	;SE CARGA DATO ALTO-------------------	
			
SUM_D_13:LDA  M+4				;
		ADD   M+5				;
		PSHA					;
		SUB   #64H				;
		
		BMI   SUM_D_23          ; SUMA PARTES DECIMALES
		
		PULX					;
		CLRX					;
		PSHA
		LDA   M+3
		ADD   #1H				;SUMA MAS 1 En todos los casos PARTE ENTERA
		STA   M+3				;
		CLRA					;
		ADC   M+2				;
		STA   M+2				;
		CLRA					;
		ADC   M+1				;
		STA   M+1
		CLRA
		ADC   M+0
		STA   M+0
SUM_D_23:PULA 
		STA   M+4              ; SUMA PARTES DECIMALES
		ADD   M+6
		PSHA
		SUB   #64H
		
		BMI   MUL_23
		
		PULX
		CLRX					;
		PSHA
		LDA   M+3
		ADD   #1H				;SUMA MAS 1 En todos los casos PARTE ENTERA
		STA   M+3				;
		CLRA					;
		ADC   M+2				;
		STA   M+2				;
		CLRA					;
		ADC   M+1				;
		STA   M+1
		CLRA
		ADC   M+0
		STA   M+0
MUL_23:	PULA 
		STA   M+4				;SE GUARDA LA PARTE DECIMAL FINAL		
		JMP *		


UMULT16:    EQU     *
            PSHA                        ;save acc
            PSHX                        ;save x-reg
            PSHH                        ;save h-reg
            AIS     #-6                 ;reserve six bytes of temporary
                                        ;storage on stack
            CLR     6,SP                ;zero storage for multiplication carry
*
*     Multiply (INTACC1:INTACC1+1) by INTACC2+1
*
            LDX     INTACC1+1           ;load x-reg w/multiplier LSB
            LDA     INTACC2+1           ;load acc w/multiplicand LSB
            MUL                         ;multiply
            STX     6,SP                ;save carry from multiply
            STA     INTACC1+3           ;store LSB of final result
            LDX     INTACC1             ;load x-reg w/multiplier MSB
            LDA     INTACC2+1           ;load acc w/multiplicand LSB
            MUL                         ;multiply
            ADD     6,SP                ;add carry from previous multiply
            STA     2,SP                ;store 2nd byte of interm. result 1.
            BCC     NOINCA              ;check for carry from addition
            INCX                        ;increment MSB of interm. result 1.
NOINCA      STX     1,SP                ;store MSB of interm. result 1.
            CLR     6,SP                ;clear storage for carry
*
*     Multiply (INTACC1:INTACC1+1) by INTACC2
*
            LDX     INTACC1+1           ;load x-reg w/multiplier LSB
            LDA     INTACC2             ;load acc w/multiplicand MSB
            MUL                         ;multiply
            STX     6,SP                ;save carry from multiply
            STA     5,SP                ;store LSB of interm. result 2.
            LDX     INTACC1             ;load x-reg w/multiplier MSB
            LDA     INTACC2             ;load acc w/multiplicand MSB
            MUL                         ;multiply
            ADD     6,SP                ;add carry from previous multiply
            STA     4,SP                ;store 2nd byte of interm. result 2.
            BCC     NOINCB              ;check for carry from addition
            INCX                        ;increment MSB of interm. result 2.
NOINCB      STX     3,SP                ;store MSB of interm. result 2.

*     Add the intermediate results and store the remaining three bytes of the
*     final value in locations INTACC1:INTACC1+2.
*
            LDA     2,SP                ;load acc with 2nd byte of 1st result
            ADD     5,SP                ;add acc with LSB of 2nd result
            STA     INTACC1+2           ;store 2nd byte of final result
            LDA     1,SP                ;load acc with MSB of 1st result
            ADC     4,SP                ;add w/ carry 2nd byte of 2nd result
            STA     INTACC1+1           ;store 3rd byte of final result
            LDA     3,SP                ;load acc with MSB from 2nd result
            ADC     #0                  ;add any carry from previous addition
            STA     INTACC1             ;store MSB of final result
*
*     Reset stack pointer and recover original register values
*
            AIS     #6                  ;deallocate the six bytes of local
                                        ;storage
            PULH                        ;restore h-reg
            PULX                        ;restore x-reg
            PULA                        ;restore accumulator
            RTS                         ;return
;
;     32 x 16 Unsigned Divide

;     This routine takes the 32-bit dividend stored in INTACC1:INTACC1+3
;     and divides it by the 16-bit divisor stored in INTACC2:INTACC2+1.
;     The quotient replaces the dividend and the remainder replaces the divisor.

UDVD32:   EQU     *
*
DIVIDEND  EQU     INTACC1+2
DIVISOR   EQU     INTACC2
QUOTIENT  EQU     INTACC1
REMAINDER EQU     INTACC1
*
        PSHH                            ;save h-reg value
        PSHA                            ;save accumulator
        PSHX                            ;save x-reg value
        AIS     #-3                     ;reserve three bytes of temp storage
        LDA     #32D                    ;
        STA     3,SP                    ;loop counter for number of shifts
        LDA     DIVISOR                 ;get divisor MSB
        STA     1,SP                    ;put divisor MSB in working storage
        LDA     DIVISOR+1               ;get divisor LSB
        STA     2,SP                    ;put divisor LSB in working storage
*
*     Shift all four bytes of dividend 16 bits to the right and clear
*     both bytes of the temporary remainder location
*
        MOV     DIVIDEND+1,DIVIDEND+3   ;shift dividend LSB
        MOV     DIVIDEND,DIVIDEND+2     ;shift 2nd byte of dividend
        MOV     DIVIDEND-1,DIVIDEND+1   ;shift 3rd byte of dividend
        MOV     DIVIDEND-2,DIVIDEND     ;shift dividend MSB
        CLR     REMAINDER               ;zero remainder MSB
        CLR     REMAINDER+1             ;zero remainder LSB
*
*     Shift each byte of dividend and remainder one bit to the left
*
SHFTLP  LDA     REMAINDER               ;get remainder MSB
        ROLA                            ;shift remainder MSB into carry
        ROL     DIVIDEND+3              ;shift dividend LSB
        ROL     DIVIDEND+2              ;shift 2nd byte of dividend
        ROL     DIVIDEND+1              ;shift 3rd byte of dividend
        ROL     DIVIDEND                ;shift dividend MSB
        ROL     REMAINDER+1             ;shift remainder LSB
        ROL     REMAINDER               ;shift remainder MSB

*     Subtract both bytes of the divisor from the remainder
*
        LDA     REMAINDER+1             ;get remainder LSB
        SUB     2,SP                    ;subtract divisor LSB from remainder LSB
        STA     REMAINDER+1             ;store new remainder LSB
        LDA     REMAINDER               ;get remainder MSB
        SBC     1,SP                    ;subtract divisor MSB from remainder MSB
        STA     REMAINDER               ;store new remainder MSB
        LDA     DIVIDEND+3              ;get low byte of dividend/quotient
        SBC     #0                      ;dividend low bit holds subtract carry
        STA     DIVIDEND+3              ;store low byte of dividend/quotient
*
*     Check dividend/quotient LSB. If clear, set LSB of quotient to indicate
*     successful subraction, else add both bytes of divisor back to remainder
*
        BRCLR   0,DIVIDEND+3,SETLSB     ;check for a carry from subtraction
                                        ;and add divisor to remainder if set
        LDA     REMAINDER+1             ;get remainder LSB
        ADD     2,SP                    ;add divisor LSB to remainder LSB
        STA     REMAINDER+1             ;store remainder LSB
        LDA     REMAINDER               ;get remainder MSB
        ADC     1,SP                    ;add divisor MSB to remainder MSB
        STA     REMAINDER               ;store remainder MSB
        LDA     DIVIDEND+3              ;get low byte of dividend
        ADC     #0                      ;add carry to low bit of dividend
        STA     DIVIDEND+3              ;store low byte of dividend
        BRA     DECRMT                  ;do next shift and subtract
SETLSB  BSET    0,DIVIDEND+3            ;set LSB of quotient to indicate
                                        ;successive subtraction
DECRMT  DBNZ    3,SP,SHFTLP             ;decrement loop counter and do next
                                        ;shift
*
*     Move 32-bit dividend into INTACC1:INTACC1+3 and put 16-bit
*     remainder in INTACC2:INTACC2+1
*
        LDA     REMAINDER               ;get remainder MSB
        STA     1,SP                    ;temporarily store remainder MSB
        LDA     REMAINDER+1             ;get remainder LSB
        STA     2,SP                    ;temporarily store remainder LSB
        MOV     DIVIDEND,QUOTIENT       ;
        MOV     DIVIDEND+1,QUOTIENT+1   ;shift all four bytes of quotient
        MOV     DIVIDEND+2,QUOTIENT+2   ; 16 bits to the left
        MOV     DIVIDEND+3,QUOTIENT+3   ;
        LDA     1,SP                    ;get final remainder MSB
        STA     INTACC2                 ;store final remainder MSB
        LDA     2,SP                    ;get final remainder LSB
        STA     INTACC2+1               ;store final remainder LSB
*
*     Deallocate local storage, restore register values, and return from
*     subroutine
*
        AIS     #3                      ;deallocate temporary storage
        PULX                            ;restore x-reg value
        PULA                            ;restore accumulator value
        PULH                            ;restore h-reg value
        RTS                             ;return

********************************************************************************
********************************************************************************
*
*     Table Lookup and Interpolation
*
*     This subroutine performs table lookup and interpolation between two 16-bit
*     dependent variables (Y) from a table of up to 256 enties (512 bytes) and
*     allowing up to 256 interpolation levels between entries. INTACC1 contains
*     the position of ENTRY2 and INTACC1+1 contains the interpolation fraction.
*     The 16-bit result is placed in INTACC1+2=MSB, INTACC1+3=LSB. INTACC2 is
*     used to hold the two 16-bit entries during the routine.
*
*     Y = ENTRY1 + (INTPFRC(ENTRY2 - ENTRY1))/256
*
TBLINT:   EQU      *
*
ENTNUM   EQU       INTACC1              ;position of entry2 (0-255)
INTPFRC  EQU       INTACC1+1            ;interpolation fraction (1-255)/256
RESULT   EQU       INTACC1+2            ;16-bit interpolated Y value
ENTRY1   EQU       INTACC2              ;16-bit enrty from table
ENTRY2   EQU       INTACC2+2            ;16-bit entry from table
*
         PSHH                           ;save h-register
         PSHA                           ;save accumulator
         PSHX                           ;save x-reg
         AIS       #-1                  ;allocate one byte of temp storage
         CLRH                           ;zero h-reg
         CLRA                           ;zero accumulator
         CLR       1,SP                 ;clear storage for difference sign
*
*     Load H:X with position of ENTRY2
*
         LDX       ENTNUM               ;get position of entry2 (0-255)
         LSLX                           ;multiply by 2 (for 16-bit entries)
         BCC       GETENT               ;if overflow from multiply occured,
                                        ;increment H-reg.
         INCA                           ;accumulator = 1
         PSHA                           ;push accumulator value on stack
         PULH                           ;transfer acc. value to h register
*
*     Get both entries from table, subtract ENTRY1 from ENTRY2 and store the
*     16-bit result.
*
GETENT   LDA       TABLE-2,x            ;get entry1 LSB
         STA       ENTRY1
         LDA       TABLE-1,x            ;get entry1 MSB
         STA       ENTRY1+1
         LDA       TABLE,x              ;get entry2 MSB
         STA       ENTRY2
         LDA       TABLE+1,x            ;get entry2 LSB
         STA       ENTRY2+1
         SUB       ENTRY1+1             ;entry2(LSB) - entry1(LSB)
         STA       RESULT+1             ;store result LSB
         LDA       ENTRY2
         SBC       ENTRY1               ;entry2(MSB) - entry1(MSB)
         STA       RESULT               ;store result MSB

*
*     Two's complement 16-bit result if ENTRY1 was greater than ENTRY2, else
*     go do multiply
*
*
         TSTA                           ;test result MSB for negative
         BGE       MLTFRAC              ;go do multiply if postive
         INC       1,SP                 ;set sign flag for negative result
         NEG       RESULT+1             ;two's complement result LSB
         BCC       NODECR               ;check for borrow from zero
         NEG       RESULT               ;two's complement result MSB
         DEC       RESULT               ;decrement result MSB for borrow
         BRA       MLTFRAC              ;go do multiply
NODECR   NEG       RESULT               ;two's comp result MSB (no borrow)
*
*     (INTPFRC(RESULT:RESULT+1))/256 = Interpolated result
*
*     Multiply result by interpolation fraction
*
MLTFRAC  LDA       INTPFRC              ;get interpolation fraction
         LDX       RESULT+1             ;get result LSB
         MUL                            ;multiply
         STX       RESULT+1             ;store upper 8-bits of result and throw
                                        ;away lower 8-bits (divide by 256)
         LDA       INTPFRC              ;get interpolation fraction
         LDX       RESULT               ;get result MSB
         MUL                            ;multiply
         ADD       RESULT+1             ;add result LSB to lower 8-bits of
                                        ;product
         STA       RESULT+1             ;store new result LSB
         TXA                            ;get upper 8-bits of product
         ADC       #0                   ;add carry from last addition
         STA       RESULT               ;store result MSB
*
*     Y = ENTRY1 + Interpolated result
*
*     Check sign flag to determine if interpolated result is to be added to
*     or subtracted from ENTRY1
*
         TST       1,SP                 ;test sign flag for negative
         BLE       ADDVAL               ;if not set, add interpolated result
                                        ;to entry1, else subtract
         LDA       ENTRY1+1             ;get entry1 LSB
         SUB       RESULT+1             ;subtract result LSB
         STA       RESULT+1             ;store new result LSB
         LDA       ENTRY1               ;get entry1 MSB
         SBC       RESULT               ;subtact w/ carry result MSB
         STA       RESULT               ;store new result MSB
         BRA       TBLDONE              ;finished
ADDVAL   LDA       RESULT+1             ;get result LSB
         ADD       ENTRY1+1             ;add entry1 LSB
         STA       RESULT+1             ;store new result LSB
         LDA       ENTRY1               ;get entry1 MSB
         ADC       RESULT               ;add w/ carry result MSB
         STA       RESULT               ;store new result MSB
*     Deallocate local storage, restore register values, and return from
*     subroutine.
*
TBLDONE  AIS       #1                   ;deallocate local storage
         PULX                           ;restore x-reg
         PULA                           ;restore accumulator
         PULH                           ;restore h-reg
         RTS                            ;return from subroutine
*
*     Sample of 16-bit table entries
*
TABLE    EQU       *
         FDB       !0000                ;entry 0
         FDB       !32767               ;entry 1
         FDB       !2416                ;entry 2
         FDB       !4271                ;entry 3
************************************************ 
        
TABLA: FCB 10H,80H,23H,16H
;			Aentero,Adecimal,B,C
	ORG 0FFFEH;
	FDB INICIO; A DONDE SE DIRIGE DESPUES DE RESET
