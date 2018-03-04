	INCLUDE 'MC9S08JM16.INC'

;Para ubicar el apuntador en la direccion de memoria 0b0H, utilizar la instruccion mov
	ORG 0B0H 
;Define variables
;Dos byte mas significativos parte entera y el menos significativo parte decimal
INTACC1     DS    4            ;32-bit integer accumulator #1
INTACC2     DS    4  
AUX			DS 1
M 			DS 6
SUMA		DS 3
CONTADOR DS 1
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
MULTI:	MOV		SUMA+1,INTACC1+1;
		LDA		M,X				;
		STA		INTACC2+1		;
		JSR		UMULT16			;
		MOV		INTACC1+0,SUMA+0;
		MOV		INTACC1+3,SUMA+1;
		JMP *					;


UMULT16:     EQU     *
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
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
*
*     32 x 16 Unsigned Divide
*
*     This routine takes the 32-bit dividend stored in INTACC1:INTACC1+3
*     and divides it by the 16-bit divisor stored in INTACC2:INTACC2+1.
*     The quotient replaces the dividend and the remainder replaces the divisor.
*
UDVD32:    EQU     *
*
DIVIDEND  EQU     INTACC1+2
DIVISOR   EQU     INTACC2
QUOTIENT  EQU     INTACC1
REMAINDER EQU     INTACC1
*
        PSHH                            ;save h-reg value
        PSHA                            ;save accumulator
        PSHX                            ;save x-reg value
        AIS     #-3D                     ;reserve three bytes of temp storage
        LDA     #32d                    ;
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
SHFTLP: LDA     REMAINDER               ;get remainder MSB
        ROLA                            ;shift remainder MSB into carry
        ROL     DIVIDEND+3              ;shift dividend LSB
        ROL     DIVIDEND+2              ;shift 2nd byte of dividend
        ROL     DIVIDEND+1              ;shift 3rd byte of dividend
        ROL     DIVIDEND                ;shift dividend MSB
        ROL     REMAINDER+1             ;shift remainder LSB
        ROL     REMAINDER               ;shift remainder MSB
        
TABLA: FCB 9H,80H,17H,16H
;			Aentero,Adecimal,B,C
	ORG 0FFFEH;
	FDB INICIO; A DONDE SE DIRIGE DESPUES DE RESET
