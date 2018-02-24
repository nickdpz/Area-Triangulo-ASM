	INCLUDE 'MC9S08JM16.INC'

;Para ubicar el apuntador en la direccion de memoria 0b0H, utilizar la instruccion mov
	ORG 0B0H 
;Define variables
;Dos byte mas significativos parte entera y el menos significativo parte decimal
INTACC1     DS    4            ;32-bit integer accumulator #1
INTACC2     DS    4  
AUX			DS 5
M 			DS 6
SUMA		DS 3
CONTADOR DS 1
;Desactivar WatchDog Timer Evita que el microcontrolador se reinicie
	ORG 0C000H
	
INICIO: CLRA;Limpia registro A 
		STA SOPT1;Carga registro SOPT1 con ceros para Deshabilitar Modulo COP
		LDHX #4B0H;Carga #4B0H para reubicar la pila al final de la RAM 
		TXS; SP = 4AFH o  (HX - 1) -> SP
		CLR SUMA+0;
		CLR SUMA+1;
		CLR SUMA+2;
		CLR AUX;
		CLR AUX+1;
		CLR AUX+2;
		CLR AUX+3;
		CLR AUX+4;
OPERACIONES:
		LDHX	#0H				;Se le asigna la posicion de la tabla que quiere leer 
CARGA:	LDA		TABLA,	X		;Se accede a la tabla direccionando en la posicion x
		PSHA					;Se carga dato a la pila								
		AND 	#0FH			;Se emascara la parte baja del dato
		STA 	M,X			;Se carga el dato En M+1 Parte mas baja entera			
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
SUMAS:  LDA		M+0				;
		ADD     M+2				;
		STA     SUMA+1			;
		LDA		M+3
		ADD 	SUMA+1			;
		STA		SUMA+1			;
		CLRA					;Limpia a para la suma 
		ADC		SUMA+0			;
		STA		SUMA+0			;
		CLRA					;
		
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
        
TABLA: FCB 99H,11H,93H,94H
;			Aentero,Adecimal,B,C
	ORG 0FFFEH;
	FDB INICIO; A DONDE SE DIRIGE DESPUES DE RESET
