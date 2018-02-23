	INCLUDE 'MC9S08JM16.INC'

;Para ubicar el apuntador en la direccion de memoria 0b0H, utilizar la instruccion mov
	ORG 0B0H 
;Define variables
;Dos byte mas significativos parte entera y el menos significativo parte decimal
AUX	DS 5
M1 	DS 2
M2 	DS 2
M3 	DS 2
S	DS 2
CONTADOR DS 1
;Desactivar WatchDog Timer Evita que el microcontrolador se reinicie
	ORG 0C000H
	
INICIO: CLRA;Limpia registro A 
		STA SOPT1;Carga registro SOPT1 con ceros para Deshabilitar Modulo COP
		LDHX #4B0H;Carga #4B0H para reubicar la pila al final de la RAM 
		TXS; SP = 4AFH o  (HX - 1) -> SP
OPERACIONES:
		LDHX #2H;le asigno el valor cero a hx (1)
		LDA TABLA, X;
		PSHA;
		AND #0F0H;
		ASRA;
		ASRA;
		ASRA;
		ASRA;
		LDX #0AH;
		MUL;
		PULX;
		PSHA;
		LDA X
		AND #0FH;
		PULX;
		ADD ,X;
		JMP *;
		
TABLA: FCB 55H,23H,33H,44H,0A0H,0H,0FFH,11H,8BH,44H,1H,5H,7H,23H,11H,7CH,0C0H,0AAH,19H,20H,21H

	ORG 0FFFEH;
	FDB INICIO; A DONDE SE DIRIGE DESPUES DE RESET
