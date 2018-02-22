	INCLUDE 'MC9S08JM16.INC'

;Para ubicar el apuntador en la direccion de memoria 0b0H, utilizar la instruccion mov
	ORG 0B0H 
;Define variables
;Dos byte mas significativos parte entera y el menos significativo parte decimal
AUX	DS 3
M1 	DS 3
M2 	DS 3
M3 	DS 3
S	DS 3
CONTADOR DS 1
;Desactivar WatchDog Timer Evita que el microcontrolador se reinicie
	ORG 0C000H
	
INICIO: CLRA;Limpia registro A 
		STA SOPT1;Carga registro SOPT1 con ceros para Deshabilitar Modulo COP
		LDHX #4B0H;Carga #4B0H para reubicar la pila al final de la RAM 
		TXS; SP = 4AFH o  (HX - 1) -> SP
OPERACIONES: 

TABLAC:	FCB 0D,0D,0D
        
		

	ORG 	0FFFEH
	FDB 	INICIO
		
