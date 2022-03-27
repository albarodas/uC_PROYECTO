; Archivo:	RELOJ.s
; Dispositivo:	PIC16F887
; Autor:	Alba Rodas
; Compilador:	pic-as (v2.35), MPLABX V6.00
;                
; Programa:	RELOJ DIGITAL (FECHA, HORA, TIMER, ALARMA, ALARMA DIGITAL)
; Hardware:	LED, PIC, RESISTENCIAS, TRANSISTORES PNP.
;    
; Creado:	26-02-2022
; Última modificación: 08-03-2022
 ; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 PROCESSOR 16F887
 #include <xc.inc>

 ;CONFIGURATION WORD 1
  CONFIG FOSC=INTRC_NOCLKOUT //oscilador interno --> reloj interno..
  CONFIG WDTE=OFF // WDT disables  (reinicia repetitivamente el PIC)
  CONFIG PWRTE=ON // PWRT enabled (se espera 72ms al empezar el funcionamiento)
  CONFIG MCLRE=OFF // El pin MCLR de utiliza como INPUT/OUTPUT
  CONFIG CP=OFF // Sin proteccion de codigo
  CONFIG CPD=OFF // Sin protección de datos
  CONFIG BOREN=OFF //Se desabilita/OFF para que cuando exista una baja de voltaje <4V, no haya reinicio
  CONFIG IESO=OFF // Se establece un reinicio sin cambiar del reloj interno al externo
  CONFIG FCMEN=OFF // Si existiera un fallo, se configura el cambio de reloj de externo a interno
  CONFIG LVP=ON // Se permite el desarrollo de la programacion, incluso en bajo voltaje
 
 ;CONFIGURATION WORD 2
 CONFIG WRT=OFF // Se programa como desactivada la protección de autoescritura 
 CONFIG BOR4V=BOR40V // Se programa reinicio cuando el voltaje sea menor a 4V
 
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MACROS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 ; >>> MACRO: RESET DEL TMR0
 RESET_TMR0 macro	    ; ESTE LO PUEDO DEJAR CON VALOR DE UNA VEZ, PORQUE NO LO LLAMARÉ EN OTRA FUNCION, *HASTA EL MOMENTO*
    banksel PORTA
    movlw   231		    ; VALOR INICIAL PARA TENER SALTOS DE 2ms
    movwf   TMR0	    ; ALMACENO EL VALOR INICIAL DADO EN EL TMR0
    bcf	    T0IF	    ; LIMPIO LA BANDERA DE OVERFLOW
    endm
    ; SIMILAR A USADO EN LAB
    
 ; >>> MACRO: RESET DEL TMR1
 RESET_TMR1 MACRO TMR1_H, TMR1_L	
    BANKSEL TMR1H
    MOVLW   TMR1_H	    ; TMR1H a W
    MOVWF   TMR1H	    ; W --> TMR1H
    MOVLW   TMR1_L	    ; TMR1L --> W
    MOVWF   TMR1L	    ; W --> TMR1L
    BCF	    TMR1IF	    ; LIMPIO BANDERA DEL TMR1
    
    ENDM

; >>> MACRO: DIVISOR PARA MOSTRAR DATOS EN DISPLAY
 division	macro	divisor, cociente, residuo
    movwf   dividendo
    clrf    dividendo+1
    incf    dividendo+1
    movlw   divisor
    subwf   dividendo, f
    btfsc   STATUS,0
    goto    $-4
    decf    dividendo+1, W
    movwf   cociente
    movlw   divisor
    addwf   dividendo, W
    movwf   residuo	
    endm

; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< VARIABLES >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 PSECT udata_bank0 ; COMMON MEMORY --> DEBO TENER CUIDADO PORQUE SE PUEDE LLENAR ESTA PAGINA

 ;VARIABLES AUXILIARES
    status_transistores:		    DS  1	; STATUS DE TRANSISTORES
    presiono_boton:			    DS	1	; SWITCH PARA CAMBIAR DE ESTADO
    dividendo:				    DS  2	; VARIABLE PARTE DEL MACRO DE DIVISION
    limite_days:			    DS	1	; VARIABLE CON LA QUE LIMITO DIAS
    limite_days_feb:			    DS	2	; VARIABLE CON LA QUE LIMITO DIAS DE FEB

;VARIABLES PARA FECHA Y HORA    
    segundos:				    DS  1	; VARIABLES EN LAS QUE ALMACENO INFO DEL TIEMPO
    minutos:				    DS  1	; VARIABLES EN LAS QUE ALMACENO INFO DEL TIEMPO
    horas:				    DS  1	; VARIABLES EN LAS QUE ALMACENO INFO DEL TIEMPO
    dias:				    DS  1	; VARIABLES EN LAS QUE ALMACENO INFO DEL TIEMPO
    meses:				    DS  1	; VARIABLES EN LAS QUE ALMACENO INFO DEL TIEMPO

;VARIABLES PARA DESPLEGAR 'TIEMPO/HORA' 
    minutos_display1:			    DS  1	; EN ESTAS VARIABLES GUARDO EL RESULTDAO DE DIVISIONES
    minutos_display2:			    DS  1	; EN ESTAS VARIABLES GUARDO EL RESULTDAO DE DIVISIONES
    horas_display3:			    DS  1	; EN ESTAS VARIABLES GUARDO EL RESULTDAO DE DIVISIONES
    horas_display4:			    DS  1	; EN ESTAS VARIABLES GUARDO EL RESULTDAO DE DIVISIONES

;VARIABLES PARA DESPLEGAR 'FECHA'    
    meses_display1:			    DS  1	; EN ESTAS VARIABLES GUARDO EL RESULTDAO DE DIVISIONES PARA DISPLAYS
    meses_display2:			    DS  1	; EN ESTAS VARIABLES GUARDO EL RESULTDAO DE DIVISIONES PARA DISPLAYS 
    dias_display3:			    DS  1	; EN ESTAS VARIABLES GUARDO EL RESULTDAO DE DIVISIONES PARA DISPLAYS 
    dias_display4:			    DS  1	; EN ESTAS VARIABLES GUARDO EL RESULTDAO DE DIVISIONES PARA DISPLAYS

;VARIABLES QUE GUARDAN YA TODOS LOS DATOS CONVERTIDOS EN HEX
    show_display1:			    DS  1	; EN ESTAS VARIABLES GUARDO EL RESULTDAO DE DIVISIONES DE DISPLAY EN HEX
    show_display2:			    DS  1	
    show_display3:			    DS  1	
    show_display4:			    DS  1	; MUESTRO VALROES CONVERTIDOS POR TABLAS 'VALUES'

 ;VARIABLES DISPLAY TIMER
    segundos_timer_d1:		    DS  1	
    segundos_timer_d2:		    DS  1	
    minutos_timer_d3:		    DS  1
    minutos_timer_d4:		    DS  1
 
 ;VARIABLES TIMER
    segundos_timer:		    DS  1		; VARIABLES ACUMULADORAS DEL TIEMPO DEL TIMER
    minutos_timer:		    DS  1		; VARIABLES ACUMULADORAS DEL TIEMPO DEL TIMER
    horas_timer:		    DS  1		; VARIABLES ACUMULADORAS DEL TIEMPO DEL TIMER
    dias_timer:			    DS  1		; VARIABLES ACUMULADORAS DEL TIEMPO DEL TIMER
    meses_timer:		    DS  1		; VARIABLES ACUMULADORAS DEL TIEMPO DEL TIMER
    alarma_timeup:		    DS  1		; VARIABLES ACUMULADORAS DEL TIEMPO DEL TIMER
    alarma_bandera:		    DS	1		; VARIABLES ACUMULADORAS DEL TIEMPO DEL TIMER
    bandera_timer:		    DS  1		; BANDERA QUE ME PERMITE DECREMENTAR TIMER SI ESTÁ ENCENDIDA
    contador_tmr1_timer:	    DS	1		; VARIABLES ACUMULADORAS DEL TIEMPO DEL TIMER
    
 PSECT udata_shr ; COMMON MEMORY
    W_TEMP:	    DS  1	; TAMAÑO: 1 byte
    STATUS_TEMP:    DS  1	; TAMAÑO: 1 byte
  
 ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< RESET >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
 PSECT resVect, class=CODE, abs, delta=2
 ORG 00h	; POSICION 0000h PARA RESET
 resetVec:
     PAGESEL main
     goto main
    
; <<<<<<<<<<<<<<<<<<<<<<<<<< VECTOR INTERRUCPIÓN >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 PSECT intVect, class=CODE, abs, delta=2
 ORG 04h
 ; POSICION 0004h para interrupciones
 push:
    movwf	W_TEMP
    swapf	STATUS, W
    movwf	STATUS_TEMP
 isr:
    btfsc	T0IF		; CHECK BANDERA TMR0
    call	interrupcion_tmr0
    btfsc	TMR1IF		; CHECK BANDERA TMR1
    call	interrupcion_tmr1
    btfsc	TMR2IF		; CHECK BANDERA TMR2
    call	interrupcion_tmr2
    btfsc	RBIF		; CHECK BANDERA RBIF
    call	interrupcion_iocb
 pop:
    swapf	STATUS_TEMP, W
    movwf	STATUS
    swapf	W_TEMP, F
    swapf	W_TEMP, W
    retfie
      
; <<<<<<<<<<<<<<<<<<<<<<<<<<<< SUBRUTINAS DE INTERRUPCIÓN >>>>>>>>>>>>>>>>>>>>>>
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< SUBRUTINAS DE TMR0 >>>>>>>>>>>>>>>>>>>>>>>>>
 interrupcion_tmr0:
    RESET_TMR0		
    ; BAJO FLAG Y RESET SU VALOR
    clrf	PORTD		; APAGO POSIBLES TRANSISTORES ENCEDIDOS
    
    btfss	status_transistores,1	; AL IGUAL QUE CON LOS ESTADOS, HAGO REVISO EN QUE ESTADO SE ENCUENTRA LA VARIABLE
    goto	estado_inicial_tmr0	; ME PERMITE SABER QUE TRANSISTOR DEBO ENCENDER
    goto	estado_secundario_tmr0	
 
    movf	presiono_boton, W
    xorlw	0
    btfsc	STATUS, 2
    goto	estado_0000_tmr0
    
    movf	presiono_boton, W
    xorlw	1
    btfsc	STATUS, 2
    goto	estado_0001_tmr0
    
    movf	presiono_boton, W
    xorlw	2
    btfsc	STATUS, 2
    goto	estado_0010_tmr0
    
    movf	presiono_boton, W
    xorlw	3
    btfsc	STATUS, 2
    goto	estado_0011 
   
    movf	presiono_boton, W
    xorlw	4
    btfsc	STATUS, 2
    goto	estado_0100
    
    movf	presiono_boton, W
    xorlw	5
    btfsc	STATUS, 2
    goto	estado_0110
    return
    
    movf	presiono_boton, W
    xorlw	6
    btfsc	STATUS, 2
    goto	estado_0111
    return
    
 estado_inicial_tmr0:
    btfss	status_transistores,0
    goto	estado_0000_tmr0
    goto	estado_0001_tmr0
   
 estado_secundario_tmr0:
    btfss	status_transistores,0
    goto	estado_0010_tmr0
    goto	estado_0011_tmr0
   
 estado_0000_tmr0:
    incf	status_transistores		; INCREMENTO VARIABLE
    movf	show_display1,w			; VALOR DE VARIABLE --> W
    movwf	PORTA				; W --> PORTA
    bsf		PORTD,4				; ENCIENDO EL TRANSISTOR QUE TOCA
    goto	return_from_tmr0
 
 estado_0001_tmr0:
    incf	status_transistores		; INCREMENTO VARIABLE
    movf	show_display2,W			; VALOR DE VARIABLE --> W
    movwf	PORTA				; W --> PORTA
    bsf		PORTD,5				; ENCIENDO EL TRANSISTOR QUE TOCA
    goto	return_from_tmr0
 estado_0010_tmr0:
    incf	status_transistores		; INCREMENTO VARIABLE
    movf	show_display3,W			; VALOR DE VARIABLE --> W
    movwf	PORTA				; W --> PORTA
    bsf		PORTD,6				; ENCIENDO EL TRANSISTOR QUE TOCA
    goto	return_from_tmr0
 estado_0011_tmr0:
    incf	status_transistores		
    movf	show_display4,W	
    movwf	PORTA		
    bsf		PORTD,7	
    goto	return_from_tmr0
    
 // PARA LOS ESTADOS DEL TIMER Y ALARMA
 
 estado_0100_tmr0:			    ; MUESTRO ESTADO DEL TIMER  99min
    incf	status_transistores		; INCREMENTO VARIABLE
    movf	show_display1,W			; VALOR DE VARIABLE --> W
    movwf	PORTA		
    bsf		PORTD,4		
    goto	return_from_tmr0
 
 estado_0110_tmr0:		; HABILTO OPCION DE EDICION DEL TIMER
    clrf	status_transistores		; INCREMENTO VARIABLE
    movf	show_display4,W	; VALOR DE VARIABLE --> W
    movwf	PORTA		; W --> PORTA
    bsf		PORTD,7		; ENCIENDO EL TRANSISTOR QUE TOCA
    goto	return_from_tmr0
 
 estado_0111_tmr0:		; HABILTO OPCION DE EDICION DEL TIMER
    clrf	status_transistores		; INCREMENTO VARIABLE
    movf	show_display4,W	; VALOR DE VARIABLE --> W
    movwf	PORTA		; W --> PORTA
    bsf		PORTD,7		; ENCIENDO EL TRANSISTOR QUE TOCA
    goto	return_from_tmr0
 
 return_from_tmr0:
    return
    
    ;| DISPLAY 1  | DISPLAY2 | DISPLAY 3  | DISPLAY 4 |
 ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< SUBRUTINAS DE TMR1 >>>>>>>>>>>>>>>>>>>>>>>>>>>>
 interrupcion_tmr1:
    RESET_TMR1 0xB, 0xDC
    incf	segundos	; INCREMENTO MI VARIABLE DE SEGUNDOS
    call	check_flag	; HABILITO OPCION PARA EL TIMER, Y PROCEDO A VERIFICAR BANDERA
    return
; ACTIVACION DE BANDERA PARA DECREMENTAR EL TIMER (MODO):
    
  check_flag:			; VERIFICO BANDERA PARA EL TIMER (MODO)
    btfss   bandera_timer,0	; SOLO SI LA BANDERA ESTA ON, DECREMENTO, SI NO SIGO ESPERANDO 
    return
    decf    segundos_timer
    call    dec_segundos_timer
    return
    
 dec_segundos_timer:		; BANDERA ESTA ON, DECREMENTO MINUTOS TOO
    movf	segundos_timer, W
    sublw	-1		    ; ME SEGURO QUE VAYA DECREMENTANDO 1MIN Y 1SEG
    btfsc	STATUS,2
    call	dec_minutos_timer
    return

 dec_minutos_timer:
    movlw	59
    movwf	segundos_timer
    movf	minutos_timer, W
    sublw	0
    btfsc	STATUS,2
    return
    decf	minutos_timer
    return
  
    
 ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< SUBRUTINAS DE TMR2 >>>>>>>>>>>>>>>>>>>>>>>>>>
 interrupcion_tmr2:
    bcf		TMR2IF
    incf	PORTE		; INCF --> PORTE --> ENCEDER LED PARPADEANTE, CADA 500ms	
    return			; SIMILAR AL LAB06
    
 ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<< SUBRUTINAS DE PORTB >>>>>>>>>>>>>>>>>>>>>>>>>>>
 interrupcion_iocb:
    banksel	PORTB
    clrf	PORTC		; LIMPIO LEDS DE ESTADO, PARA EVITAR ERRORE
    
    btfss	PORTB,4		    ; SALTO DE ESTADO SI EL PULLUP ESTÁ ACTIVO
    incf	presiono_boton	    ; LE DIGO 'CAMBIA DE ESTADO' CADA VEZ QUE OPRIMAN EL BOTON
    
    movf	presiono_boton, W
    sublw	6		    ; LO LIMITO A 7 BOTONAZOS
    btfsc	STATUS, 2
    clrf	presiono_boton		; HAY 6TO BOTONAZO, RETURN A STATUS 0.
    movf	presiono_boton, W
    xorlw	0
    btfsc	STATUS, 2
    goto	estado_0000_cascada
    
    movf	presiono_boton, W
    xorlw	1
    btfsc	STATUS, 2
    goto	estado_0001_cascada
    
    movf	presiono_boton, W
    xorlw	2
    btfsc	STATUS, 2
    goto	estado_0010_cascada
    
    movf	presiono_boton, W
    xorlw	3
    btfsc	STATUS, 2
    goto	estado_0011_cascada
    
    movf	presiono_boton, W
    xorlw	4
    btfsc	STATUS, 2
    goto	estado_0110_cascada
    
    movf	presiono_boton, W
    xorlw	5
    btfsc	STATUS, 2
    goto	estado_0111_cascada
   /* 
    movf	presiono_boton, W
    xorlw	6
    btfsc	STATUS, 2
    goto	estado_0111_cascada
    return
 */
 estado_0000_cascada:			; ESTADO: LECTURA DE HORA
    ; AQUI LOS BOTONES DE INCREMENTO/DECREMENTO NO TIENEN FUNCIONALIDAD PORQUE SOLO ES PARA VER
    bsf		PORTC,7
    bcf		RBIF
    return
 
 estado_0001_cascada:			; ESTADO: LECTURA DE FECHA
    bsf		PORTC,6
    ; AQUI LOS BOTONES DE INCREMENTO/DECREMENTO NO TIENEN FUNCIONALIDAD PORQUE SOLO ES PARA VER
    bcf		RBIF
    return
    
 estado_0010_cascada:			; ESTADO: CONFIGURACION DE HORA
    ; ENCIENDO LEDS QUE NECESITO, APAGO EL RESTO PARA ASEGURARME QUE ESTAN OFF
    bsf		PORTC,7		; ENCIENDO LED DE HORA
    bsf		PORTC,3		; ENCIENDO LED DE CONFIG
    bcf		PORTC,4
    bcf		PORTC,5
    bcf		PORTC,6
    btfss	PORTB,0	
    ; COMIENZO A DECREMENTAR/INCREMENTAR SEGUN BOTONES
    incf	minutos		; INCF --> MINUTOS
    btfss	PORTB,1		
    decf	minutos		; DECF --> MINUTOS
    btfss	PORTB,2		
    incf	horas		; INCF --> HORAS
    btfss	PORTB,3		
    decf	horas		; DECF --> HORAS
    bcf		RBIF
    return
    
 estado_0011_cascada:			; ESTADO: CONFIGURACION DE FECHA
    ; ENCIENDO LEDS QUE NECESITO, APAGO EL RESTO PARA ASEGURARME QUE ESTAN OFF
    bcf		PORTC,7		; ENCIENDO LED DE HORA
    bsf		PORTC,3		; ENCIENDO LED DE CONFIG
    bcf		PORTC,4
    bcf		PORTC,5
    bsf		PORTC,6
    btfss	PORTB,0		
    ; COMIENZO A DECREMENTAR/INCREMENTAR SEGUN BOTONES
    incf	dias		; INCF --> DIAS
    btfss	PORTB,1		
    decf	dias		; DECF --> DIAS
    btfss	PORTB,2		
    incf	meses		; INCF --> MESES
    btfss	PORTB,3		
    decf	meses		; DECF --> MESES
    bcf		RBIF
    return

 estado_0110_cascada:	; EDICION DE TIMER MINUTOS Y SEGUNDOS
    ; RC4 y RC5 para aumentar/decrementar el valor de mins y seg, RC6 para 'aceptar' e inicia el decremento 
    ; RC7 para 'detener' --> 
     ; ENCIENDO LEDS QUE NECESITO, APAGO EL RESTO PARA ASEGURARME QUE ESTAN OFF
    bcf		PORTC,7		
    bsf		PORTC,3		; ENCIENDO LED DE CONFIG
    bcf		PORTC,4
    bsf		PORTC,5		; ENCIENDO LED DE TIMER
    bcf		PORTC,6
    btfss	PORTB,0
    ; COMIENZO A DECREMENTAR/INCREMENTAR SEGUN BOTONES
    incf	minutos_timer		; INCF --> MINUTOS
    btfss	PORTB,1		
    decf	minutos_timer		; DECF --> MINUTOS
    btfss	PORTB,2		
    incf	segundos_timer		; INCF --> SEGUNDOS
    btfss	PORTB,3		
    decf	segundos_timer		; DECF --> SEGUNDOS
    bcf		RBIF
    return
 
  estado_0111_cascada:
    ;IDEA DE FUNCIONAMIENTO DE ESTADO ADICIONAL
    ; Al cambiar al nuevo estado, se habilitan las opciones de:
    ; B0 = COMENZAR CON EL TIMER (SE EMPIEZAN A DECREMENTAR LOS SEGUNDOS Y POR ENDE MINUTOS)
    ; B1 = PARA TIMER, EL TEMPORIZADOR SE DETIENE --> SOLO PARA Y SE QUEDA ASÍ HASTA QUE SE LE INDIQUE LO CONTRARIO.
    ; B2 = APAGAR ALARMA DE QUE SE ACABÓ EL TIEMPO.
    ; B3 = REINICIAR EL TIMER. --> Si se pudiera, si no, pues no. 
    bcf		PORTC,6
    bsf		PORTC,5
    bsf		PORTC,7
    bcf		PORTC,4
    bsf		PORTC,3
    btfsc	PORTB,0
    call	turn_on_flag_timer
    bcf		RBIF	    ; LE DOY BIT CLEAR
    return
  
  turn_on_flag_timer:	    ; SOLAMENTE AL ESTAR LA BANDERA ACTIVADA, EL TIMER INICIA A DECREMENTAR
    bsf	    bandera_timer,0
    return
   
  int_tmr1_para_timer:
    incf    segundos_timer
    movlw   1		    ; DOY VALOR INICIAL DE 1
    subwf   segundos_timer, W
    btfsc   STATUS,2
    call    decrementar_timer
    return
    
  decrementar_timer:
    decf    minutos_timer
    return
 ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 PSECT code, delta=2, abs
 ORG 100h	    ; 100h PARA TABLA DE VALUES

; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TABLAS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 values:
    clrf    PCLATH
    bsf	    PCLATH, 0	; PCLATH = 01	PCL = 02
    addwf   PCL		; PC => PCLATH + PCL + W
    retlw   00111111B	; VALOR = 0
    retlw   00000110B	; VALOR = 1
    retlw   01011011B	; VALOR = 2
    retlw   01001111B	; VALOR = 3
    retlw   01100110B	; VALOR = 4
    retlw   01101101B	; VALOR = 5
    retlw   01111101B	; VALOR = 6
    retlw   00000111B	; VALOR = 7
    retlw   01111111B	; VALOR = 8
    retlw   01101111B	; VALOR = 9
    retlw   00111111B	; VALOR = 0 (EXTRA, PARA EVITAR ERRORES)
 
 ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 PSECT code, delta=2, abs
 ORG 130h	    ; UNA VEZ PASADA LA DIRECCION 100h, PONGO 130h
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 values_months:
    clrf    PCLATH
    bsf	    PCLATH, 0	; PCLATH = 01	PCL = 02
    addwf   PCL		; PC => PCLATH + PCL + W
    retlw   00100000B	; (EXTRA VALUE: evitar errores)
    retlw   00100000B	; VALOR = ENERO
    retlw   00011101B	; VALOR = FEBRERO
    retlw   00100000B	; VALOR = MARZO
    retlw   00011111B	; VALOR = ABRIL
    retlw   00100000B	; VALOR = MAYO
    retlw   00011111B	; VALOR = JUNIO
    retlw   00100000B	; VALOR = JULIO
    retlw   00100000B	; VALOR = AGOSTO
    retlw   00011111B	; VALOR = SEPTIEMBRE
    retlw   00100000B	; VALOR = OCTUBRE
    retlw   00011111B	; VALOR = NOVIEMBRE
    retlw   00100000B	; VALOR = DICIEMBRE
    retlw   00100000B	; (EXTRA VALUE: evitar errores)

; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< CÓDIGO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MAIN >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 main:
    call	config_ins_outs
    call	config_clk
    call	configuracion_tmr0
    call	configuracion_tmr1
    call	configuracion_tmr2
    call	config_int_enable
    call	activo_pullups
    call	configuracion_iocb 
    banksel	PORTA
    banksel	PORTC
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< LOOP PRINCIPAL >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 loop:
    ; PARA MODO DE HORA:
    call	incremento_minutos	; LLAMO A TODAS MIS SUBRUTINAS DE INCREMENTO
    call	incremento_horas
    call	incremento_dias
    call	incremento_meses
    ;call	inc_segundos_timer	; SE PUEDEN DEJAR, PERO LA OTRA FUNCION CUMPLE LO MISMO
    ;call	inc_minutos_timer
    call	decf_segundos_timer
    call	decf_minutos_timer
    call	establecemos_limites		; LLAMO A LOS LIMITES DE LAS VARIABLES QUE INCREMENTAN
    ;EN ESTA MAQUINA CON LA CASCADA ANTERIOR YA ELEGÍ EN QUE ESTADO ESTOY, AQUI SOLO LO MUESTRO
    call	maquina_estados_finitos		; CALL A MI MAQUINA DE ESTADOS FINITOS
    ; BACK AL LOOP
    goto	loop
    
 ;-------------------------------- SUBRUTINAS --------------------------------
 ;---------------------------- ENTRADAS Y SALIDAS ----------------------------
 config_ins_outs:
    banksel	ANSEL
    clrf	ANSEL
    clrf	ANSELH	    ; DEFINO QUE NECESITO PINES DE ENTRADA/SALIDA DIGITALES
    
    ;DEFINIMOS PUERTOS Y CUALES COMO SALIDAS Y ENTRADAS DIGITALES
    banksel	TRISA
    clrf	TRISA	    ; DEFINO MI PORTA COMO SALIDA POR EL MULTIPLEXADO DE DISPLAYS
    bsf		TRISB,0
    bsf		TRISB,1
    bsf		TRISB,2
    bsf		TRISB,3
    bsf		TRISB,4	    ; DEFINO MI PORTB COMO ENTRADA (PUSH-BUTTON)
    
    ; CONFIGURACION DE LEDS INDICADORES DE ESTADO:
    bcf		TRISC,3	    ; LED QUE INDICA MODO DE CONFIGURACIÓN DE ESTADO.
    bcf		TRISC,4	    ; ALERTA DE QUE SE ACABÓ EL TIEMPO, ALARMA
    bcf		TRISC,5	    ; LED DEL TIMER
    bcf		TRISC,6	    ; LED DE FECHA
    bcf		TRISC,7	    ; LED DE HORA
    
    ;DEFINO LOS TRANSISTORES Y LOS PUERTOS EN LOS QUE LOS VOY A CONECTAR:
    bcf		TRISD,4	    ; DEFINO EL PORTD COMO SALIDA PARA TRANSISTORES
    bcf		TRISD,5
    bcf		TRISD,6
    bcf		TRISD,7
    
    ; LED PARA DOS PUNTITOS DEL CENTRO:
    bcf		TRISE,0	    ; LED A 500ms (PUNTOS TITILANTES)
    
    ; HAGO LIMPIEZA GENERAL DE TODO: POR ESTO ESTABA FALLANDO Y MOSTRANDO VALORES RANDOM
    banksel	PORTA
    banksel	PORTC
    clrf	PORTA	    ; LIMPIO TODOS MIS PUERTOS PARA EVITAR ERRORES
    clrf	PORTC	    
    clrf	PORTD	    
    clrf	PORTE
    
    ; LIMPIO VARIABLES
    clrf	segundos_timer
    clrf	minutos_timer
    clrf	segundos
    clrf	minutos
    clrf	horas
    clrf	dias
    clrf	meses
    
    ; INICIO A MOSTRAR VALORES EN DISPLAY DESDE CERO
    clrf	show_display1
    clrf	show_display2
    clrf	show_display3
    clrf	show_display4  ; LIMPIO TODAS LAS VARIABLES DE 'SHOW' VALORES EN DISPLAYS
    
    ; CLR A MIS VARIABLES DE ESTADO
    clrf	status_transistores
    clrf	presiono_boton  ; LIMPIO LAS VARIABLES DE ESTADO DE TRANSISTORES 
    
    ; CLR A VARIABLES GENERALES DE ALMACENAMIENTO DE DATOS
    clrf	segundos    ; LIMPIO VARIABLES DE ALMACENAMIENTO DE VALORES
    clrf	minutos	    
    clrf	horas
    clrf	dias
    bsf		dias,0
    clrf	meses	    
    bsf		meses,0
    ; LE DOY UN VALOR DE 2 INICIAL A 'W' PARA QUE AL HACER LA RESTA, SI ME DA 0, ESTAMOS EN FEBRERO
    movlw	2
    movwf	limite_days_feb
    return
    ; ME MANTENGO EN ESTA SUBRUTINA
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< CONFIG RELOJ >>>>>>>>>>>>>>>>>>>>>>>>>>>
 config_clk:		    ; DETERMINO VELOCIDAD DEL OSCILADOR
    banksel	OSCCON
    bcf		IRCF2
    bsf		IRCF1
    bsf		IRCF0	    ; 011 = RELOJ INTERNO A 500kHz
    bsf		SCS	    ; ACTIVO RELOJ INTERNO
    return
    
  ;BIT 4, EN 1 --> BIT MENOS SIGNIFICATIVO. 
  ;BIT 5, EN 1
  ;BIT 6, EN 0
  //USO PINES 4, 5, 6 ya que me permiten configurar la frecuencia de oscilación
   //UTILIZO RELOJ INTERNO CON UNA FRECUENCIA DE 500kHz (011).
 ; <<<<<<<<<<<<<<<<<<<<<<<<<<<< CONFIGURACION TMR0 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 configuracion_tmr0:
    banksel	OPTION_REG
    bcf		T0CS	    ; USO RELOJ INTERNO
    bcf		PSA	    ; CARGO MI PRESCALER AL TMR0
    bcf		PS2
    bcf		PS1
    bcf		PS0	    ; 000 = PRESCALER, VALOR = 1:2
    RESET_TMR0	    ; LLAMO AL MACRO DEL PRINCIPIO Y CARGO
    return
 ; <<<<<<<<<<<<<<<<<<<<<<<<<<<< CONFIGURACON TMR1 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 configuracion_tmr1:
    banksel	T1CON
    bcf		TMR1GE	    ; DEFINO QUE EL TMR1 ESTÉ SIEMPRE CONTANDO
    bcf		T1CKPS1
    bsf		T1CKPS0	    ; PRESCALER DE 1:2
    bcf		T1OSCEN	    ; OSCILADOR LP = OFF
    bcf		TMR1CS	    ; USO RELOJ INTERNO
    bsf		TMR1ON	    ; ACTIVO RELOJ INTERNO
    RESET_TMR1 0xB, 0xDC	    ; LLAMO AL MACRO DEL PRINCIPIO Y CARGO
    return
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< CONFIGURACION TMR2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 configuracion_tmr2:		    
    banksel	PORTA
    bsf		TMR2ON	    ; ACTIVO EL TMR2
    bsf		TOUTPS3
    bsf		TOUTPS2
    bsf		TOUTPS1
    bsf		TOUTPS0	    ; DEFINO POSTACLER DE 1:16
    bsf		T2CKPS1
    bcf		T2CKPS0	    ; PRESCALER = 16
    banksel	TRISA
    movlw	244	    ; 244 DE VALOR PARA DELAY DE 0.4999s
    movwf	PR2	    ; REDUCIENDO FRECUENCIA DEL RELOJ, PUEDO AUMENTAR EL DELAY, SI NO NO SE PEUDE
    clrf	TMR2	    ; CLR AL TMR2
    bcf		TMR2IF	    ; CLR A BANDERA DE OVERFLOW
    return
; <<<<<<<<<<<<<<<<<<<<<<<<<<< CONFIGURACION INTERRUPCIONES >>>>>>>>>>>>>>>>>>>>>>>>>>
 config_int_enable:
    banksel	TRISA
    bsf		TMR1IE	    ; INTERRUPCION TMR1 = ACTIVADA
    bsf		TMR2IE	    ; INTERRUPCION TMR2 = ACTIVADA
    banksel	T1CON	    ; CAMBIO DE BANCO
    bsf		GIE	    ; INTERRUPCIONES GLOBALES = ACTIVADAS
    bsf		PEIE	    ; INTERRUPCIONES PERIFERICAS = ACTIVADAS
    bsf		RBIE	    ; INTERRUPCIONES PORTB = ACTIVADAS
    bsf		T0IE	    ; INTERRUPCION TMR0 = ACTIVADA
    bcf		T0IF	    ; CLR BANDERA DE OVERFLOW --> TMR0
    bcf		TMR1IF	    ; CLR BANDERA DE OVERFLOW --> TMR1
    bcf		TMR2IF	    ; CLR BANDERA DE OVERFLOW --> TMR2
    bcf		RBIF	    ; CLR BANDERA DE OVERFLOW --> PORTB
    return
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<< CONFIGURACION DE PORTB >>>>>>>>>>>>>>>>>>>>>>>>>
 activo_pullups:
    banksel	WPUB
    bcf		OPTION_REG, 7	; ACTIVO LAS PULLUPS INTERNOS DEL PIC
    bsf		WPUB,0		; WEAK PULLUPS, PORTB = ACTIVADAS
    bsf		WPUB,1
    bsf		WPUB,2
    bsf		WPUB,3
    bsf		WPUB,4		
    bcf		WPUB,5
    bcf		WPUB,6
    bcf		WPUB,7		; SOLO VOY A USAR DEL RB0 - RB4, PERO AJÁ.
    return
    
    
 configuracion_iocb:		; ACTIVO INTERRUPCIONES 'ON CHANGE' DEL PORTB
    banksel	IOCB
    bsf		IOCB,0
    bsf		IOCB,1
    bsf		IOCB,2
    bsf		IOCB,3
    bsf		IOCB,4		
    
    banksel	PORTA
    movf	PORTB, W	; LEO EL PORTB Y PASO A NEXT LINE
    bcf		RBIF		; END AL MISMATCH
    return
 ;<<<<<<<<<<<<<<<<<<<<<<<<< MÁQUINA DE ESTADOS FINITOS (EN BASE A INTERRUPCIONES) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 maquina_estados_finitos:	   ; AJUSTO LOS ESTADOS EN MI 'MÁQUINA DE ESTADOS FINITOS'
   
    movf	presiono_boton, W   ; HAGO LA CASCADA DE ESTADOS QUE HICE EN APARTADOS ANTERIORES, PARA SABER EN QUE ESTADO ESTOY
    xorlw	0		    ; AQUI YA NO DEFINO NUEVAMENTE LE LIMITE DE BOTONAZOS, YA QUE LO HICE ARRIBA
    btfsc	STATUS, 2
    goto	estado_0000
    
    movf	presiono_boton, W   ; ME VOY AL ESTADO 1
    xorlw	1
    btfsc	STATUS, 2
    goto	estado_0001
    
    movf	presiono_boton, W   ; ME VOY AL ESTADO 2
    xorlw	2
    btfsc	STATUS, 2
    goto	estado_0010
    
    movf	presiono_boton, W   ; ME VOY AL ESTADO 3
    xorlw	3
    btfsc	STATUS, 2
    goto	estado_0011
    
    movf	presiono_boton, W   ; ME VOY AL ESTADO 4
    xorlw	4
    btfsc	STATUS, 2
    goto	estado_0100
    
    movf	presiono_boton, W   ; ME VOY AL ESTADO 5
    xorlw	5
    btfsc	STATUS, 2
    goto	estado_0110

    movf	presiono_boton, W   ; ME VOY AL ESTADO 6
    xorlw	6
    btfsc	STATUS, 2
    goto	estado_0111
    return

 ; FUNCIONES: RELOJ DIGITAL, FECHA Y HORA.   
 estado_0000:
    call	divisor_mostrar_time	; MUESTRO HORA, SE AJUSTA AL FORMATO 
    return
    
 estado_0001:
    call	divisor_mostrar_date		; MUESTRO FECHA, SE AJUSTA AL FORMATO 
    return
    
 estado_0010:
    call	divisor_mostrar_time	; MUESTRO HORA, SE AJUSTA AL FORMATO, SE PUEDE MODIFICAR CON BOTONES
    return
    
 estado_0011:
    call	divisor_mostrar_date		; MUESTRO FECHA, SE AJUSTA AL FORMATO, SE PUEDE MODIFICAR CON BOTONES
    return
 
 ; FUNCIONES: ESTADOS DE ALARMA Y TIMER
 
 estado_0100:	; MUESTRO EL TIMER --> SIN HABILITAR MODIFICACIONES
    call	divisor_tiempo_timer
    return
    
 estado_0110:	; TIMER --> HABILITO EDICION DE MINUTOS Y SEGUNDOS --> ENCIENDO LED DE 'HORA'
    call	divisor_tiempo_timer
    return
 
 estado_0111:	; ALARMA --> ACEPTO TIMER, APAGO ALARMA DE TIEMPO CUMPLIDO --> led on: HORA, TIMER, CONFIG
    call	divisor_tiempo_timer
    return

; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< PREPARO MIS DISPLAYS (TMR0) >>>>>>>>>>>>>>>>>>
 ; UTILIZAMOS EL TMR0, PARA IR MOSTRANDO EN CADA DISPLAY SEGUN DECENAS, UNIDADES...
 divisor_mostrar_time:
    movf	minutos, W
    division	10, minutos_display2, minutos_display1  ; DIVISOR DE MINUTOS, MODO: HORA
    call	show_displays_minutos
    
    movf	horas, W
    division	10, horas_display4, horas_display3	    ; DIVISION DE HORAS, MODO: HORA
    call	show_displays_horas
    return
  
 divisor_tiempo_timer:
    movf	segundos_timer, W
    division	10, segundos_timer_d2, segundos_timer_d1 ; DIVISION DE SEGUNDOS, MODO: TIMER
    call	show_displays_segundos_timer
    
    movf	minutos_timer, W
    division	10, minutos_timer_d4, minutos_timer_d3	    ; DIVISION DE MINUTOS, MODO: TIMER
    call	show_displays_minutos_timer
    return
 
 show_displays_minutos:
    movf	minutos_display1, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display1	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    ; *COMENTARIOS APLICAN AL RESTO DE CONFIGURACIONES CON 'SHOW_DISPLAY_VALOR'
    
    movf	minutos_display2, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display2	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    return
   
  show_displays_segundos_timer:
    movf	segundos_timer_d1, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display1	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    
    movf	segundos_timer_d2, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display2	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    return
    
 show_displays_horas:
    movf	horas_display3, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display3	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    
    movf	horas_display4, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display4	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    return
    
  show_displays_minutos_timer:
    movf	minutos_timer_d3, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display3	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    
    movf	minutos_timer_d4, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display4	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    return
 //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
  divisor_mostrar_date:
    // NOS PREPARAMOS A MOSTRAR FECHA EN DISPLAYS
    movf	dias, w	    ; DIAS --> LO PASO A REGISTRO 'W'
    division	10, dias_display4, dias_display3	    ; DIVISOR PARA FECHA
    call	para_mostrar_dias_displays
    
    // LO ANTERIOR FUE PARA DIAS, SEGUIMOS CON MESES PARA FORMATO DE FECHA:
    
    movf	meses, w
    division	10, meses_display2, meses_display1	    ; DIVISOR PARA HORA
    call	para_mostrar_meses_displays
    return
    
    // CONTAMOS LOS DISPLAYS DE DERECHA A IZQUIERDA, LOS DISPLAY 4 Y 3 SON LOS ÚLTIMOS DE IZQUIERDA A DERECHA
 // TRADUCCION DE BINARIO A HEX, USANDO TABLA DE 'VALUES'
 para_mostrar_dias_displays:
    movf	dias_display3, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display3	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    
    movf	dias_display4, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display4	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    return
    
 para_mostrar_meses_displays:
    movf	meses_display1, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display1	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    
    movf	meses_display2, w	; VARIABLE --> W
    call	values		; VALORES --> HEX --> CON VALUES
    movwf	show_display2	; ME MUEVO AL REGISTRO EN EL QUE SUCEDEN MIS INTERRUPCIONES
    return
   ; ///////////////////////////////////////////////////////////////////
; <<<<<<<<<<<<<<<<<<<<<<<<<<< SUBRUTINAS DE LA HORA (TMR1) >>>>>>>>>>>>>>>>>>>>>>>>>
 incremento_minutos:
    movlw	60
    subwf	segundos, w
    btfss	STATUS,2	; SALTO, SI RESULTADO DE RESTA = 0
    goto	return_para_timer1	; ME SALGO DE 'RETURN_PARA_TIMER1'
    clrf	segundos	; REINICIO/RESET A CUENTA DE SEGUNDOS
    incf	minutos		; INCF MINUTOS, AL LLEGAR A 60 SEGUNDOS
    return
   
 incremento_horas:
    movlw	60
    subwf	minutos, w
    btfss	STATUS,2	; SALTO, SI RESULTADO DE RESTA = 0
    goto	return_para_timer1	; ME SALGO DE 'RETURN_PARA_TIMER1'
    clrf	minutos		; REINICIO/RESET A CUENTA DE SEGUNDOS
    incf	horas		; INCF HORAS, AL LLEGAR A 60 MINUTOS
    return
 
 inc_segundos_timer:
    movlw	60
    subwf	segundos_timer, w
    btfss	STATUS,2	; SALTO, SI RESULTADO DE RESTA = 0
    goto	return_para_timer1	; ME SALGO DE 'RETURN_PARA_TIMER1'
    clrf	segundos_timer	; REINICIO/RESET A CUENTA DE SEGUNDOS
    incf	minutos_timer		; INCF MINUTOS, AL LLEGAR A 60 SEGUNDOS
    return
   
 inc_minutos_timer:
    movlw	100
    subwf	segundos_timer, w
    btfss	STATUS,2	; SALTO, SI RESULTADO DE RESTA = 0
    goto	return_para_timer1	; ME SALGO DE 'RETURN_PARA_TIMER1'
    clrf	segundos_timer		; REINICIO/RESET A CUENTA DE SEGUNDOS
    incf	minutos_timer		; INCF MINUTOS, AL LLEGAR A 60 SEGUNDOS
    return
 ; >>> SURUTINAS PARA TIMER:   
 
 // CON ESTO BUSCO PONER LOS LIMITES PARA EL TIMER DE 0-99 EN MINUTOS
 // CON ESTO BUSCO PONER LOS LIMTES PARA SEGUNDOS DEL TIMER 0-59
 // PONER UN VALOR ARRIBA DEL LIMTE QUE BUSCO
 
 decf_segundos_timer:
    movlw	60		; DOY UN VALOR INCICIAL DE 60
    subwf	segundos_timer, w
    btfss	STATUS,2	; SALTO, SI RESULTADO DE RESTA = 0
    goto	return_para_timer1	; ME SALGO DE 'RETURN_PARA_TIMER1'
    clrf	segundos_timer	; REINICIO/RESET A CUENTA DE SEGUNDOS
    decf	minutos_timer		; INCF MINUTOS, AL LLEGAR A 60 SEGUNDOS
    return
   
 decf_minutos_timer:	
    movlw	100		; DOY UN VALOR INCICIAL DE 100
    subwf	segundos_timer, w
    btfss	STATUS,2	; SALTO, SI RESULTADO DE RESTA = 0
    goto	return_para_timer1	; ME SALGO DE 'RETURN_PARA_TIMER1'
    clrf	segundos_timer		; REINICIO/RESET A CUENTA DE MINUTOS
    ;incf	minutos_timer	    ; NO HACE DIFERENCIA, LO PUEDO QUITAR
 ; >>> SURUTINAS PARA TIMER:
 

 ; ---------------------------------------------------------------
 // CON ESTO BUSCO PONER LOS LIMITES TIEMPO DEL DIA 0-23HRS
 // CON ESTO BUSCO PONER LOS LIMTES PARA MINUTOS DEL TIMER 0-59
 // PONER UN VALOR ARRIBA DEL LIMTE QUE BUSCO
 
 incremento_dias:
    movlw	24
    subwf	horas, w
    btfss	STATUS,2	; SALTO, SI RESULTADO DE RESTA = 0
    goto	return_para_timer1	; SALIR DE SUBRUTINA
    clrf	horas		; REINICIO/RESET A CUENTA DE HRS
    incf	dias		; INCF MINUTOS, AL LLEGAR A 60 SEGUNDOS
    return
  
 incremento_meses:
    movf	meses, w	; VALOR INNICIAL --> W
    call	values_months	; LIMITE DEL MES --> W
    subwf	dias, w		; LIMITE - W
    btfss	STATUS,2	; SALTO, SI RESULTADO DE RESTA = 0
    goto	return_para_timer1
    clrf	dias		; SI REACHED EL LIMITE DE DIAS, RESET A VARIABLE 'DIAS'
    bsf		dias,0		; VALOR INICIAL DE DÍAS = 1, YA QUE NO HAY DIA 0
    incf	meses		; INCREMENTO EL VALOR DEL MES
    return
   
 return_para_timer1:
    return
   // LO PONGO AQUI, PARA QUE EL CODIGO NO SE TRASLAPE SI VOLVER AL LOOP O A QUE.
 ; <<<<<<<<<<<<<<<<<<<<<<<<< SUBRUTINAS DE LÍMITES >>>>>>>>>>>>>>>>>>>>>>>>>>>
 establecemos_limites:		; LA DEBO LLAMAR EN EL LOOP PARA QUE ESTE SIEMPRE CORRIENDO
    call	limit_mins	; LLAMO A MIS LIMITES DE MINUTOS
    call	limit_hrs	; LLAMO A MIS LIMITES DE HORAS
    call	limite_minimo_dias  ; LLAMO A MIS LIMITES DE DIAS
    call	limitamos_febrero_28dias    ; LLAMO A MIS LIMITES DE 28 DIAS PARA FEBRERO
    call	limite_minutos_timer	    ; LLAMO A MI LIMITE DE 99MIN PARA EL TIMER
    call	limite_segundos_timer	    ; LLAMO A MIS LIMITES DE 59SEG PARA TIMER
    call	limite_min_meses    ; LLAMO A MIS LIMITES DE MESES
    call	limite_max_meses    ; LLAMO A MIS LIMITES DE MESES PARA MAXIMO DE 12 MESES
    return
   ; DESCRIPCION DE SUBRUTINAS DE LIMITES
   ; VAMOS EN ORDEN: MINS, HORAS, 
 limit_mins:
    ; BCF = APAGAR, BSF = ENCENDER --> EQUIVALENTE A 1 y 0
    btfss	minutos,7	; ANALIZO NIBBLES, EVALUANDO EL BIT MÁS SIGNIFICATIVO
    goto	return_a_limites_hr_fecha
    bcf		minutos,7
    bcf		minutos,6
    bsf		minutos,5
    bsf		minutos,4
    bsf		minutos,3
    bcf		minutos,2
    bsf		minutos,1
    bsf		minutos,0	; ACTUALIZO EL VALOR DE MINUTOS A 59 -> 00111011B --> UNDERFLOW/OVERFLOW
    return
   
 limit_hrs:
    btfss	horas,7		; ANALIZO NIBBLES, EVALUANDO EL BIT MÁS SIGNIFICATIVO
    goto	return_a_limites_hr_fecha
    ; BCF = APAGAR, BSF = ENCENDER --> EQUIVALENTE A 1 y 0
    bcf		horas,7
    bcf		horas,6
    bcf		horas,5
    bsf		horas,4
    bcf		horas,3
    bsf		horas,2
    bsf		horas,1
    bsf		horas,0		; ACTUALIZO EL VALOR DE HORAS A 23 -> 00010111B --> UNDERFLOW/OVERFLOW
    return
 
 limite_minutos_timer:
    btfss	minutos_timer,7		; ANALIZO NIBBLES, EVALUANDO EL BIT MÁS SIGNIFICATIVO
    goto	return_a_limites_hr_fecha
    ; BCF = APAGAR, BSF = ENCENDER --> EQUIVALENTE A 1 y 0
    bcf		minutos_timer,7
    bsf		minutos_timer,6
    bsf		minutos_timer,5
    bcf		minutos_timer,4
    bcf		minutos_timer,3
    bcf		minutos_timer,2
    bsf		minutos_timer,1
    bsf		minutos_timer,0		; ACTUALIZO EL VALOR DE HORAS A 99 -> 01100011B --> UNDERFLOW/OVERFLOW
    return  
    
 limite_segundos_timer:
    btfss	segundos_timer,7	; ANALIZO NIBBLES, EVALUANDO EL BIT MÁS SIGNIFICATIVO
    goto	return_a_limites_hr_fecha
   ; BCF = APAGAR, BSF = ENCENDER --> EQUIVALENTE A 1 y 0
    bcf		segundos_timer,7
    bcf		segundos_timer,6
    bsf		segundos_timer,5
    bsf		segundos_timer,4
    bsf		segundos_timer,3
    bcf		segundos_timer,2
    bsf		segundos_timer,1
    bsf		segundos_timer,0	; ACTUALIZO EL VALOR DE MINUTOS A 59 -> 00111011B --> UNDERFLOW/OVERFLOW
    return
    
 limite_minimo_dias:		; VOY INCREMENTANDO DE 0 -> 1, EN AUTOMATICO
    ; USAMOS BTFSC PARA SKIP SI ESTA EN 0 Y ME QUEDO ESPERANDO A QUE CAMBIE, POR ESO EL RETURN
    btfsc	dias,7		; VERIFICO QUE LOS BITS SEAN 0
    goto	return_a_limites_hr_fecha
    btfsc	dias,6
    goto	return_a_limites_hr_fecha
    btfsc	dias,5
    goto	return_a_limites_hr_fecha
    btfsc	dias,4
    goto	return_a_limites_hr_fecha
    btfsc	dias,3
    goto	return_a_limites_hr_fecha
    btfsc	dias,2
    goto	return_a_limites_hr_fecha
    btfsc	dias,1
    goto	return_a_limites_hr_fecha
    btfsc	dias,0
    goto	return_a_limites_hr_fecha
    
    movf	meses,w		; MUEVO EL VALOR DE MESES A 'W'
    call	values_months	; VERIFICO EL NUMERO DE DIAS QUE TENGA EL MES
    movwf	limite_days	; MUEVO EL VALOR DEL DIAS A LA VARIABLE DE 'LIMITE_DAYS'
    decf	limite_days	; LE RESTO '1' PARA AJUSTAR AL LÍMITE DE DIAS
    movf	limite_days,w	; MUEVO EL VALOR QUE ESTABA EN MI VARIABLE A 'W'
    movwf	dias		; ME PREPARO PARA MOSTRAR EL RESULTADO 'W' --> 'DIAS'
    return
 
 limite_min_meses:		; UNDERFLOW: INCF de 0 a 12
    ; USAMOS BTFSC PARA SKIP SI ESTA EN 0 Y ME QUEDO ESPERANDO A QUE CAMBIE, POR ESO EL RETURN
    btfsc	meses,7		; CHECK PARA QUE CADA BIT SEA 0
    goto	return_a_limites_hr_fecha
    btfsc	meses,6
    goto	return_a_limites_hr_fecha
    btfsc	meses,5
    goto	return_a_limites_hr_fecha
    btfsc	meses,4
    goto	return_a_limites_hr_fecha
    btfsc	meses,3
    goto	return_a_limites_hr_fecha
    btfsc	meses,2
    goto	return_a_limites_hr_fecha
    btfsc	meses,1
    goto	return_a_limites_hr_fecha
    btfsc	meses,0
    goto	return_a_limites_hr_fecha
    
    movlw	12		; DOY EL VALOR DE '12' A MESES, Y LO PASO A 'W'
    movwf	meses
    return
   
 limite_max_meses:		; RUTINA PARA LIMITAR EL MÁXIMO DE MESE
    ; USAMOS BTFSC PARA SKIP SI ESTA EN 0 Y ME QUEDO ESPERANDO A QUE CAMBIE, POR ESO EL RETURN
    btfsc	meses,7		; OVERFLOW; check cada bit para 13 = 0000 1101
    goto	return_a_limites_hr_fecha
    btfsc	meses,6
    goto	return_a_limites_hr_fecha
    btfsc	meses,5
    goto	return_a_limites_hr_fecha
    btfsc	meses,4
    goto	return_a_limites_hr_fecha
    btfss	meses,3
    goto	return_a_limites_hr_fecha
    btfss	meses,2
    goto	return_a_limites_hr_fecha
    btfsc	meses,1
    goto	return_a_limites_hr_fecha
    btfss	meses,0
    goto	return_a_limites_hr_fecha
    
    clrf	meses		; LIMPIO MI VARIABLE DE MESE
    bsf		meses,0		; MESES: 0 --> 1
    return
 ; IMPORTANTE PARA FEBREROOOOOOOOOOOOOOOOOOOOOOO:  
 limitamos_febrero_28dias:
    ; USAMOS BTFSC PARA SKIP SI ESTA EN 0 Y ME QUEDO ESPERANDO A QUE CAMBIE, POR ESO EL RETURN
    movf	meses,W
    subwf	limite_days_feb,W	; HAGO UNA RESTA DE 2-2 = 0 Y SI DA '0' ME MANTENGO EN LA RUTINA, SI DA '1', SIGUE EL CODIGO A LA NEXT LINEA
    btfss	STATUS,2		; HAY SALTO SI EL MES SI ES FEBRERO
    goto	return_a_limites_hr_fecha
    
    btfsc	dias,7		; OVERFLOW: CHECKEO CADA BIT PARA EVITAR EL VALOR '13'
    goto	return_a_limites_hr_fecha
    btfsc	dias,6
    goto	return_a_limites_hr_fecha
    btfsc	dias,5
    goto	return_a_limites_hr_fecha
    btfss	dias,4
    goto	return_a_limites_hr_fecha
    btfss	dias,3
    goto	return_a_limites_hr_fecha
    btfss	dias,2
    goto	return_a_limites_hr_fecha
    btfss	dias,1
    goto	return_a_limites_hr_fecha
    btfss	dias,0
    goto	return_a_limites_hr_fecha
    
    clrf	dias		; LIMPIO PARA QUE VUELVA A '28' SI ES FEBRERO
    ; USAMOS BTFSC PARA SKIP SI ESTA EN 0 Y ME QUEDO ESPERANDO A QUE CAMBIE, POR ESO EL RETURN
    
    
    ; USAMOS BTFSC PARA SKIP SI ESTA EN 0 Y ME QUEDO ESPERANDO A QUE CAMBIE, POR ESO EL RETURN
    btfsc	dias,7		; REVISO CADA BIT = 30 = 0001 1110
    goto	return_a_limites_hr_fecha
    btfsc	dias,6
    goto	return_a_limites_hr_fecha
    btfsc	dias,5
    goto	return_a_limites_hr_fecha
    btfss	dias,4
    goto	return_a_limites_hr_fecha
    btfss	dias,3
    goto	return_a_limites_hr_fecha
    btfss	dias,2
    goto	return_a_limites_hr_fecha
    btfss	dias,1
    goto	return_a_limites_hr_fecha
    btfsc	dias,0
    goto	return_a_limites_hr_fecha
    
    clrf	dias		; RETORNA A '28'
    return
   // RECORDAR: SIEMPRE DEBO LIMPIAR MIS VARIABLES APRA EVITAR MOSTRAR VALORES ALEATORIOS
 return_a_limites_hr_fecha:
    return
     // LO PONGO AQUI, PARA QUE EL CODIGO NO SE TRASLAPE SI VOLVER AL LOOP O A QUE.
END











