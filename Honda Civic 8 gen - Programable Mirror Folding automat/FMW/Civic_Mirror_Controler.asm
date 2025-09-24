; ===========================================================================================================================
; Program koji upravlja sa modulom za kontrolu retrovizora preko daljinkog od otkljuèavanja i zakljuèavanja vrata
; dizajniran za Hondu Civic 8 gen, ali trebao bi moæi raditi na bilo kojem autu sliène konfiguracije
; (mehanièkog prekidaèa za položaj retrovizora i da ima SOD module u retrovizorima)
; Ver 2 - 17.07.2017
; Pojaèano debounce filtriranje, uveden RETRACT_DEBOUNCE_PAR
; Riješen bug kod dolaska napajanja da se releji ne aktiviraju na kratko
; Promijenjena frekvencija HI i LO tona, da se bolje èuje
; ===========================================================================================================================
	
	LIST P=12F1501, R=DEC
	#include "P12F1501.INC"


	__CONFIG _CONFIG1, _CLKOUTEN_OFF & _BOREN_NSLEEP & _CP_ON & _MCLRE_OFF & _PWRTE_ON & _WDTE_OFF & _FOSC_INTOSC
	__CONFIG _CONFIG2, _LVP_OFF & _BORV_LO & _STVREN_ON & _WRT_OFF 

; Internal oscilator 4MHz


;			Absolute definition variables

W_temp	equ	0x7F


	CBLOCK	0x20 		; rezerviranje mjesta u RAM-u

	MY_FLAGS_1
	MY_FLAGS_2
	MY_FLAGS_3
	RTC1_cnt
	RTC2_cnt
	RTC3_cnt
	RTC4_cnt
	RTC5_cnt
	RTC6_cnt
	MAIN_POINTER
	BUZZER_INDEX
	BEEP_COUNTER
	BEEP_cnt
	LOCK_cnt
	UNLOCK_cnt
	EXT_DURATION_cnt
	RET_EXT_DURATION_cnt
	PROG_WINDOW_cnt
	SWITCH_RETRACT_cnt
	UNLOCK_DEBOUNCE_filter
	RETRACT_DEBOUNCE_filter
	CONTACT_DEBOUNCE_filter
	LOCK_DEBOUNCE_filter
	BEEP_TONE_TIMER_cnt
	CONTACT_PAUSE_cnt
	AUTORESET_cnt
	SLEEP_cnt

	endc

	CBLOCK	0x7E	    ; dostupne varijable iz svih bankova

	DEVICE_MODE
	temp

	endc


;--------DEFINITIONS--------------------------

#define		RTC_1s						MY_FLAGS_1,0
#define		RTC_10ms					MY_FLAGS_1,1
#define		RTC_100ms					MY_FLAGS_1,2
#define		RTC_500ms					MY_FLAGS_1,3
#define		RTC_10s						MY_FLAGS_1,4
#define		TIME_ELAPSED					MY_FLAGS_1,5
#define		PWR_UP_flag					MY_FLAGS_1,6
	
#define		CONTACT_OFF_AC_flag				MY_FLAGS_2,0
#define		CONTACT_ON_flag					MY_FLAGS_2,1
#define		SW_RETRACT_ON_flag				MY_FLAGS_2,2
#define		LOCK_ON_flag					MY_FLAGS_2,3
#define		UNLOCK_ON_flag					MY_FLAGS_2,4
#define		SW_RETRACT_AC_flag				MY_FLAGS_2,5
#define		BUZZER_TU_TII_flag				MY_FLAGS_2,6
#define		BUZZER_TI_TUU_flag				MY_FLAGS_2,7
	
;-------------------- CONSTANTS ----------------------------------------


FALSE				EQU	0
TRUE				EQU	1
RET_EXT_DURATION_PAR		EQU	100	; x 100 ms   120
PROG_WINDOW_PAR			EQU	30	; x 100 ms , vremenski prozor za ulazak u mod programiranja
						; i timeout unutar moda programiranja
SWITCH_RETRACT_PAR		EQU	3
LOW_TONE_PAR			EQU	64
HIGH_TONE_PAR			EQU	59
SHORT_BEEP_TIMER_PAR		EQU	20	; x 10ms - bipanje, prvi ton
LONG_BEEP_TIMER_PAR		EQU	30	; x 10ms - bipanje, drugi ton
BEEP_PAUSE_TIMER_PAR		EQU	80	; x 10ms - pauza nakon bipanja
AUTORESET_PAR			EQU	30	; x 1s - autoreset LOCK i UNLOCK brojaca
SLEEP_PAR			EQU	18	; x10s - vrijeme za spavanje
RETRACT_DEBOUNCE_PAR		EQU	20	; x10ms - debounce filter za prekidac retrovizora
		

;-------------------- I/O Pins -----------------------------------------

#define		UNLOCK_IN			PORTA,0
#define		LOCK_IN				PORTA,2
#define		CONTACT_IN			PORTA,3
#define		SW_RETRACT_IN			PORTA,1
#define		BYPASS_OUT			PORTA,4
#define		EXT_RETRACT_OUT			PORTA,5				
				
;----------------------- MACROS -----------------------------------------
				
BANK_0	macro
	BANKSEL	PORTA
	endm
	
RELAY_RETRACT_POSITION	macro
	bsf	EXT_RETRACT_OUT
	endm
	
RELAY_EXTEND_POSITION	macro
	bcf	EXT_RETRACT_OUT
	endm
	
RELAY_BYPASS_ON	    macro
	bcf	BYPASS_OUT
	endm
	
RELAY_BYPASS_OFF    macro
	bsf	BYPASS_OUT
	endm
	
				
;===============================================================================



	ORG	0

	GOTO	Main
	
	ORG	4	; Interrupt vektor

	GOTO	Interrupt
	

;====================================================================
;			Main Program
;====================================================================
Main
	
	CLRF	INTCON  		; onemoguæi sve interrupte, obrisi sve flagove
								; od moguæih interrupta
	BANKSEL	PIR1
	CLRF	PIR1			; Obrisi sve flagove od perifernih interrupta
	CLRF	PIR2
		
	CLRF	T1CON
	movlw	B'00000001'			; TMR2 Postscale 1:1, TMR2 OFF (za sada), Prescale 1:4
	movwf	T2CON
	movlw	h'FF'
	movwf	PR2
		
        BANKSEL PORTA
        CLRF    PORTA		;Init PORTA
        BANKSEL LATA		;Data Latch
	movlw	B'00110000'	;ugasi releje BYPASS i RETRACT preko LATA registara
        movwf   LATA
        BANKSEL ANSELA
        CLRF    ANSELA		;digital I/O
	
	    ;bank 1 -------------------------------
	BANKSEL	TRISA
	MOVLW	B'11001111'		; PORTA 4 i 5 su izlazi, ostalo ulazi
	MOVWF	TRISA

	MOVLW	B'00000001'		; PORT_B pull-ups omoguæeni, prescaler TMR0 - 1:4
	MOVWF	OPTION_REG	

	movlw	B'01101010'		; Internal Oscilator, 4 MHZ, 
	movwf	OSCCON						

	CLRF	PIE1
	bsf	PIE1,TMR2IE		; omoguci TMR2 prekid			
	CLRF	PIE2	

		; bank 4 -----------------------------------
	BANKSEL	WPUA
	movlw	B'00001101'
	movwf	WPUA		; ukljuèi pull-up na PORTA 0, 2 i 3
	
		; bank 12 ----------------------------------	

	BANKSEL	PWM1CON
	clrf	PWM1CON
	


		; bank 0 ----------------------------------
	
	BANKSEL	IOCAP
	movlw	B'00001101'
	movwf	IOCAP	    ; omoguæi unterrupt na rastuci brid na PORTA 0, 2 i 3 pinu 
	movwf	IOCAN	    ; omoguæi unterrupt na padajuci brid na PORTA 0, 2 i 3 pinu
;	BANKSEL	VREGCON
;	bcf	VREGCON,VREGPM	    ; omoguci Low power sleep down mode
		
	BANKSEL	PORTA
	RELAY_BYPASS_OFF
	RELAY_RETRACT_POSITION	

	movlw	h'20'		; delete RAM from h'20' to h'7F' 
	movwf	FSR0L
	clrf	FSR0H
Loop
	clrf	INDF0
	movf	FSR0L,W
	sublw	h'7F'
	btfsc	STATUS,Z
	goto	Finish_Loop
	incf	FSR0L,F
	goto	Loop
Finish_Loop
	
;----------------------------------------------------------------------------------------
	bcf	INTCON,TMR0IF
	bsf	INTCON,TMR0IE	; omoguci TMR0 prekid
	bsf	INTCON,IOCIE	; omoguci IOCIE prekid (prekid kod promjene stanja bilo kojeg pina
	clrf	TMR0
	bsf	INTCON,GIE


	
	
;-----------------------------------------------------------------------------------------
;*****************************************************************************************
;/////////////////////////////////////////////////////////////////////////////////////////
; 
; MAIN LOOP IS HERE
;
;/////////////////////////////////////////////////////////////////////////////////////////
;*****************************************************************************************
;-----------------------------------------------------------------------------------------

DUTY_LOOP
	CALL	RTC
	CALL	INPUT_HANDLER
	CALL	MAIN_HANDLER
	CALL	BUZZER_HANDLER
	CALL	WATCHDOG_MONITOR
		
WAIT_TO_TIME_ELAPSE

	BTFSS	TIME_ELAPSED
	GOTO	WAIT_TO_TIME_ELAPSE
	BCF	TIME_ELAPSED
		
	GOTO	DUTY_LOOP

;===================================================================
;			Interrupt rutina
;===================================================================

Interrupt
				; Push	 Potprogram za spremanje W, OPTION,
				; STATUS, PCLATH i FSR registara
	btfsc	INTCON,TMR0IF
	goto	TMR0_interrupt	; prekid TMR0
	btfsc	INTCON,IOCIF
	goto	IOC_Interrupt	; prekid promjene stanja pracenih pinova
	RETFIE
	
TMR0_interrupt
	bcf	INTCON,TMR0IF
	bsf	TIME_ELAPSED
	movlw	9	
	movwf	TMR0				; korekcija TMR0 brojaèa da interrupt bude što bliže 1ms na 4MHz oscilatoru
	RETFIE

IOC_Interrupt
	BANKSEL	IOCAF
	;----------------------------------
	movlw 0xff	; procedura brisanja IOC flagova
	xorwf IOCAF, W	; vidi datasheet, tocka  12.4
	andwf IOCAF, F
	;-----------------------------------
	BANKSEL	PORTA
	clrf	SLEEP_cnt
	RETFIE
	
;-------------------------------------------------------------------

RTC		; subroutine which set flags:
		; RTC_10ms		- every 10ms
		; RTC_100ms		- every 100ms
		; RTC_500ms		- every 500ms
		; RTC_1s		- every 1s
		; RTC_10s		- every minute
				

	bcf	RTC_10ms
	bcf	RTC_100ms
	bcf	RTC_500ms
	bcf	RTC_1s
	bcf	RTC_10s

	incf	RTC1_cnt,F
	movlw	10
	subwf	RTC1_cnt,W
	btfss	STATUS,Z
	return
	clrf	RTC1_cnt
	bsf	RTC_10ms
	incf	RTC2_cnt,F
	movlw	10
	subwf	RTC2_cnt,W
	btfss	STATUS,Z 
	return
	clrf	RTC2_cnt
	bsf	RTC_100ms
	incf	RTC3_cnt,F
	movlw	5
	subwf	RTC3_cnt,W
	btfss	STATUS,Z
	return
	clrf	RTC3_cnt
 	bsf	RTC_500ms
	incf	RTC4_cnt,F
	movlw	2
	subwf	RTC4_cnt,W
	btfss	STATUS,Z
	return
	clrf	RTC4_cnt
	bsf	RTC_1s
	incf	RTC5_cnt,F
	movlw	10
	subwf	RTC5_cnt,W
	btfss	STATUS,Z
	return
	clrf	RTC5_cnt
	bsf	RTC_10s

	return


;=================================================================================================================

MAIN_HANDLER
	btfss	RTC_10ms
	return
	movlw	HIGH	MAIN_STATE
	movwf	PCLATH
	movf	MAIN_POINTER,W
	goto	MAIN_STATE

;---------------------------------------------------------------------
POWER_UP				; MAIN_POINTER = 0

	btfss	RTC_1s
	return
	bsf	PWR_UP_flag

	BANKSEL PMADRL	    ; Select Bank for PMCON registers
	MOVLW	h'FF'
	MOVWF	PMADRL	    ; Store LSB of address
	MOVLW	h'03'		    ; adresa 0x3FF je zadnji bajt flash memorije 1kB
	MOVWF	PMADRH	    ; Store MSB of address
	BCF	PMCON1,CFGS ; Do not select Configuration Space
	BSF	PMCON1,RD   ; Initiate read
	NOP	    ;Ignored 
	NOP	    ;Ignored 
	MOVF	PMDATL,W    ; Get LSB of word
	MOVWF	DEVICE_MODE ; Store in user location
	BANK_0

	movwf	BEEP_COUNTER
	incf	BEEP_COUNTER,F	    ; BEEP_COUNTER + 1 , ako je 0 bipni jednom, ... ako je 3 bipni 4 puta
	incf	MAIN_POINTER,F
	bcf	CONTACT_OFF_AC_flag
	return

;---------------------------------------------------------------------
CONTACT_ON				; MAIN_POINTER = 1
	btfsc	CONTACT_ON_flag		; da li je kontakt ON ?
	bsf	CONTACT_OFF_AC_flag	; ako je postavi AC flag
	btfsc	CONTACT_ON_flag
	return				; i izadji van
	btfsc	CONTACT_OFF_AC_flag	; ako je kontakt OFF, provjeri da li je u prijasnjem ciklusu bio kontakt ON
	goto	DETECTED_CONTACT_CHANGE_TO_OFF	; ako je tada je detektirana promjena kontakta ON => OFF
	btfss	RTC_100ms		; u prijasnjem ciklusu kontakt nije bio OFF	
	return			    
	incf	CONTACT_PAUSE_cnt,F	; svakih 100ms povecaj CONTACT_PAUSE_cnt
	movf	CONTACT_PAUSE_cnt,W	; pauza 2s koja ima svrhu da nakon izvrsene EXTEND/RETRACT akcije 
	sublw	20			; proces se vraca u ovaj state, 2 sec pauza je dodana da se ignorora pracenje 
					; LOCK/UNLOCK ulaza i tako sprijeci nezeljeno okidanje ili neke druge smetnje
	btfss	STATUS,Z		; da li je proslo 2s?
	return				; ako nije, izadji van
	clrf	CONTACT_PAUSE_cnt	
	incf	MAIN_POINTER,F		; nakon 2 sec idi u sljedeci state
	clrf	LOCK_cnt		; obrisi LOCK i UNLOCK brojace
	clrf	UNLOCK_cnt
	return
DETECTED_CONTACT_CHANGE_TO_OFF
	bcf	CONTACT_OFF_AC_flag ; detektirana je promjena kontakta ON=>OFF ,
	movlw	5		    ; idi u pre-program mode
	movwf	MAIN_POINTER
	clrf	SWITCH_RETRACT_cnt
	return
	
;---------------------------------------------------------------------
STAND_BY				; MAIN_POINTER = 2 , contact OFF
	btfsc	CONTACT_ON_flag
	goto	RETURN_BACK_TO_CONTACT_ON
	movf	DEVICE_MODE,W
	movwf	temp
	btfsc	STATUS,Z
	goto	STANDBY_MODE_0
	decf	temp,F
	btfsc	STATUS,Z
	goto	STANDBY_MODE_1
	decf	temp,F
	btfsc	STATUS,Z
	goto	STANDBY_MODE_2
	decf	temp,F
	btfsc	STATUS,Z
	goto	STANDBY_MODE_3
	goto	STANDBY_MODE_4
	return
	
RETURN_BACK_TO_CONTACT_ON
	decf	MAIN_POINTER,F
	return

STANDBY_MODE_0		    ; mod 1 - 1x zakljucavanje - sklopi / 1x otkljucavanje - otklopi
	movf	LOCK_cnt,W	    ; LOCK_cnt - brojac koji broji koliko se puta pojavio LOCK signal
	sublw	1
	btfss	STATUS,Z
	goto	NEXT_MODE_0
	incf	MAIN_POINTER,F
	goto	STANDBY_COMMON
NEXT_MODE_0
	movf	UNLOCK_cnt,W	    ; UNLOCK_cnt - brojac koji broji koliko se puta pojavio UNLOCK signal
	sublw	1
	btfss	STATUS,Z
	goto	STANDBY_COMMON
	incf	MAIN_POINTER,F
	incf	MAIN_POINTER,F
	goto	STANDBY_COMMON

STANDBY_MODE_1		    ; mod 2 - 1x zakljucavanje - sklopi / otkljucavanje - ignoriraj
	movf	LOCK_cnt,W	    ; LOCK_cnt - brojac koji broji koliko se puta pojavio LOCK signal
	sublw	1
	btfss	STATUS,Z
	goto	NEXT_MODE_1
	incf	MAIN_POINTER,F
	goto	STANDBY_COMMON
NEXT_MODE_1
	movf	UNLOCK_cnt,W	    ; UNLOCK_cnt - brojac koji broji koliko se puta pojavio UNLOCK signal
	sublw	1
	btfss	STATUS,Z
	goto	STANDBY_COMMON
	movlw	1
	movwf	BEEP_COUNTER	    ; 1x BIP , ignorirano zakljucavanje
	movwf	MAIN_POINTER
	goto	STANDBY_COMMON
	
STANDBY_MODE_2		    ; mod 3 - 2x zakljucavanje - sklopi / 2x otkljucavanje - otklopi
	movf	LOCK_cnt,W	    ; LOCK_cnt - brojac koji broji koliko se puta pojavio LOCK signal
	sublw	2
	btfss	STATUS,Z
	goto	NEXT_MODE_2
	incf	MAIN_POINTER,F
	goto	STANDBY_COMMON
NEXT_MODE_2
	movf	UNLOCK_cnt,W	    ; UNLOCK_cnt - brojac koji broji koliko se puta pojavio UNLOCK signal
	sublw	2
	btfss	STATUS,Z
	goto	STANDBY_COMMON
	incf	MAIN_POINTER,F
	incf	MAIN_POINTER,F
	goto	STANDBY_COMMON

STANDBY_MODE_3		    ; mod 4 - 2x zakljucavanje - sklopi / otkljucavanje - ignoriraj
	movf	LOCK_cnt,W	    ; LOCK_cnt - brojac koji broji koliko se puta pojavio LOCK signal
	sublw	2
	btfss	STATUS,Z
	goto	NEXT_MODE_3
	incf	MAIN_POINTER,F
	goto	STANDBY_COMMON
NEXT_MODE_3
	movf	UNLOCK_cnt,W	    ; UNLOCK_cnt - brojac koji broji koliko se puta pojavio UNLOCK signal
	sublw	2
	btfss	STATUS,Z
	goto	STANDBY_COMMON
	movlw	1
	movwf	BEEP_COUNTER	    ; 1x BIP , ignorirano zakljucavanje
	movwf	MAIN_POINTER
	goto	STANDBY_COMMON

STANDBY_MODE_4			    ; sve funkcije ugašene
	movf	LOCK_cnt,W	    ; LOCK_cnt - brojac koji broji koliko se puta pojavio LOCK signal
	sublw	1
	btfss	STATUS,Z
	goto	NEXT_MODE_4
	movlw	1
	movwf	BEEP_COUNTER	    ; 1x BIP , ignorirano zakljucavanje
	movwf	MAIN_POINTER
	goto	STANDBY_COMMON
NEXT_MODE_4
	movf	UNLOCK_cnt,W	    ; UNLOCK_cnt - brojac koji broji koliko se puta pojavio UNLOCK signal
	sublw	1
	btfss	STATUS,Z
	goto	STANDBY_COMMON
	movlw	1
	movwf	BEEP_COUNTER	    ; 1x BIP , ignorirano zakljucavanje
	movwf	MAIN_POINTER

STANDBY_COMMON
	btfss	RTC_10s
	return
	incf	SLEEP_cnt,F
	movf	SLEEP_cnt,W
	sublw	SLEEP_PAR
	btfss	STATUS,Z
	return
	clrf	SLEEP_cnt
	
	BANKSEL	IOCAP	    
	;----------------------------------
	movlw	0xff		; procedura brisanja IOC flagova
	xorwf	IOCAF, W	; vidi datasheet, tocka  12.4
	andwf	IOCAF, F
	;-----------------------------------
	bsf	IOCAP,2	    ; omoguæi interrupt na rastuci brid na PORTA 2 pinu 
	bsf	IOCAN,2	    ; omoguæi interrupt na padajuci brid na PORTA 2 pinu
	BANK_0
	
	SLEEP
	nop
	return
	
;---------------------------------------------------------------------
MIRROR_RETRACT				; MAIN_POINTER = 3
	btfsc	SW_RETRACT_ON_flag
	goto	RETRACT_GIVE_UP
	btfsc	UNLOCK_ON_flag
	goto	RETRACT_FINISH
	btfsc	CONTACT_ON_flag
	goto	RETRACT_FINISH
	movf	RET_EXT_DURATION_cnt,W
	sublw	RET_EXT_DURATION_PAR
	btfsc	STATUS,Z
	goto	RETRACT_FINISH
	btfss	RTC_100ms
	return
	movf	RET_EXT_DURATION_cnt,W
	btfsc	STATUS,Z		; postavi RET/EXT relej kad je RET_EXT_DURATION_cnt=0
	RELAY_RETRACT_POSITION		; tj. na poèetku procesa
	decf	RET_EXT_DURATION_cnt,W	; ukljuèi BYPASS relej kad je RET_EXT_DURATION_cnt=1
	btfsc	STATUS,Z		; tj. 100 ms kasnije
	RELAY_BYPASS_ON
	incf	RET_EXT_DURATION_cnt,F
	return

RETRACT_GIVE_UP
	movlw	2
	movwf	BEEP_COUNTER
RETRACT_FINISH
	RELAY_BYPASS_OFF
	RELAY_RETRACT_POSITION	    ; OFF
	clrf	RET_EXT_DURATION_cnt
	movlw	1
	movwf	MAIN_POINTER
;	clrf	LOCK_cnt
;	clrf	UNLOCK_cnt
	return
	
;---------------------------------------------------------------------
MIRROR_EXTEND				; MAIN_POINTER = 4
	btfsc	SW_RETRACT_ON_flag
	goto	EXTEND_GIVE_UP
	btfsc	LOCK_ON_flag
	goto	EXTEND_FINISH
	btfsc	CONTACT_ON_flag
	goto	EXTEND_FINISH
	movf	RET_EXT_DURATION_cnt,W
	sublw	RET_EXT_DURATION_PAR
	btfsc	STATUS,Z
	goto	EXTEND_FINISH
	btfss	RTC_100ms
	return
	movf	RET_EXT_DURATION_cnt,W
	btfsc	STATUS,Z	    ; postavi RET/EXT relej kad je RET_EXT_DURATION_cnt=0
	RELAY_EXTEND_POSITION	    ; tj. na poèetku procesa
	decf	RET_EXT_DURATION_cnt,W
	btfsc	STATUS,Z	    ; ukljuèi BYPASS relej kad je RET_EXT_DURATION_cnt=1
	RELAY_BYPASS_ON		    ; tj. 100 ms kasnije
	incf	RET_EXT_DURATION_cnt,F
	return

EXTEND_GIVE_UP
	movlw	2
	movwf	BEEP_COUNTER
EXTEND_FINISH
	RELAY_BYPASS_OFF
	RELAY_RETRACT_POSITION	    ; OFF
	clrf	RET_EXT_DURATION_cnt
	movlw	1
	movwf	MAIN_POINTER
	return
	
;---------------------------------------------------------------------
PRE_PROG_MODE				; MAIN_POINTER = 5
	btfsc	RTC_100ms
	incf	PROG_WINDOW_cnt,F
	btfsc	SW_RETRACT_AC_flag	; da li je detektirana promjena RETRACT prekidaca EXTEND=>RETRACT?
	goto	RETRACT_SWITCH_DETECTED
	movf	PROG_WINDOW_cnt,W
	sublw	PROG_WINDOW_PAR
	btfss	STATUS,Z
	return
	clrf	PROG_WINDOW_cnt
	clrf	SWITCH_RETRACT_cnt
	clrf	CONTACT_PAUSE_cnt
	movlw	1
	movwf	MAIN_POINTER
	return
RETRACT_SWITCH_DETECTED
	clrf	PROG_WINDOW_cnt
	incf	SWITCH_RETRACT_cnt,F
	movf	SWITCH_RETRACT_cnt,W
	sublw	SWITCH_RETRACT_PAR
	btfss	STATUS,Z
	return
	clrf	SWITCH_RETRACT_cnt
	clrf	PROG_WINDOW_cnt
	incf	MAIN_POINTER,F
	bsf	BUZZER_TU_TII_flag
	return
	
;---------------------------------------------------------------------
PROG_MODE_PRESENT_SETUP		; MAIN_POINTER = 6
	btfsc	RTC_100ms
	incf	PROG_WINDOW_cnt,F
	movlw	PROG_WINDOW_PAR
	subwf	PROG_WINDOW_cnt,W
	btfss	STATUS,Z
	return
	incf	DEVICE_MODE,W
	movwf	BEEP_COUNTER
	incf	MAIN_POINTER,F
	clrf	PROG_WINDOW_cnt
	return

;---------------------------------------------------------------------
PROG_MODE_PREAPARE_SETUP	; MAIN_POINTER = 7
	movf	BEEP_COUNTER,F
	btfss	STATUS,Z
	return
	bsf	BUZZER_TU_TII_flag
	incf	MAIN_POINTER,F
	return
	
;---------------------------------------------------------------------
PROG_MODE_SETUP		    ; MAIN_POINTER = 8
	btfsc	RTC_100ms
	incf	PROG_WINDOW_cnt,F
	btfsc	SW_RETRACT_AC_flag	; da li je detektirana promjena RETRACT prekidaca EXTEND=>RETRACT?
	goto	RETRACT_SWITCH_DETECTED_2
	movf	PROG_WINDOW_cnt,W
	sublw	PROG_WINDOW_PAR
	btfss	STATUS,Z
	return
	incf	MAIN_POINTER,F
	clrf	PROG_WINDOW_cnt
	return
RETRACT_SWITCH_DETECTED_2
	clrf	PROG_WINDOW_cnt
	incf	SWITCH_RETRACT_cnt,F
	return	
	
;---------------------------------------------------------------------
PROG_MODE_PRE_SAVE	   ; MAIN_POINTER = 9
	movf	SWITCH_RETRACT_cnt,W
	btfsc	STATUS,Z
	goto	CANCEL_PROG
	sublw	5		
	btfss	STATUS,C	; ako je izbrojano vise od 5 pulseva "SWITCH_RETRACT_cnt"
	goto	CANCEL_PROG	; tada otkazi programiranje
	decf	SWITCH_RETRACT_cnt,W
	movwf	DEVICE_MODE
	incf	MAIN_POINTER,F
	return
CANCEL_PROG
	bsf	BUZZER_TI_TUU_flag
	movlw	1
	movwf	MAIN_POINTER
	clrf	CONTACT_PAUSE_cnt
	clrf	SWITCH_RETRACT_cnt
	return
	
;---------------------------------------------------------------------
PROG_MODE_SAVE		; MAIN_POINTER = 10
		; /// Memory location Erease
	BCF	INTCON,GIE  ; Disable ints so required sequences will execute properly
	BANKSEL	PMADRL
	MOVLW	h'FF'	    ; Load lower 8 bits of erase address boundary
	MOVWF	PMADRL
	MOVLW	h'03'	    ; Load upper 6 bits of erase address boundary
	MOVWF	PMADRH
	BCF	PMCON1,CFGS ; Not configuration space
	BSF	PMCON1,FREE ; Specify an erase operation
	BSF	PMCON1,WREN ; Enable writes
	MOVLW	55h	    ; Start of required sequence to initiate erase
	MOVWF	PMCON2	    ; Write 55h
	MOVLW	0AAh 
	MOVWF	PMCON2	    ; Write AAh
	BSF	PMCON1,WR   ; Set WR bit to begin erase
	NOP	    ; NOP instructions are forced as processor starts
	NOP	    ; row erase of program memory.
		; The processor stalls until the erase process is complete
		; after erase processor continues with 3rd instruction
	BCF	PMCON1,WREN ; Disable writes
		; /// ERASE finished
		; /// Start to write in to Flash
	MOVLW	h'FF'	    ; Load lower 8 bits of erase address boundary
	MOVWF	PMADRL
	MOVLW	h'03'	    ; Load upper 6 bits of erase address boundary
	BCF	PMCON1,CFGS ; Not configuration space
	BSF	PMCON1,LWLO ; Only Load Write Latches
	BSF	PMCON1,WREN ; Enable writes
	movf	DEVICE_MODE,W
	movwf	PMDATL
	clrf	PMDATH
	BCF	PMCON1,LWLO ; Write Latches to Flash
	MOVLW	55h	    ; Start of required sequence to initiate erase
	MOVWF	PMCON2	    ; Write 55h
	MOVLW	0AAh 
	MOVWF	PMCON2	    ; Write AAh
	BSF	PMCON1,WR   ; Set WR bit to begin write
	NOP		    ; NOP instructions are forced as processor starts
	NOP		    ; to write data to Flash
	BCF	PMCON1,WREN ; Disable writes
	BSF	INTCON,GIE	    ; Enable interrupts.
	    ; /// Write finish
	BANK_0
	bsf	BUZZER_TU_TII_flag
	incf	MAIN_POINTER,F
	return
	
;---------------------------------------------------------------------
PROG_MODE_CHECK		; MAIN_POINTER = 11
	btfsc	BUZZER_TU_TII_flag
	return
	
	BANKSEL PMADRL	    ; Select Bank for PMCON registers
	MOVLW	h'FF'
	MOVWF	PMADRL	    ; Store LSB of address
	MOVLW	h'03'		    ; adresa 0x3FF je zadnji bajt flash memorije 1kB
	MOVWF	PMADRH	    ; Store MSB of address
	BCF	PMCON1,CFGS ; Do not select Configuration Space
	BSF	PMCON1,RD   ; Initiate read
	NOP	    ;Ignored 
	NOP	    ;Ignored 
	MOVF	PMDATL,W    ; Get LSB of word
	MOVWF	DEVICE_MODE ; Store in user location
	BANK_0
	
	movwf	BEEP_COUNTER
	incf	BEEP_COUNTER,F
	incf	MAIN_POINTER,F
	return	
	
;---------------------------------------------------------------------
PROG_MODE_CONFIRM		; MAIN_POINTER = 12
	movf	BEEP_COUNTER,F
	btfss	STATUS,Z
	return
	bsf	BUZZER_TU_TII_flag
	movlw	1
	movwf	MAIN_POINTER
	clrf	CONTACT_PAUSE_cnt
	return
	
	
;========================================================================
INPUT_HANDLER
	btfss	PWR_UP_flag
	return
	btfss	RTC_10ms
	return
			; debounce filter za detekciju Kontakta
	rlf	CONTACT_DEBOUNCE_filter,F
	bcf	CONTACT_DEBOUNCE_filter,0
	btfss	CONTACT_IN
	bsf	CONTACT_DEBOUNCE_filter,0
	movf	CONTACT_DEBOUNCE_filter,W
	btfsc	STATUS,Z
	bcf	CONTACT_ON_flag
	incf	CONTACT_DEBOUNCE_filter,W
	btfsc	STATUS,Z
	bsf	CONTACT_ON_flag		; detektiran KONTAKT signal 
		    	; debounce filter za detekciju LOCK signala
	BANKSEL	TRISA
	btfss	TRISA,2
	goto	CHECK_UNLOCK_INPUT ; LOCK ulaz je postavljen kao izlaz (buzzer), preskoci
	BANK_0	    ; LOCK ulaz gledaj samo ako je RA2 postavljen kao ulaz
	rlf	LOCK_DEBOUNCE_filter,F
	bcf	LOCK_DEBOUNCE_filter,0
	btfss	LOCK_IN
	bsf	LOCK_DEBOUNCE_filter,0
	movf	LOCK_DEBOUNCE_filter,W
	btfsc	STATUS,Z
	bcf	LOCK_ON_flag
	incf	LOCK_DEBOUNCE_filter,W
	btfsc	STATUS,Z
	bsf	LOCK_ON_flag		; detektiran LOCK signal (zakljucavanje)
	movf	LOCK_DEBOUNCE_filter,W
	sublw	B'01111111'		; detekcija prelaza LOCK signala OFF=>ON
	btfss	STATUS,Z
	goto	CHECK_UNLOCK_INPUT
	clrf	UNLOCK_cnt		; obrisi brojac suprotnog (UNLOCK) brojaca
	incf	LOCK_cnt,F		; ako je prijelaz detektiran uvecaj brojac
	btfsc	STATUS,Z
	decf	LOCK_cnt,F		; ne dozvoli prekoracenje brojaca preko 0xFF
		    	; debounce filter za detekciju UNLOCK signala	
CHECK_UNLOCK_INPUT
	BANK_0
	rlf	UNLOCK_DEBOUNCE_filter,F
	bcf	UNLOCK_DEBOUNCE_filter,0
	btfss	UNLOCK_IN
	bsf	UNLOCK_DEBOUNCE_filter,0
	movf	UNLOCK_DEBOUNCE_filter,W
	btfsc	STATUS,Z
	bcf	UNLOCK_ON_flag
	incf	UNLOCK_DEBOUNCE_filter,W
	btfsc	STATUS,Z
	bsf	UNLOCK_ON_flag		; detektiran UNLOCK signal (otkljucavanje)
	movf	UNLOCK_DEBOUNCE_filter,W
	sublw	B'01111111'		; detekcija prelaza UNLOCK signala OFF=>ON
	btfss	STATUS,Z
	goto	CHECK_RETRACT_SWITCH
	clrf	LOCK_cnt		; obrisi brojac suprotnog (LOCK) brojaca
	incf	UNLOCK_cnt,F		; ako je prijelaz detektiran uvecaj brojac
	btfsc	STATUS,Z
	decf	UNLOCK_cnt,F		; ne dozvoli prekoracenje brojaca preko 0xFF
		    	; debounce filter za detekciju polozaja prekidaca retrovizora u RETRACT	
CHECK_RETRACT_SWITCH	   ; debounce za RETRACT SWITCH je malo drugaèiji i jaèi nego za LOCK i UNLOCK		
	
	bcf	SW_RETRACT_AC_flag
	btfss	SW_RETRACT_IN
	goto	SW_IN_RETRACT_POSITION
	movf	RETRACT_DEBOUNCE_filter,F
	btfss	STATUS,Z
	decf	RETRACT_DEBOUNCE_filter,F
	btfsc	STATUS,Z
	bcf	SW_RETRACT_ON_flag	; prekidac retrovizora u EXTEND poziciji
	return
	
SW_IN_RETRACT_POSITION
	movlw	RETRACT_DEBOUNCE_PAR
	subwf	RETRACT_DEBOUNCE_filter,W
	btfsc	STATUS,Z
	goto	RETRACT_DEBOUNCE_PASS
	incf	RETRACT_DEBOUNCE_filter,F
	return
RETRACT_DEBOUNCE_PASS
	btfss	SW_RETRACT_ON_flag
	bsf	SW_RETRACT_AC_flag	; postavi alternate flag (prijelaz EXTEND=>RETRACT)
	bsf	SW_RETRACT_ON_flag	; prekidac retrovizora u RETRACT poziciji
	return
	
;========================================================================	
BUZZER_HANDLER
	btfss	RTC_10ms
	return
	movf	BEEP_COUNTER,F
	btfss	STATUS,Z
	goto	SOUND_N_BEEPS
	btfsc	BUZZER_TU_TII_flag
	goto	SOUND_2_TONE	
	btfsc	BUZZER_TI_TUU_flag
	goto	SOUND_2_TONE	
	return
;------------------------------------------------------------------------	
SOUND_N_BEEPS
	movlw	HIGH	BEEP_STATE
	movwf	PCLATH
	movf	BUZZER_INDEX,W
	goto	BEEP_STATE
;------------------------------------------------------------------------	
BEEP_INIT		    ; BUZZER_INDEX=0
	movlw	LOW_TONE_PAR
	CALL	INIT_PWM
	movlw	SHORT_BEEP_TIMER_PAR
	movwf	BEEP_TONE_TIMER_cnt
	incf	BUZZER_INDEX,F
	movf	BEEP_COUNTER,W
	movwf	BEEP_cnt
	return
;------------------------------------------------------------------------	
BEEP_ONGOING		    ; BUZZER_INDEX=1
	decfsz	BEEP_TONE_TIMER_cnt
	return
	CALL	STOP_PWM
	movlw	BEEP_PAUSE_TIMER_PAR
	movwf	BEEP_TONE_TIMER_cnt
	incf	BUZZER_INDEX,F
	return
;------------------------------------------------------------------------	
BEEP_SILENCE		    ; BUZZER_INDEX=2
	decfsz	BEEP_TONE_TIMER_cnt,F
	return
	decfsz	BEEP_cnt,F
	goto	BEEP_NEXT_CYCLE
	movlw	10
	movwf	BEEP_TONE_TIMER_cnt
	incf	BUZZER_INDEX,F
	return
BEEP_NEXT_CYCLE
	decf	BUZZER_INDEX,F
	movlw	LOW_TONE_PAR
	CALL	INIT_PWM
	movlw	SHORT_BEEP_TIMER_PAR
	movwf	BEEP_TONE_TIMER_cnt
	return
;------------------------------------------------------------------------	
BEEP_FINISH		    ; BUZZER_INDEX=3
	decfsz	BEEP_TONE_TIMER_cnt,F
	return
	CALL	DEINIT_PWM
	clrf	BUZZER_INDEX
	clrf	BEEP_COUNTER	    ; signalizira da je bipanje završilo
	return
	
;------------------------------------------------------------------------
SOUND_2_TONE		
	movlw	HIGH	BEEP_STATE
	movwf	PCLATH
	movf	BUZZER_INDEX,W
	goto	TWO_TONE_STATE

;------------------------------------------------------------------------
INIT_1st_TONE			; BUZZER_INDEX=0
	movlw	LOW_TONE_PAR
	btfss	BUZZER_TU_TII_flag
	movlw	HIGH_TONE_PAR
	CALL	INIT_PWM
	movlw	SHORT_BEEP_TIMER_PAR
	movwf	BEEP_TONE_TIMER_cnt
	incf	BUZZER_INDEX,F
	return
;------------------------------------------------------------------------
INIT_2nd_TONE			 ; BUZZER_INDEX=1
	decfsz	BEEP_TONE_TIMER_cnt,F
	return
	movlw	HIGH_TONE_PAR
	btfss	BUZZER_TU_TII_flag
	movlw	LOW_TONE_PAR
	CALL	SET_PWM_FREQUENCY
	incf	BUZZER_INDEX,F
	movlw	LONG_BEEP_TIMER_PAR
	movwf	BEEP_TONE_TIMER_cnt
	return
;------------------------------------------------------------------------
FINISH_TONE			 ; BUZZER_INDEX=2
	decfsz	BEEP_TONE_TIMER_cnt,F
	return
	CALL	STOP_PWM
	movlw	BEEP_PAUSE_TIMER_PAR
	movwf	BEEP_TONE_TIMER_cnt
	incf	BUZZER_INDEX,F
	return
;------------------------------------------------------------------------
FINISH_PAUSE			 ; BUZZER_INDEX=3
	decfsz	BEEP_TONE_TIMER_cnt,F
	return
	CALL	DEINIT_PWM
	clrf	BUZZER_INDEX
	btfsc	BUZZER_TU_TII_flag
	bcf	BUZZER_TU_TII_flag
	btfsc	BUZZER_TI_TUU_flag
	bcf	BUZZER_TI_TUU_flag
	return
	

	
;========================================================================
INIT_PWM		; frekvencija donesena u W registru	
	CALL	SET_PWM_FREQUENCY
	clrf	TMR2
	BANKSEL	T2CON
	movlw	B'00000101'
	movwf	T2CON	    ; postscale 1:1, T2=ON, Prescale 1:4
	BANKSEL	PIR1
	bcf	PIR1,TMR2IF
	
	BANKSEL	IOCAP
	bcf	IOCAP,2	    ; onemoguæi unterrupt na rastuci brid na PORTA 2 pinu 
	bcf	IOCAN,2	    ; onemoguæi unterrupt na padajuci brid na PORTA 2 pinu
	
	BANKSEL	TRISA
	bcf	TRISA,2	    ; postavi RA2 kao izlaz
	BANKSEL	PWM1CON
	movlw	B'11000000'
	movwf	PWM1CON	    ; PWM module ON, PWM pin enabled, PWM output active high
	BANK_0
	return
	
;========================================================================
SET_PWM_FREQUENCY
	movwf	temp
	BANKSEL	PR2
	movwf	PR2
	bcf	STATUS,C
	rrf	temp,W	    ; podijeli sa 2
	BANKSEL	PWM1DCH
	movwf	PWM1DCH	    ; duty cycle = 50%  => PR2/2
	clrf	PWM1DCL
	BANK_0
	return
	
;========================================================================
STOP_PWM
	BANKSEL	PWM1CON
	movlw	B'01000000' ; PWM module OFF
	movwf	PWM1CON
	BANK_0
	return
	
;========================================================================
DEINIT_PWM
	BANKSEL	TRISA
	bsf	TRISA,2	    ; postavi RA2 kao ulaz
	BANKSEL	PWM1CON
	movlw	B'00000000'
	movwf	PWM1CON	    ; PWM module OFF, PWM pin disabled
	BANKSEL	T2CON
	bcf	T2CON,TMR2ON	; zaustavi TMR2
	BANK_0
	return
	
;========================================================================	
WATCHDOG_MONITOR    ; rutina koja prati LOCK i UNLOCK countere, i ako 
		    ; dugo vremena stoje u bilo kojem stanju osim nule
		    ; resetira ih na nulu. Brine se da brojaci ne ostanu predugo
		    ; u nekom drugom stanju
	btfss	RTC_100ms
	return
	movf	LOCK_cnt,F
	btfss	STATUS,Z
	goto	MONITOR_ON
	movf	UNLOCK_cnt,F
	btfss	STATUS,Z
	goto	MONITOR_ON
	clrf	AUTORESET_cnt
	return
MONITOR_ON
	btfss	RTC_1s
	return
	incf	AUTORESET_cnt,F
	movf	AUTORESET_cnt,W
	sublw	AUTORESET_PAR
	btfss	STATUS,Z
	return
	clrf	LOCK_cnt
	clrf	UNLOCK_cnt
	clrf	SLEEP_cnt
	return
		    
		    
;========================================================================
	ORG	h'2F0'		; lookup tabele

MAIN_STATE
	BRW
	goto	POWER_UP
	goto	CONTACT_ON
	goto	STAND_BY
	goto	MIRROR_RETRACT
	goto	MIRROR_EXTEND
	goto	PRE_PROG_MODE
	goto	PROG_MODE_PRESENT_SETUP	
	goto	PROG_MODE_PREAPARE_SETUP
	goto	PROG_MODE_SETUP	
	goto	PROG_MODE_PRE_SAVE
	goto	PROG_MODE_SAVE	
	goto	PROG_MODE_CHECK	
	goto	PROG_MODE_CONFIRM
	
BEEP_STATE
	BRW
	goto	BEEP_INIT
	goto	BEEP_ONGOING
	goto	BEEP_SILENCE
	goto	BEEP_FINISH


TWO_TONE_STATE
	BRW
	goto	INIT_1st_TONE	
	goto	INIT_2nd_TONE
	goto	FINISH_TONE
	goto	FINISH_PAUSE	

	
	ORG	h'3FF'
	dt 0		; HEF lokacija za varijablu DEVICE_MODE, inicijalna vrijednost
			; HEF = umjesto EEPROM-a kojeg 12F1501 nema
			
	end