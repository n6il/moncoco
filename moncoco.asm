*  6809/6309 Debug monitor for use with NOICE09 6809/6309 versions on Color Computer
*
*  Copyright (c) 1992-2006 by John Hartman
*
*  Modification History:
*     18-Feb-06 JLH ported from 6809 version
*
*     27-Feb-06 Robert Gault Converted program to work with the Tandy
*               Color Computer (Coco) with either a 6809 or 6309 CPU.
*               For complete compatibility, a Coco1 or Coco2 must be placed
*               in full RAM mode. If not, NoICE can't Step through any code
*               in the region of $8000-$FFFF (ROM).
*               In short, this monitor can be used with NoICE09 and
*               NoICE6309 without recompiling.
*============================================================================
*
*  To customize for a given target, you must change code in the
*  hardware equates. The string TSTG must not be changed.
*
*  This file was assembled with the assembler Mamou from the NitrOS-9 project.
*  It should assemble with the Motorola Freeware assembler available from the
*  Motorola Freeware BBS and elsewhere. It should assemble with the Tandy Disk
*  EDTASM assembler after the lables are shortened to 6 or less characters.
*
*  This file may also be assembled with the Dunfield assembler
*
*
*============================================================================
*  RG
*  The Tandy Color Computer (Coco) boots into ROM code and the hardware
*  vectors are also in ROM. That has required significant changes to the
*  original code. The UART available for the Coco is based on the 6551 and
*  will be installed in a peripheral device MultiPack Interface (MPI) which
*  requires slot selection. The code has been made position relative so that
*  it can be loaded anywhere in RAM memory without recompiling. The monitor
*  tests for the type of CPU. If a 6309 is found, initially regMD content is
*  assumed to be $00. Changing the 4th byte in this program will change this
*  default without the need for recompiling.
*============================================================================
*  HARDWARE PLATFORM CUSTOMIZATIONS
*
* The next two lines are needed to compile, but the program may be loaded
* anywhere there is 1088 bytes of RAM.
DATA_START       EQU     $E00           START OF MONITOR DATA
CODE_START       EQU     $F00           START OF MONITOR CODE
* Do not change the next line.
HARD_VECT        EQU     $100           START OF HARDWARE VECTORS
MPI              EQU     $FF7F          Multi-Pack Interface
* Change the next line to match the location of your RS-232 pack.
SLOT             EQU     1              0=SLOT1, 1=SLOT2, 2=SLOT3, 3=SLOT4

*============================================================================
*  Equates for memory mapped 6551 serial port in the Tandy RS-232 pack.
*
U6551   equ     $FF68             base of 16450 UART
RXR     equ     0                 Receiver buffer register
TXR     equ     0                 Transmitter buffer register
STAT    EQU     1                 Status register
RST     EQU     1                 Reset register
CMD     EQU     2                 Command register
CTRL    EQU     3                 Control register
*
*  Define monitor serial port
SER_STATUS      EQU     U6551+STAT
SER_RXDATA      EQU     U6551+RXR
SER_TXDATA      EQU     U6551+TXR
SER_RESET       EQU     U6551+RST
SER_COMMAND     EQU     U6551+CMD
SER_CONTROL     EQU     U6551+CTRL
RXRDY           EQU     $08              BIT MASK FOR RX BUFFER FULL
TXRDY           EQU     $10              BIT MASK FOR TX BUFFER EMPTY
*
*
*  Condition code bits
C       EQU     1
I       EQU     $10
F       EQU     $40
E       EQU     $80
*
*============================================================================
*  RAM definitions:
        ORG     DATA_START
*
NVEC            EQU     3               number of vectors not in use
RAMVEC          LBRA    INT_ENTRY
* This byte can be changed in the compiled file to select modes.
* bit0: 0=emulate 1=native; bit1 0=normal FIRQ 1= FIRQ same as IRQ
MDDEF           FCB     0               default=emulate,normalFIRQ 
*  Initial user stack
*  (Size and location is user option)
                RMB     64
INITSTACK
*
*  Monitor stack
*  (Calculated use is at most 7 bytes.  Leave plenty of spare)
                RMB     16
MONSTACK
*
STACK_IMG       RMB     2
CC_IMG          RMB     1
CPU             RMB     1
MPI_IMG         RMB     1
RETURN          RTS
*
*  Target registers:  DO NOT CHANGE!
TASK_REGS
REG_STATE       RMB     1
REG_PAGE        RMB     1
REG_SP          RMB     2
REG_U           RMB     2
REG_Y           RMB     2
REG_X           RMB     2
REG_F           RMB     1               F BEFORE E, SO W IS LEAST SIG. FIRST
REG_E           RMB     1
REG_B           RMB     1               B BEFORE A, SO D IS LEAST SIG. FIRST
REG_A           RMB     1
REG_DP          RMB     1
REG_CC          RMB     1
REG_MD          RMB     1
REG_V           RMB     2
REG_PC          RMB     2
TASK_REG_SZ     EQU     *-TASK_REGS
*
*  Communications buffer
*  (Must be at least as long as TASK_REG_SZ.  At least 19 bytes recommended.
*  Larger values may improve speed of NoICE memory move commands.)
*  Buffer can not be made larger than 255 bytes without changing code.
COMBUF_SIZE     EQU     128             DATA SIZE FOR COMM BUFFER
COMBUF          RMB     2+COMBUF_SIZE+1 BUFFER ALSO HAS FN, LEN, AND CHECK
*
RAM_END         EQU     *               ADDRESS OF TOP+1 OF DATA
*
*===========================================================================
        ORG     CODE_START
*
*  Monitor initialization after a LOADM and EXEC on the Coco
RESET
*
*  Set CPU mode to safe state
        PSHS    CC,A                SAVE regs A & CC
        ORCC    #I+F                INTERRUPTS OFF
        LDA     MPI
        STA     MPI_IMG,PCR
        PULS    A                   RECOVER regCC
        STA     CC_IMG,PCR
        PULS    A                   RECOVER regA
        PSHS    PC                  THIS JUST IS A PLACE MARKER
        PSHS    U
        PSHS    Y 
        PSHS    X 
        PSHS    DP
        TFR     D,X
        LDB     #$38            ASCII 8
        STB     B1+1,PCR        SET MESSAGE TO 6809
        LDB     #5              PROCESSOR TYPE 6809
        CLRD
        STB     CPU,PCR
        BNE     I1
        LDA     MDDEF,PCR
        BITA    #1
        BEQ     I0
        PSHSW
I0      LDB     #$33            ASCII 3
        STB     B1+1,PCR        SET MESSAGE TO 6309
        LDB     #17             PROCESSOR TYPE 17   = 6309
I1      STB     TSTG,PCR        Set PROCESSOR TYPE
        TFR     X,D             RECOVER regD
        PSHS    D
        LDA     CC_IMG,PCR
        PSHS    A 
        LEAX    RETURN,PCR
        TST     CPU,PCR
        BNE     I2
        LDA     MDDEF,PCR
        BITA    #1
        BEQ     I2
        TFR     X,D
        STD     12,S                POINT PC TO RTS
        BRA     I3
I2      TFR     X,D
        STD     10,S
I3      STS     STACK_IMG,PCR
        LEAS    MONSTACK,PCR
*
*----------------------------------------------------------------------------
        LBSR    INIT_UART
*----------------------------------------------------------------------------
*
*  Initialize RAM interrupt vectors
        LDY     #HARD_VECT 
        LEAX    VECTORS,PCR
        LDB     #NVEC                  
RES20   LDA     ,X+
        STA     ,Y+
        LDU     ,X++
        STU     ,Y++
        DECB
        BNE     RES20

*
*  Initialize user registers
         LDS     STACK_IMG,PCR            RECOVER USER STACK
         CLRA                             INDICATE A RESET
         LBRA    INT_ENTRY                ENTER MONITOR
*
*===========================================================================
*  Get a character to A
*
*  Return A=char, CY=0 if data received
*         A=error, CY=1 if timeout (0.5 seconds)
*
*  Uses 6 bytes of stack including return address
*
GETCHAR PSHS    X
        LDX     #0
GC10    LEAX    -1,X
        BEQ     GC90            EXIT IF TIMEOUT
        LDA     SER_STATUS      READ DEVICE STATUS
        BITA    #RXRDY
        BEQ     GC10            NOT READY YET.
*
*  Data received:  return CY=0. data in A
        CLRA                    CY=0
        LDA     SER_RXDATA      READ DATA
        PULS    X,PC
*
*  Timeout:  return CY=1
GC90    ORCC    #C              CY=1
        PULS    X,PC
*
*===========================================================================
*  Output character in A
*
*  Uses 5 bytes of stack including return address
*
PUTCHAR PSHS    A 
PC10    LDA     SER_STATUS      CHECK TX STATUS
        ANDA    #TXRDY          RX READY ?
        BEQ     PC10
        PULS    A
        STA     SER_TXDATA      TRANSMIT CHAR.
        RTS
*
*======================================================================
*  Response string for GET TARGET STATUS request
*  Reply describes target:
TSTG    FCB     17                      2: PROCESSOR TYPE = 6309
        FCB     COMBUF_SIZE             3: SIZE OF COMMUNICATIONS BUFFER
* The next two lines are not used with the Coco. The code could be rewritten
* for the Coco3 using the Memory Management Unit (MMU).
        FCB     0                       4: NO TASKING SUPPORT
        FDB     0,0                     5-8: LOW/HIGH LIMIT OF MAPPED MEM (NONE)
        FCB     B1-B0                   9 BREAKPOINT INSTR LENGTH
B0      SWI                             10+ BREKAPOINT INSTRUCTION
B1      FCC     '6309 monitor V2.0'     DESCRIPTION, ZERO
        FCB     0
TSTG_SIZE       EQU     *-TSTG          SIZE OF STRING
*
*======================================================================
*  HARDWARE PLATFORM INDEPENDENT EQUATES AND CODE
*
*  Communications function codes.
FN_GET_STAT     EQU     $FF    reply with device info
FN_READ_MEM     EQU     $FE    reply with data
FN_WRITE_M      EQU     $FD    reply with status (+/-)
FN_READ_RG      EQU     $FC    reply with registers
FN_WRITE_RG     EQU     $FB    reply with status
FN_RUN_TARG     EQU     $FA    reply (delayed) with registers
FN_SET_BYTE     EQU     $F9    reply with data (truncate if error)
FN_IN           EQU     $F8    input from port
FN_OUT          EQU     $F7    output to port
*
FN_MIN          EQU     $F7    MINIMUM RECOGNIZED FUNCTION CODE
FN_ERROR        EQU     $F0    error reply to unknown op-code
*
*===========================================================================
*  Common handler for default interrupt handlers
*  Enter with A=interrupt code = processor state
*  All registers stacked, PC=next instruction
INT_ENTRY 
        STA     REG_STATE,PCR       SAVE STATE
*
*  Save registers from stack to reg block for return to master
*  Host wants least significant bytes first, so flip as necessary
        PULS    A
        STA     REG_CC,PCR          CONDITION CODES
        PULS    D
        EXG     A,B
        STD     REG_B,PCR           D 
        TST     CPU                 6809 or 6309?
        BNE     IE_1
        LDA     MDDEF,PCR
        BITA    #1                   emulation or native mode?
        BEQ     IE_5
        PULSW
IE_5    EXG     E,F
        STW     REG_F,PCR
*  V isn't on the stack, but we haven't touched it.  Copy to RAM
        TFR     V,D
        EXG     A,B
        STD     REG_V,PCR
*  There seems is no way to store MD, and no way to load it except immediate
*  Thus we have to construct it by BITMD
        LDA     MDDEF,PCR                  get bits 0&1
        BITMD   #$40                       read bit 6
        BEQ     IE_2
        ORA     #$40
IE_2    BITMD   #$80                       read bit 7
        BEQ     IE_3
        ORA     #$80
IE_3    STA     REG_MD,PCR
IE_1    PULS    A
        STA     REG_DP,PCR          DP
        PULS    D
        EXG     A,B
        STD     REG_X,PCR           X 
        PULS    D
        EXG     A,B
        STD     REG_Y,PCR           Y 
        PULS    D
        EXG     A,B
        STD     REG_U,PCR           U 
*
*  If this is a breakpoint (state = 1), then back up PC to point at SWI
        PULS    X               PC AFTER INTERRUPT
        LDA     REG_STATE,PCR
        CMPA    #1
        BNE     NOTBP           BR IF NOT A BREAKPOINT
        LEAX    -1,X            ELSE BACK UP TO POINT AT SWI LOCATION
NOTBP   TFR     X,D             TRANSFER PC TO D
        EXG     A,B
        STD     REG_PC,PCR      PC 
        LBRA    ENTER_MON       REG_PC POINTS AT POST-INTERRUPT OPCODE
*
*===========================================================================
*  Main loop  wait for command frame from master
*
*  Uses 6 bytes of stack including return address
*
MAIN    LBSR    INIT_UART
*
        LEAS     MONSTACK,PCR               CLEAN STACK IS HAPPY STACK
MAIN2   LEAX     COMBUF,PCR                 BUILD MESSAGE HERE 
*
*  First byte is a function code
        LBSR    GETCHAR                 GET A FUNCTION (6 bytes of stack)
        BCS     MAIN3                   JIF TIMEOUT: RESYNC
        CMPA    #FN_MIN
        BLO     MAIN2                    JIF BELOW MIN: ILLEGAL FUNCTION
        STA     ,X+                     SAVE FUNCTION CODE
*
*  Second byte is data byte count (may be zero)
        LBSR    GETCHAR                 GET A LENGTH BYTE
        BCS     MAIN3                   JIF TIMEOUT: RESYNC
        CMPA    #COMBUF_SIZE
        BHI     MAIN2                   JIF TOO LONG: ILLEGAL LENGTH
        STA     ,X+                     SAVE LENGTH
        CMPA    #0
        BEQ     MA80                    SKIP DATA LOOP IF LENGTH = 0
*
*  Loop for data
        TFR     A,B                     SAVE LENGTH FOR LOOP
MA10    LBSR    GETCHAR                 GET A DATA BYTE
        BCS     MAIN3                   JIF TIMEOUT: RESYNC
        STA     ,X+                     SAVE DATA BYTE
        DECB
        BNE     MA10
*
*  Get the checksum
MA80    LBSR    GETCHAR                 GET THE CHECKSUM
        BCS     MAIN3                   JIF TIMEOUT: RESYNC
        PSHS    A                       SAVE CHECKSUM
*
*  Compare received checksum to that calculated on received buffer
*  (Sum should be 0)
        LBSR     CHECKSUM
        ADDA    ,S+                     ADD SAVED CHECKSUM TO COMPUTED
        BNE     MAIN3                   JIF BAD CHECKSUM
*
*  Process the message.
        LEAX    COMBUF,PCR
        LDA     ,X+                     GET THE FUNCTION CODE
        LDB     ,X+                     GET THE LENGTH
        CMPA    #FN_GET_STAT
        LBEQ    TARGET_STAT
        CMPA    #FN_READ_MEM
        LBEQ    READ_MEM
        CMPA    #FN_WRITE_M
        LBEQ    WRITE_MEM
        CMPA    #FN_READ_RG
        LBEQ    READ_REGS
        CMPA    #FN_WRITE_RG
        LBEQ    WRITE_REGS
        CMPA    #FN_RUN_TARG
        LBEQ    RUN_TARGET
        CMPA    #FN_SET_BYTE
        LBEQ    SET_BYTES
        CMPA    #FN_IN
        LBEQ    IN_PORT
        CMPA    #FN_OUT
        LBEQ    OUT_PORT
*
*  Error: unknown function.  Complain
        LDA     #FN_ERROR
        STA     COMBUF,PCR          SET FUNCTION AS "ERROR"
        LDA     #1
        LBRA     SEND_STATUS     VALUE IS "ERROR"
*
MAIN3   ANDA    #7               KEEP ONLY Overun Framing Parity errors
        LBNE    MAIN             Initialize UART and restart
        LBRA    MAIN2            Just restart
*===========================================================================
*
*  Target Status:  FN, len
*
*  Entry with A=function code, B=data size, X=COMBUF+2
*
TARGET_STAT  
        LEAX    TSTG,PCR                   DATA FOR REPLY
        LEAY    COMBUF+1,PCR               POINTER TO RETURN BUFFER
        LDB     #TSTG_SIZE              LENGTH OF REPLY
        STB     ,Y+                     SET SIZE IN REPLY BUFFER
TS10    LDA     ,X+                     MOVE REPLY DATA TO BUFFER
        STA     ,Y+
        DECB
        BNE     TS10
*
*  Compute checksum on buffer, and send to master, then return
        LBRA     SEND

*===========================================================================
*
*  Read Memory:  FN, len, page, Alo, Ahi, Nbytes
*
*  Entry with A=function code, B=data size, X=COMBUF+2
*
READ_MEM 
*  Get address
        LDD     1,X
        EXG     A,B 
        TFR     D,Y                     ADDRESS IN Y
*
*  Prepare return buffer: FN (unchanged), LEN, DATA
        LDB     3,X                     NUMBER OF BYTES TO RETURN
        STB     COMBUF+1,PCR                RETURN LENGTH = REQUESTED DATA
        BEQ     GLP90                   JIF NO BYTES TO GET
*
*  Read the requested bytes from local memory
GLP     LDA     ,Y+                     GET BYTE
        STA     ,X+                     STORE TO RETURN BUFFER
        DECB
        BNE     GLP
*
*  Compute checksum on buffer, and send to master, then return
GLP90   LBRA     SEND

*===========================================================================
*
*  Write Memory:  FN, len, page, Alo, Ahi, (len-3 bytes of Data)
*
*  Entry with A=function code, B=data size, X=COMBUF+2
*
*  Uses 6 bytes of stack
*
WRITE_MEM 
* Skip MAP
        LEAX    1,X
*  Get address
        LDD     ,X++
        EXG     A,B 
        TFR     D,Y                     ADDRESS IN Y
*
*  Compute number of bytes to write
        LDB     COMBUF+1,PCR            NUMBER OF BYTES TO RETURN
        SUBB    #3                      MINUS PAGE AND ADDRESS
        BEQ     WLP50                   JIF NO BYTES TO PUT
*
*  Write the specified bytes to local memory
        PSHS    B,X,Y
WLP     LDA     ,X+                     GET BYTE TO WRITE
        STA     ,Y+                     STORE THE BYTE AT ,Y
        DECB
        BNE     WLP
*
*  Compare to see if the write worked
        PULS    B,X,Y
WLP20   LDA     ,X+                     GET BYTE JUST WRITTEN
        CMPA    ,Y+
        BNE     WLP80                   BR IF WRITE FAILED
        DECB
        BNE     WLP20
*
*  Write succeeded:  return status = 0
WLP50   LDA     #0                      RETURN STATUS = 0
        BRA     WLP90
*
*  Write failed:  return status = 1
WLP80   LDA     #1

*  Return OK status
WLP90   LBRA     SEND_STATUS

*===========================================================================
*
*  Read registers:  FN, len=0
*
*  Entry with A=function code, B=data size, X=COMBUF+2
*
READ_REGS 
*
*  Enter here from SWI after "RUN" and "STEP" to return task registers
RETURN_REGS
        LEAY    TASK_REGS,PCR           POINTER TO REGISTERS
        LEAX    COMBUF+1,PCR            POINTER TO RETURN BUFFER 
        LDB     #TASK_REG_SZ            NUMBER OF BYTES
        STB     ,X+                     SAVE RETURN DATA LENGTH
        TST     CPU,PCR                 6809 or 6309?
        BEQ     RERG2
        SUBD    #5                      FEWER REGISTERS WITH A 6809
        STB     -1,X
        LDB     #REG_F-TASK_REGS
        BSR     GRLP
        LDB     #REG_MD-REG_B
        LEAY    2,Y
        BSR     GRLP
        LDB     #REG_PC-REG_V
        LEAY    3,Y
RERG2   BSR     GRLP
*  Compute checksum on buffer, and send to master, then return
        LBRA     SEND
*  Copy the registers
GRLP    LDA     ,Y+                     GET BYTE TO A
        STA     ,X+                     STORE TO RETURN BUFFER
        DECB
        BNE     GRLP
        RTS
*
*===========================================================================
*
*  Write registers:  FN, len, (register image)
*
*  Entry with A=function code, B=data size, X=COMBUF+2
*
WRITE_REGS  
        TSTB                            NUMBER OF BYTES
        BEQ     WRR80                   JIF NO REGISTERS
        LEAY    TASK_REGS,PCR           POINTER TO REGISTERS
        TST     CPU,PCR                 6809 or 6309?
        BEQ     WRRE2
        LDB     #REG_F-TASK_REGS
*
*  Copy the registers
        BSR     WRRLP
        LDB     #REG_MD-REG_B
        LEAY    2,Y
        BSR     WRRLP
        LDB     #REG_PC-REG_V
        LEAY    3,Y
WRRE2   BSR     WRRLP
        BRA     WRR80
WRRLP   LDA     ,X+                     GET BYTE TO A
        STA     ,Y+                     STORE TO REGISTER RAM
        DECB
        BNE     WRRLP
        RTS
*
*  Return OK status
WRR80   CLRA
        LBRA     SEND_STATUS

*===========================================================================
*
*  Run Target:  FN, len
*
*  Entry with A=function code, B=data size, X=COMBUF+2
*
RUN_TARGET 
*  Switch to user stack
        LDD     REG_SP,PCR              BACK TO USER STACK
        EXG     A,B 
        TFR     D,S                     TO S
        TST     CPU,PCR
        BNE     RT1
*
*  Check MD, as it affects stack building and RTI
*  It's time for some self-modifying code!  Build LDMD #xxx, RTS in RAM and
*  call it. Place code where it won't cause problems at middle of buffer.
        LDD     #$113D                  LDMD #imm
        STD     COMBUF+30,PCR           Start code string
        LDA     REG_MD,PCR
        ANDA    #3                      Keep only bits 0&1
        STA     MDDEF,PCR               Update the default
        STA     COMBUF+32,PCR           add to code string
        LDA     #$39                    RTS add to code string
        STA     COMBUF+33,PCR
        LBSR    COMBUF+30               Run the code string
*
*  Restore V, which isn't on the stack
        LDD     REG_V,PCR
        EXG     A,B
        TFR     D,V
*
*  Restore registers
RT1     LDD     REG_PC,PCR              USER PC FOR RTI
        EXG     A,B 
        PSHS    D
*
        LDD     REG_U,PCR
        EXG     A,B 
        PSHS    D
*
        LDD     REG_Y,PCR
        EXG     A,B 
        PSHS    D
*
        LDD     REG_X,PCR
        EXG     A,B 
        PSHS    D
*
        LDA     REG_DP
        PSHS    A
*
        TST     CPU,PCR               6809 or 6309?
        BNE     RT2                   go if 6809
        LDW     REG_F,PCR             regW must be updated whether stacked
        EXG     E,F                   or not
        LDA     MDDEF,PCR
        BITA    #1                    emulation or native mode?
        BEQ     RT2                   jump if 6809 or emulation mode
*else push W on stack for RTI 
        PSHSW 
RT2     LDD     REG_B,PCR
        EXG     A,B
        PSHS    D 
*
        LDA     REG_CC,PCR               SAVE USER CONDITION CODES FOR RTI
        ORA     #E                      _MUST_ BE "ALL REGS PUSHED"
        PSHS    A
*
        LDA     MPI_IMG,PCR
        STA     MPI                      RESTORE MPI INFO
*  Return to user (conditioned by MD.0)
        RTI
*
*===========================================================================
*
*  Common continue point for all monitor entrances
*  SP = user stack
ENTER_MON 
        TFR     S,D             USER STACK POINTER
        EXG     A,B
        STD     REG_SP,PCR
*
*  Change to our own stack
        LEAS     MONSTACK,PCR   AND USE OURS INSTEAD
*
*  Operating system variables
        LDA     #0              ... OR ZERO IF UNMAPPED TARGET
        STA     REG_PAGE,PCR        SAVE USER'S PAGE
*
*  Return registers to master
        LBRA     RETURN_REGS

*===========================================================================
*
*  Set target byte(s):  FN, len { (page, alow, ahigh, data), (...)... }
*
*  Entry with A=function code, B=data size, X=COMBUF+2
*
*  Return has FN, len, (data from memory locations)
*
*  If error in insert (memory not writable), abort to return short data
*
*  This function is used primarily to set and clear breakpoints
*
*  Uses 1 byte of stack
*
SET_BYTES 
        LEAU    COMBUF+1,PCR            POINTER TO RETURN BUFFER
        CLRA
        STA     ,U+                     SET RETURN COUNT AS ZERO
        LSRB
        LSRB                            LEN/4 = NUMBER OF BYTES TO SET
        BEQ     SB99                    JIF NO BYTES (COMBUF+1 = 0)
*
*  Loop on inserting bytes
SB10    PSHS    B                       SAVE LOOP COUNTER
*
*
*  Get address
        LDD     1,X 
        EXG     A,B 
        TFR     D,Y                     MEMORY ADDRESS IN Y
*
*  Read current data at byte location
        LDA     ,Y
*
*  Insert new data at byte location
        LDB     3,X                     GET BYTE TO STORE
        STB     ,Y                      WRITE TARGET MEMORY
*
*  Verify write
        CMPB    ,Y                      READ TARGET MEMORY
        PULS    B                       RESTORE LOOP COUNT, CC'S INTACT
        BNE     SB90                    BR IF INSERT FAILED: ABORT
*
*  Save target byte in return buffer
        STA     ,U+
        INC     COMBUF+1,PCR            COUNT ONE RETURN BYTE
*
*  Loop for next byte
        LEAX    4,X                     STEP TO NEXT BYTE SPECIFIER
        CMPB    COMBUF+1,PCR
        BNE     SB10                    *LOOP FOR ALL BYTES
*
*  Return buffer with data from byte locations
SB90 
*
*  Compute checksum on buffer, and send to master, then return
SB99    LBRA     SEND

*===========================================================================
*
*  Input from port:  FN, len, PortAddressLo, PAhi (=0)
*
*  While the 6809 has no input or output instructions, we retain these
*  to allow write-without-verify
*
*  Entry with A=function code, B=data size, X=COMBUF+2
*
IN_PORT 
*
*  Get port address
        LDD     ,X
        EXG     A,B
        TFR     D,Y                     MEMORY ADDRESS IN Y
*
*  Read the requested byte from local memory
        LDA     ,Y
*
*  Return byte read as "status"
        LBRA     SEND_STATUS

*===========================================================================
*
*  Output to port  FN, len, PortAddressLo, PAhi (=0), data
*
*  Entry with A=function code, B=data size, X=COMBUF+2
*
OUT_PORT 
*
*  Get port address
        LDD     ,X
        EXG     A,B 
        TFR     D,Y                     MEMORY ADDRESS IN Y
*
*  Get data
        LDA     2,X
*
*  Write value to port
        STA     ,Y
*
*  Do not read port to verify (some I/O devices don't like it)
*
*  Return status of OK
        CLRA
        LBRA     SEND_STATUS

*===========================================================================
*  Build status return with value from "A"
*
SEND_STATUS 
        STA     COMBUF+2,PCR                SET STATUS
        LDA     #1
        STA     COMBUF+1,PCR                SET LENGTH
        BRA     SEND

*===========================================================================
*  Append checksum to COMBUF and send to master
*
SEND    LBSR     CHECKSUM              GET A=CHECKSUM, X->checksum location
        NEGA
        STA     ,X                     STORE NEGATIVE OF CHECKSUM
*
*  Send buffer to master
        LEAX    COMBUF,PCR              POINTER TO DATA
        LDB     1,X                     LENGTH OF DATA
        ADDB    #3                      PLUS FUNCTION, LENGTH, CHECKSUM
        LBSR    INIT_UART
SND10   LDA     ,X+
        LBSR     PUTCHAR                SEND A BYTE
        DECB
        BNE      SND10
        LBRA     MAIN                   BACK TO MAIN LOOP

*===========================================================================
*  Compute checksum on COMBUF.  COMBUF+1 has length of data,
*  Also include function byte and length byte
*
*  Returns:
*       A = checksum
*       X = pointer to next byte in buffer (checksum location)
*       B is scratched
*
CHECKSUM  
        LEAX    COMBUF,PCR              pointer to buffer
        LDB     1,X                     length of message
        ADDB    #2                      plus function, length
        LDA     #0                      init checksum to 0
CHK10   ADDA    ,X+
        DECB
        BNE     CHK10                   loop for all
        RTS                             return with checksum in A

***********************************************************************
*
*  Interrupt handlers to catch unused interrupts and traps
*  Registers are stacked.  Branch to INT_ENTRY, interrupt type in regA
*
*  This will affect only interrupt routines looking for register values!
*
*  Our default handler uses the code in "A" as the processor state to be
*  passed back to the host.
*
*  RES_ENT is "reserved" on 6809
*  Used for Divide-by-zero and Illegal-instruction on 6309.
*  It is not used because the trap address would crash the Basic ROM
*  routines. If you want to use this trap, the RESET section must copy
*  the line at TRAP_VEC to $0000.
RES_ENT     LDA     #7
            LBRA    INT_ENTRY
*
SWI3_ENT    LDA     #6
            LBRA    INT_ENTRY
*
SWI2_ENT    LDA     #5
            LBRA    INT_ENTRY
*
*  Will have only PC and CC's pushed unless we were waiting for an interrupt
*  or MD.1 is true.  Use CC's E bit to distinguish.
*  Push all registers here for common entry (else we can't use our RAM vector)
FIRQ_ENT    STA     REG_A,PCR   SAVE A REG
            PULS    A           GET CC'S FROM STACK
            BITA    #E
            BNE     FIRQ9       BR IF ALL REGISTERS PUSHED ALREADY
            PSHS    U,Y,X,DP    ELSE PUSH THEM NOW
            TST     CPU,PCR
            BNE     FE1
            PSHSW
FE1         PSHS    B
            LDB     REG_A,PCR
            PSHS    B
            ORA     #E          SET AS "ALL REGS PUSHED"
FIRQ9       PSHS    A           REPLACE CC'S
            LDA     #4
            LBRA    INT_ENTRY
*
IRQ_ENT     LDA     #3
            LBRA    INT_ENTRY
*
NMI_ENT     LDA     #2
            LBRA    INT_ENTRY
*
SWI_ENT     LDA     #1
            LBRA    INT_ENTRY
*
*============================================================================
INIT_UART
        PSHS    A
        ORCC    #I+F
        LDA     MPI_IMG,PCR
        ANDA    #$30
        ORA     #SLOT
        STA     MPI
        CLRA
        STA     SER_RESET
*  access baud generator, 1 stop bit, 8 data bits
*  fixed baud rate of 9600
        LDA     #$1E
        STA     SER_CONTROL
*
*
*  no ECHO, TXR on, RTS on, INTERRUPTS off
        LDA     #$0B
        STA     SER_COMMAND
*
        LDA     SER_RXDATA
        PULS    A,PC
*
*=============================================================================
*  VECTORS ARE IN ROM ON A COLOR COMPUTER AND CAN'T BE CHANGED
*  Second set of vectors are in RAM on a Coco1 or Coco2 and can be changed but
*  the reserved/trap and reset vectors are not included in the RAM set. The
*  Coco3 has two sets of RAM vectors but again the Trap and Reset are not
*  included.

TRAP_VEC

        LBRA    RES_ENT+TRAP_VEC                          fff0 (reserved/trap)

VECTORS 
        LBRA    SWI3_ENT-HARD_VECT+VECTORS                fff2 (SWI3)
        LBRA    SWI2_ENT-HARD_VECT+VECTORS                fff4 (SWI2)
*       LBRA    FIRQ_ENT-HARD_VECT+VECTORS                fff6 (FIRQ)
*       LBRA    IRQ_ENT-HARD_VECT+VECTORS                 fff8 (IRQ)
        LBRA    SWI_ENT-HARD_VECT+VECTORS                 fffa (SWI/breakpoint)
*       LBRA    NMI_ENT-HARD_VECT+VECTORS                 fffc (NMI 
*       FDB     RESET                   fffe reset
*
        END     RESET
