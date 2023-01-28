Declare Sub nada ' error instruccion desconocida
Declare Sub LODZ
Declare Sub LODI
Declare Sub LODR
Declare Sub LODA
Declare Sub LDPL
Declare Sub STPL
Declare Sub SPSU
Declare Sub SPSL
Declare Sub RETC
Declare Sub BCTR
Declare Sub BCTA
Declare Sub EORZ
Declare Sub EORI
Declare Sub EORR
Declare Sub EORA
Declare Sub REDC
Declare Sub RETE
Declare Sub BSTR
Declare Sub BSTA
Declare Sub HALT
Declare Sub ANDZ
Declare Sub ANDI
Declare Sub ANDR
Declare Sub ANDA
Declare Sub RRR
Declare Sub REDE
Declare Sub BRNR
Declare Sub BRNA
Declare Sub IORZ
Declare Sub IORI
Declare Sub IORR
Declare Sub IORA
Declare Sub REDD
Declare Sub CPSU
Declare Sub CPSL
Declare Sub PPSU
Declare Sub PPSL
Declare Sub BSNR
Declare Sub BSNA
Declare Sub ADDZ
Declare Sub ADDI
Declare Sub ADDR
Declare Sub ADDA
Declare Sub LPSU
Declare Sub LPSL
Declare Sub DAR
Declare Sub BCFR
Declare Sub ZBRR
Declare Sub BCFA
Declare Sub BXA
Declare Sub SUBZ
Declare Sub SUBI
Declare Sub SUBR
Declare Sub SUBA
Declare Sub WRTC
Declare Sub TPSU
Declare Sub TPSL
Declare Sub BSFR
Declare Sub ZBSR
Declare Sub BSFA
Declare Sub BSXA
Declare Sub NOP 
Declare Sub STRZ
Declare Sub STRR
Declare Sub STRA
Declare Sub RRL
Declare Sub WRTE
Declare Sub BIRR
Declare Sub BIRA
Declare Sub COMZ
Declare Sub COMI
Declare Sub COMR
Declare Sub COMA
Declare Sub WRTD
Declare Sub TMI
Declare Sub BDRR
Declare Sub BDRA

' instruccion incondicionales
Declare Sub RETCUN
Declare Sub BCTRUN
Declare Sub BCTAUN
Declare Sub RETEUN
Declare Sub BSTRUN
Declare Sub BSTAUN

' variables globales para las instrucciones
Dim shared as integer ret ' devuelve un codigo de error de algunas ins
dim shared as ubyte inst1 ' guarda los tres bytes de una ins (de 1 a 3)
dim shared as ubyte inst2
dim shared as ubyte inst3

dim shared as integer address ' temporal para guardar la EFFADDR
dim shared as integer effaddr ' effective address (direccion efectiva) calculada

Dim shared regnum as integer ' numero de registro a usar
Dim shared result As Integer ' generica para uso interno dentro de algunas INS
Dim shared ind    as integer ' cilos extras para accesos indirectos


' cpu function return codes
#define NORMAL      (0 shl 16)  ' just registers have changed
#define INV_OPCODE  (1 shl 16)  ' invalid opcode
#define HALT2       (2 shl 16)  ' HALT instruction encounted
#define MEM_WRITE   (3 shl 16)  ' a location in memory has been altered
#define MEM_READ    (4 shl 16)  ' a location in memory has been read
#define PC_WRITE    (5 shl 16)  ' A write to port-c
#define PC_READ     (6 shl 16)  ' A read from port-c
#define PD_WRITE    (7 shl 16)  ' A write to port-d
#define PD_READ     (8 shl 16)  ' A read from port-d
#define EXT_WRITE   (9 shl 16)  ' A write to port-d
#define EXT_READ   (10 shl 16)  ' A read from port-d

' modos de direccionamiento
   'Registro
   'Inmediato
   'Relativo
   'Relativo Indirecto
   'Absoluto
   'Absoluto Indirecto
   'Absoluto Indexado
   'Absoluto Indirecto Indexado

' addressing format
#define F_N  0  ' no addressing
#define F_Z  1  ' register addressing
#define F_I  2  ' immediate
#define F_R  3  ' relative
#define F_A  4  ' absolute non branch
#define F_B  5  ' absolute branch
#define F_C  6  ' absolute program status
#define F_E  7  ' miscellaneaus one byte instructions
#define F_EI 8  ' miscellaneaus immediate
#define F_ER 9  ' zero branch relative
#define F_EB 10 ' absolute branch index R3


' registros de estado PSL y PSU

' psu bits             ---------------
'                      + solo 2650B  +
' +------+------+------+      |      +------+------+------+
' |  S   |  F   |  II  | UF1  | UF2  | SP2  | SP1  | SP0  |
' +------+------+------+------+------+------+------+------+
'    7      6      5      4      3      2      1      0
'
#define SPMASK &h07     'Stack Pointer 0,1,2 (3 bits)
#define UF1   (1 shl 3) 'User Flags 1 (solo en 2650B)
#define UF2   (1 shl 4) 'User Flags 2 (solo en 2650B)
#define II    (1 shl 5) 'II:  Interrupt Inhibit
#define FLAG  (1 shl 6) 'F:   Flag
#define SENSE (1 shl 7) 'S:   Sense

' psl bits
'
' +------+------+------+------+------+------+------+------+
' | CC1  | CC0  | IDC  |  RS  |  WC  | OVF  | COM  | CYF  |
' +------+------+------+------+------+------+------+------+
'    7      6      5      4      3      2      1      0
'
#define CYF (1 shl 0) 'CYF: Carry/Borrow
#define COM (1 shl 1) 'COM: Logical/Arithmetic Compare
#define OVF (1 shl 2) 'OVF: Overflow
#define WC  (1 shl 3) 'WC:  With/Without Carry
#define RS  (1 shl 4) 'RS:  Register Bank Select
#define IDC (1 shl 5) 'IDC: Interdigit Carry
#define CC0 (1 shl 6) 'CC0: Condition Code 0
#define CC1 (1 shl 7) 'CC1: Condition Code 1



' registros
TYPE reg2650
  Dim As ubyte r(6)       ' registros R0,R1,R2,R3,R1',R2',R3' (7 registros, 0 a 6)
  Dim As ubyte psl        ' parte alta del registro de estado PSW
  Dim As ubyte psu        ' parte baja del registro de estado PSW
  Dim As integer st(7)      ' pila de datos LIFO (8 posiciones, 0 a 7)
END Type
dim shared r2650 as reg2650

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' instruciones, bytes ocupados, estado y ciclos
Type instructionset
	Dim ins As String*7
	Dim bytes As uByte
	Dim formato As uByte
	Dim ciclos As ubyte
End Type
Dim Shared opcode(256) As instructionset

' aqui siguen todas las instruciones, segun el formato
' anteriormente indicado

' 0
 DATA  "INVALID", 1, 0   , 0
 DATA  "LODZ R1", 1, F_Z , 2
 DATA  "LODZ R2", 1, F_Z , 2
 DATA  "LODZ R3", 1, F_Z , 2
 DATA  "LODI R0", 2, F_I , 2
 DATA  "LODI R1", 2, F_I , 2
 DATA  "LODI R2", 2, F_I , 2
 DATA  "LODI R3", 2, F_I , 2
 DATA  "LODR R0", 2, F_R , 3
 DATA  "LODR R1", 2, F_R , 3
 DATA  "LODR R2", 2, F_R , 3
 DATA  "LODR R3", 2, F_R , 3
 DATA  "LODA R0", 3, F_A , 4
 DATA  "LODA R1", 3, F_A , 4
 DATA  "LODA R2", 3, F_A , 4
 DATA  "LODA R3", 3, F_A , 4
' 1
 DATA  "LDPL   ", 3, F_C , 4
 DATA  "STPL   ", 3, F_C , 4
 DATA  "SPSU   ", 1, F_E , 2
 DATA  "SPSL   ", 1, F_E , 2
 DATA  "RETC  Z", 1, F_Z , 3
 DATA  "RETC  P", 1, F_Z , 3
 DATA  "RETC  N", 1, F_Z , 3
 DATA  "RETC UN", 1, F_Z , 3
 DATA  "BCTR  Z", 2, F_R , 3
 DATA  "BCTR  P", 2, F_R , 3
 DATA  "BCTR  N", 2, F_R , 3
 DATA  "BCTR UN", 2, F_R , 3
 DATA  "BCTA  Z", 3, F_B , 3
 DATA  "BCTA  P", 3, F_B , 3
 DATA  "BCTA  N", 3, F_B , 3
 DATA  "BCTA UN", 3, F_B , 3
' 2
 DATA  "EORZ R0", 1, F_Z , 2
 DATA  "EORZ R1", 1, F_Z , 2
 DATA  "EORZ R2", 1, F_Z , 2
 DATA  "EORZ R3", 1, F_Z , 2
 DATA  "EORI R0", 2, F_I , 2
 DATA  "EORI R1", 2, F_I , 2
 DATA  "EORI R2", 2, F_I , 2
 DATA  "EORI R3", 2, F_I , 2
 DATA  "EORR R0", 2, F_R , 3
 DATA  "EORR R1", 2, F_R , 3
 DATA  "EORR R2", 2, F_R , 3
 DATA  "EORR R3", 2, F_R , 3
 DATA  "EORA R0", 3, F_A , 4
 DATA  "EORA R1", 3, F_A , 4
 DATA  "EORA R2", 3, F_A , 4
 DATA  "EORA R3", 3, F_A , 4
' 3
 DATA  "REDC R0", 1, F_Z , 2
 DATA  "REDC R1", 1, F_Z , 2
 DATA  "REDC R2", 1, F_Z , 2
 DATA  "REDC R3", 1, F_Z , 2
 DATA  "RETE  Z", 1, F_Z , 3
 DATA  "RETE  P", 1, F_Z , 3
 DATA  "RETE  N", 1, F_Z , 3
 DATA  "RETE UN", 1, F_Z , 3
 DATA  "BSTR  Z", 2, F_R , 3
 DATA  "BSTR  P", 2, F_R , 3
 DATA  "BSTR  N", 2, F_R , 3
 DATA  "BSTR UN", 2, F_R , 3
 DATA  "BSTA  Z", 3, F_B , 3
 DATA  "BSTA  P", 3, F_B , 3
 DATA  "BSTA  N", 3, F_B , 3
 DATA  "BSTA UN", 3, F_B , 3
' 4
 DATA  "HALT   ", 1, F_E , 1
 DATA  "ANDZ R1", 1, F_Z , 2
 DATA  "ANDZ R2", 1, F_Z , 2
 DATA  "ANDZ R3", 1, F_Z , 2
 DATA  "ANDI R0", 2, F_I , 2
 DATA  "ANDI R1", 2, F_I , 2
 DATA  "ANDI R2", 2, F_I , 2
 DATA  "ANDI R3", 2, F_I , 2
 DATA  "ANDR R0", 2, F_R , 3
 DATA  "ANDR R1", 2, F_R , 3
 DATA  "ANDR R2", 2, F_R , 3
 DATA  "ANDR R3", 2, F_R , 3
 DATA  "ANDA R0", 3, F_A , 4
 DATA  "ANDA R1", 3, F_A , 4
 DATA  "ANDA R2", 3, F_A , 4
 DATA  "ANDA R3", 3, F_A , 4
' 5
 DATA  "RRR  R0", 1, F_Z , 2
 DATA  "RRR  R1", 1, F_Z , 2
 DATA  "RRR  R2", 1, F_Z , 2
 DATA  "RRR  R3", 1, F_Z , 2
 DATA  "REDE R0", 2, F_I , 3
 DATA  "REDE R1", 2, F_I , 3
 DATA  "REDE R2", 2, F_I , 3
 DATA  "REDE R3", 2, F_I , 3
 DATA  "BRNR R0", 2, F_R , 3
 DATA  "BRNR R1", 2, F_R , 3
 DATA  "BRNR R2", 2, F_R , 3
 DATA  "BRNR R3", 2, F_R , 3
 DATA  "BRNA R0", 3, F_B , 3
 DATA  "BRNA R1", 3, F_B , 3
 DATA  "BRNA R2", 3, F_B , 3
 DATA  "BRNA R3", 3, F_B , 3
' 6
 DATA  "IORZ R0", 1, F_Z , 2
 DATA  "IORZ R1", 1, F_Z , 2
 DATA  "IORZ R2", 1, F_Z , 2
 DATA  "IORZ R3", 1, F_Z , 2
 DATA  "IORI R0", 2, F_I , 2
 DATA  "IORI R1", 2, F_I , 2
 DATA  "IORI R2", 2, F_I , 2
 DATA  "IORI R3", 2, F_I , 2
 DATA  "IORR R0", 2, F_R , 3
 DATA  "IORR R1", 2, F_R , 3
 DATA  "IORR R2", 2, F_R , 3
 DATA  "IORR R3", 2, F_R , 3
 DATA  "IORA R0", 3, F_A , 4
 DATA  "IORA R1", 3, F_A , 4
 DATA  "IORA R2", 3, F_A , 4
 DATA  "IORA R3", 3, F_A , 4
' 7
 DATA  "REDD R0", 1, F_Z , 2
 DATA  "REDD R1", 1, F_Z , 2
 DATA  "REDD R2", 1, F_Z , 2
 DATA  "REDD R3", 1, F_Z , 2
 DATA  "CPSU   ", 2, F_EI, 3
 DATA  "CPSL   ", 2, F_EI, 3
 DATA  "PPSU   ", 2, F_EI, 3
 DATA  "PPSL   ", 2, F_EI, 3
 DATA  "BSNR  Z", 2, F_R , 3
 DATA  "BSNR  P", 2, F_R , 3
 DATA  "BSNR  N", 2, F_R , 3
 DATA  "BSNR UN", 2, F_R , 3
 DATA  "BSNA  Z", 3, F_B , 3
 DATA  "BSNA  P", 3, F_B , 3
 DATA  "BSNA  N", 3, F_B , 3
 DATA  "BSNA UN", 3, F_B , 3
' 8
 DATA  "ADDZ R0", 1, F_Z , 2
 DATA  "ADDZ R1", 1, F_Z , 2
 DATA  "ADDZ R2", 1, F_Z , 2
 DATA  "ADDZ R3", 1, F_Z , 2
 DATA  "ADDI R0", 2, F_I , 2
 DATA  "ADDI R1", 2, F_I , 2
 DATA  "ADDI R2", 2, F_I , 2
 DATA  "ADDI R3", 2, F_I , 2
 DATA  "ADDR R0", 2, F_R , 3
 DATA  "ADDR R1", 2, F_R , 3
 DATA  "ADDR R2", 2, F_R , 3
 DATA  "ADDR R3", 2, F_R , 3
 DATA  "ADDA R0", 3, F_A , 4
 DATA  "ADDA R1", 3, F_A , 4
 DATA  "ADDA R2", 3, F_A , 4
 DATA  "ADDA R3", 3, F_A , 4
' 9
 DATA  "INVALID", 1, 0   , 0
 DATA  "INVALID", 1, 0   , 0
 DATA  "LPSU   ", 1, F_E , 2
 DATA  "LPSL   ", 1, F_E , 2
 DATA  "DAR  R0", 1, F_Z , 3
 DATA  "DAR  R1", 1, F_Z , 3
 DATA  "DAR  R2", 1, F_Z , 3
 DATA  "DAR  R3", 1, F_Z , 3
 DATA  "BCFR  Z", 2, F_R , 3
 DATA  "BCFR  P", 2, F_R , 3
 DATA  "BCFR  N", 2, F_R , 3
 DATA  "ZBRR   ", 2, F_ER, 3
 DATA  "BCFA  Z", 3, F_B , 3
 DATA  "BCFA  P", 3, F_B , 3
 DATA  "BCFA  N", 3, F_B , 3
 DATA  "BXA  R3", 3, F_EB, 3
' A
 DATA  "SUBZ R0", 1, F_Z , 2
 DATA  "SUBZ R1", 1, F_Z , 2
 DATA  "SUBZ R2", 1, F_Z , 2
 DATA  "SUBZ R3", 1, F_Z , 2
 DATA  "SUBI R0", 2, F_I , 2
 DATA  "SUBI R1", 2, F_I , 2
 DATA  "SUBI R2", 2, F_I , 2
 DATA  "SUBI R3", 2, F_I , 2
 DATA  "SUBR R0", 2, F_R , 3
 DATA  "SUBR R1", 2, F_R , 3
 DATA  "SUBR R2", 2, F_R , 3
 DATA  "SUBR R3", 2, F_R , 3
 DATA  "SUBA R0", 3, F_A , 4
 DATA  "SUBA R1", 3, F_A , 4
 DATA  "SUBA R2", 3, F_A , 4
 DATA  "SUBA R3", 3, F_A , 4
' B
 DATA  "WRTC R0", 1, F_Z , 2
 DATA  "WRTC R1", 1, F_Z , 2
 DATA  "WRTC R2", 1, F_Z , 2
 DATA  "WRTC R3", 1, F_Z , 2
 DATA  "TPSU   ", 2, F_EI, 2
 DATA  "TPSL   ", 2, F_EI, 2
 DATA  "INVALID", 1, 0   , 0
 DATA  "INVALID", 1, 0   , 0
 DATA  "BSFR  Z", 2, F_R , 3
 DATA  "BSFR  P", 2, F_R , 3
 DATA  "BSFR  N", 2, F_R , 3
 DATA  "ZBSR   ", 2, F_ER, 3
 DATA  "BSFA  Z", 3, F_B , 3
 DATA  "BSFA  P", 3, F_B , 3
 DATA  "BSFA  N", 3, F_B , 3
 DATA  "BSXA R3", 3, F_EB, 3
' C
 DATA  "NOP    ", 1, F_Z , 1
 DATA  "STRZ R1", 1, F_Z , 2
 DATA  "STRZ R2", 1, F_Z , 2
 DATA  "STRZ R3", 1, F_Z , 2
 DATA  "INVALID", 1, 0   , 0
 DATA  "INVALID", 1, 0   , 0
 DATA  "INVALID", 1, 0   , 0
 DATA  "INVALID", 1, 0   , 0
 DATA  "STRR R0", 2, F_R , 3
 DATA  "STRR R1", 2, F_R , 3
 DATA  "STRR R2", 2, F_R , 3
 DATA  "STRR R3", 2, F_R , 3
 DATA  "STRA R0", 3, F_A , 4
 DATA  "STRA R1", 3, F_A , 4
 DATA  "STRA R2", 3, F_A , 4
 DATA  "STRA R3", 3, F_A , 4
' D
 DATA  "RRL  R0", 1, F_Z , 2
 DATA  "RRL  R1", 1, F_Z , 2
 DATA  "RRL  R2", 1, F_Z , 2
 DATA  "RRL  R3", 1, F_Z , 2
 DATA  "WRTE R0", 2, F_I , 3
 DATA  "WRTE R1", 2, F_I , 3
 DATA  "WRTE R2", 2, F_I , 3
 DATA  "WRTE R3", 2, F_I , 3
 DATA  "BIRR R0", 2, F_R , 3
 DATA  "BIRR R1", 2, F_R , 3
 DATA  "BIRR R2", 2, F_R , 3
 DATA  "BIRR R3", 2, F_R , 3
 DATA  "BIRA R0", 3, F_B , 3
 DATA  "BIRA R1", 3, F_B , 3
 DATA  "BIRA R2", 3, F_B , 3
 DATA  "BIRA R3", 3, F_B , 3
' E
 DATA  "COMZ R0", 1, F_Z , 2
 DATA  "COMZ R1", 1, F_Z , 2
 DATA  "COMZ R2", 1, F_Z , 2
 DATA  "COMZ R3", 1, F_Z , 2
 DATA  "COMI R0", 2, F_I , 2
 DATA  "COMI R1", 2, F_I , 2
 DATA  "COMI R2", 2, F_I , 2
 DATA  "COMI R3", 2, F_I , 2
 DATA  "COMR R0", 2, F_R , 3
 DATA  "COMR R1", 2, F_R , 3
 DATA  "COMR R2", 2, F_R , 3
 DATA  "COMR R3", 2, F_R , 3
 DATA  "COMA R0", 3, F_A , 4
 DATA  "COMA R1", 3, F_A , 4
 DATA  "COMA R2", 3, F_A , 4
 DATA  "COMA R3", 3, F_A , 4
' F
 DATA  "WRTD R0", 1, F_Z , 2
 DATA  "WRTD R1", 1, F_Z , 2
 DATA  "WRTD R2", 1, F_Z , 2
 DATA  "WRTD R3", 1, F_Z , 2
 DATA  "TMI  R0", 2, F_I , 3
 DATA  "TMI  R1", 2, F_I , 3
 DATA  "TMI  R2", 2, F_I , 3
 DATA  "TMI  R3", 2, F_I , 3
 DATA  "BDRR R0", 2, F_R , 3
 DATA  "BDRR R1", 2, F_R , 3
 DATA  "BDRR R2", 2, F_R , 3
 DATA  "BDRR R3", 2, F_R , 3
 DATA  "BDRA R0", 3, F_B , 3
 DATA  "BDRA R1", 3, F_B , 3
 DATA  "BDRA R2", 3, F_B , 3
 DATA  "BDRA R3", 3, F_B , 3
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''    funciones varias de ayuda la CPU ''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''

'Realiza funcion de suma y activa ICD,CY,OVF
Function adds(byte1 As ubyte,byte2 As ubyte) as ubyte
  dim as ushort result,result2

  result=0
  if ((r2650.psl) and (WC or CYF))=(WC or CYF) Then
    result=1
  end if
  
  result2=result
  result+=byte1+byte2
  result2+=(byte1 and 15)+(byte2 and 15)
  r2650.psl And=-1-1*IDC
  
  if (result2 and &h0010) <> 0 Then
    r2650.psl Or=IDC
  end if
  
  r2650.psl And=-1-1*CYF
  
  if (result and &h0100) <> 0 Then
  r2650.psl Or=CYF
  end if
  
  r2650.psl And=-1-1*OVF
  
  if (byte1 and &h80)=(byte2 and &h80) Then
    if (byte1 and &h80) <> (result and &h0080) Then
      r2650.psl Or=OVF
    end if
  end if
  
  return result and &hff
End function


'Realiza funcion de resta y activa ICD,CY,OVF
Function subs(byte1 As ubyte,byte2 As ubyte) as ubyte
  dim as ushort result,result2
  
  result=0
  if ((r2650.psl) and (WC or CYF))=WC Then
    result=1
  end if
  
  result2=result
  result=byte1-byte2-result
  result2=(byte1 and 15)-(byte2 and 15)-result2
  r2650.psl And=-1-1*IDC
  
  if (result2 and &h0010)=0 Then
    r2650.psl Or=IDC
  end if
  
  r2650.psl And=-1-1*CYF
  
  if (result and &h0100)=0 Then
       r2650.psl Or=CYF
  end if
  
  r2650.psl And=-1-1*OVF
  
  if (byte1 and &h80)=(byte2 and &h80) Then
    if (byte1 and &h80) <> (result and &h0080) Then
      r2650.psl Or=OVF
    end if
  end if
  
  return result and &hff
End function
  
'Instruccion RR (rotate Right)
Function rr(datobyte as ubyte) as ubyte
  dim as ushort result
  
  result=datobyte
  
  if (r2650.psl and WC) <> 0 Then
    if (r2650.psl and CYF) <> 0 Then
      result Or=&h0100
    end if
    r2650.psl And=-1-1*CYF
    if (result and &h0001) <> 0 Then
      r2650.psl Or=CYF
    end if
    r2650.psl And=-1-1*IDC
    if (result and &h0040) <> 0 Then
      r2650.psl Or=IDC
    end if
  else
    if (result and &h0001) <> 0 Then
      result Or=&h0100
    end if
  end if
  
  result=result shr 1
  
  return result and &hff
end function

'Instruccion RL (rotate left)
Function rl(datobyte as ubyte) as ubyte
  dim as ushort result
  
  result=datobyte shl 1
  
  if (r2650.psl and WC) <> 0 Then
    if (r2650.psl and CYF) <> 0 Then
      result Or=&h0001
    end if
    r2650.psl And=-1-1*CYF
    if (result and &h0100) <> 0 Then
      r2650.psl Or=CYF
    end if
    r2650.psl And=-1-1*IDC
    if (result and &h0020) <> 0 Then
      r2650.psl Or=IDC
    end if
  else
    if (result and &h0100) <> 0 Then
      result Or=&h0001
    end if
  end if

  return result and &hff
end function


' Realiza Comparacion
'  positivo si byte1>byte2
'  negativo si byte1<byte2
'  cero si byte1=byte2
' segun el estado de COM in PSL se determina el modo de comparacion
'  0 indica con signo , 1 indica sin signo
Function compare(byte1 As ubyte,byte2 As ubyte) as ubyte
  dim as ushort result
  result=0
  
  'comparacion sin signo
  if (r2650.psl and COM)<>0 Then 
    if ((byte1 and &h80)<>0) and ((byte2 and &h80)=0)Then
      result=&h0001
    end if
    if ((byte1 and &h80)=0) and ((byte2 and &h80)<>0) Then
      result=&hffff
    end if
    
  'comparacion con signo 
  else 
    if ((byte1 and &h80)<>0) and ((byte2 and &h80)=0) Then
      result=&hffff
    end if
    if ((byte1 and &h80)=0) and ((byte2 and &h80)<>0) Then
      result=&h0001
    End If
  end if
  
  if (result=0) Then
    result=byte1-byte2
  end if

  return result and &hff
end function

'Actualiza CC (condition code)
Sub setcc(datobyte as ubyte)
    r2650.psl And=(-1-1*(CC1 or CC0))
    if (datobyte and &h80) <> 0 Then
      r2650.psl Or=CC1
    else
      if (datobyte and &h7f) <> 0 Then
        r2650.psl Or=CC0
      end if
    end if
end sub
        
'Realiza un PUSH (guardar en la pila)
Sub push(address as ushort)
  r2650.st(r2650.psu and &h07)=address
  r2650.psu=((r2650.psu+1) and &h07) or (r2650.psu and &hf8)
end sub
  
'Realiza un PULL (coger de la pila)
function pull() as ushort
  r2650.psu=((r2650.psu-1) and &h07) or (r2650.psu and &hf8)
  return(r2650.st(r2650.psu and &h07))
end function
  
'Genera una interrupcion
Sub interrupt()
  'if (r2650.psu and II) then exit sub ' no es posible otra interrupcion, si ya tenemos una activa
  r2650.psu or=II ' desactivamos IRQ , ponemos a "1" para que no vuelva a generar otra
  push(direxe)
  dircpu=&h0003 ' IRQ en interton/elektor
  'dircpu=&h001d ' IRQ en PHUNSY
end sub

' PEEK RAM
function cogebyte(address as integer) as ubyte
   
   'if (address>maxrom and address<&h1f00) then cls:print address:sleep
   'if (address>&h1fff) then cls:print address:sleep
   'if address=&h75 then dormir=1
   return ram(address and maxram) and &hff
End Function

' POKE RAM
sub ponbyte(address as integer,dato as ubyte)

   dato and=&hff
   'if (address>maxrom and address<&h1f00) then cls:print address:sleep
   'if (address>&h1fff) then cls:print address:sleep
   'IF DATO=&Hee THEN 
   '   if address=&h878 then
   '    locate 25,25:print "dir:";hex(address,4)
   '    dormir=1
   '   EndIf
   'end if
       'ram(&h1f00)=&hee
       'ram(&h1f01)=&h44
       'ram(&h1f02)=&h44
       'ram(&h1f03)=&h44
       'ram(&h1f04)=&hee
       'ram(&h1f10)=&hee
       'ram(&h1f11)=&h44
       'ram(&h1f12)=&h44
       'ram(&h1f13)=&h44
       'ram(&h1f14)=&hee

      'if address=&h007d then locate 25,25:print dato
      
   if address >= maxrom then 
      'ram(address and maxram)=dato   
      ram(address)=dato   
   EndIf

End Sub





' -------------------------------------------
'               ejecuta una INS
' -------------------------------------------
Sub execins()
   dim opcode as ubyte

	opcode=inst1
	
	If opcode = &h0 Then  nada : exit sub  'error
	If opcode = &h1 Then  LODZ : exit sub
	If opcode = &h2 Then  LODZ : exit sub
	If opcode = &h3 Then  LODZ : exit sub
	
	If opcode = &h4 Then  LODI : exit sub
	If opcode = &h5 Then  LODI : exit sub
	If opcode = &h6 Then  LODI : exit sub
	If opcode = &h7 Then  LODI : exit sub
	
	If opcode = &h8 Then  LODR : exit sub
	If opcode = &h9 Then  LODR : exit sub
	If opcode = &hA Then  LODR : exit sub
	If opcode = &hB Then  LODR : exit sub
	
	If opcode = &hC Then  LODA : exit sub
	If opcode = &hD Then  LODA : exit sub
	If opcode = &hE Then  LODA : exit sub
	If opcode = &hF Then  LODA : exit sub
	
	If opcode = &h10 Then  LDPL : exit sub
	If opcode = &h11 Then  STPL : exit sub
	If opcode = &h12 Then  SPSU : exit sub
	If opcode = &h13 Then  SPSL : exit sub
	
	If opcode = &h14 Then  RETC : exit sub
	If opcode = &h15 Then  RETC : exit sub
	If opcode = &h16 Then  RETC : exit sub
	If opcode = &h17 Then  RETCUN : exit sub 'incondicional
	
	If opcode = &h18 Then  BCTR : exit sub
	If opcode = &h19 Then  BCTR : exit sub
	If opcode = &h1A Then  BCTR : exit sub
	If opcode = &h1B Then  BCTRUN : exit sub 'incondicional
	
	If opcode = &h1C Then  BCTA : exit sub
	If opcode = &h1D Then  BCTA : exit sub
	If opcode = &h1E Then  BCTA : exit sub
	If opcode = &h1F Then  BCTAUN : exit sub 'incondicional
	
	If opcode = &h20 Then  EORZ : exit sub
	If opcode = &h21 Then  EORZ : exit sub
	If opcode = &h22 Then  EORZ : exit sub
	If opcode = &h23 Then  EORZ : exit sub
	
	If opcode = &h24 Then  EORI : exit sub
	If opcode = &h25 Then  EORI : exit sub
	If opcode = &h26 Then  EORI : exit sub
	If opcode = &h27 Then  EORI : exit sub
	
	If opcode = &h28 Then  EORR : exit sub
	If opcode = &h29 Then  EORR : exit sub
	If opcode = &h2A Then  EORR : exit sub
	If opcode = &h2B Then  EORR : exit sub
	
	If opcode = &h2C Then  EORA : exit sub
	If opcode = &h2D Then  EORA : exit sub
	If opcode = &h2E Then  EORA : exit sub
	If opcode = &h2F Then  EORA : exit sub
	
	If opcode = &h30 Then  REDC : exit sub
	If opcode = &h31 Then  REDC : exit sub
	If opcode = &h32 Then  REDC : exit sub
	If opcode = &h33 Then  REDC : exit sub
	
	If opcode = &h34 Then  RETE : exit sub
	If opcode = &h35 Then  RETE : exit sub
	If opcode = &h36 Then  RETE : exit sub
	If opcode = &h37 Then  RETEUN : exit sub 'incondicional
	
	If opcode = &h38 Then  BSTR : exit sub
	If opcode = &h39 Then  BSTR : exit sub
	If opcode = &h3A Then  BSTR : exit sub
	If opcode = &h3B Then  BSTRUN : exit sub 'incondicional
	
	If opcode = &h3C Then  BSTA : exit sub
	If opcode = &h3D Then  BSTA : exit sub
	If opcode = &h3E Then  BSTA : exit sub
	If opcode = &h3F Then  BSTAUN : exit sub 'incondicional
	
	If opcode = &h40 Then  HALT : exit sub ' HALT
	If opcode = &h41 Then  ANDZ : exit sub
	If opcode = &h42 Then  ANDZ : exit sub
	If opcode = &h43 Then  ANDZ : exit sub
	
	If opcode = &h44 Then  ANDI : exit sub
	If opcode = &h45 Then  ANDI : exit sub
	If opcode = &h46 Then  ANDI : exit sub
	If opcode = &h47 Then  ANDI : exit sub
	
	If opcode = &h48 Then  ANDR : exit sub
	If opcode = &h49 Then  ANDR : exit sub
	If opcode = &h4A Then  ANDR : exit sub
	If opcode = &h4B Then  ANDR : exit sub
	
	If opcode = &h4C Then  ANDA : exit sub
	If opcode = &h4D Then  ANDA : exit sub
	If opcode = &h4E Then  ANDA : exit sub
	If opcode = &h4F Then  ANDA : exit sub
	
	If opcode = &h50 Then  RRR  : exit sub
	If opcode = &h51 Then  RRR  : exit sub
	If opcode = &h52 Then  RRR  : exit sub
	If opcode = &h53 Then  RRR  : exit sub
	
	If opcode = &h54 Then  REDE : exit sub
	If opcode = &h55 Then  REDE : exit sub
	If opcode = &h56 Then  REDE : exit sub
	If opcode = &h57 Then  REDE : exit sub
	
	If opcode = &h58 Then  BRNR : exit sub
	If opcode = &h59 Then  BRNR : exit sub
	If opcode = &h5A Then  BRNR : exit sub
	If opcode = &h5B Then  BRNR : exit sub
	
	If opcode = &h5C Then  BRNA : exit sub
	If opcode = &h5D Then  BRNA : exit sub
	If opcode = &h5E Then  BRNA : exit sub
	If opcode = &h5F Then  BRNA : exit sub
	
	If opcode = &h60 Then  IORZ : exit sub
	If opcode = &h61 Then  IORZ : exit sub
	If opcode = &h62 Then  IORZ : exit sub
	If opcode = &h63 Then  IORZ : exit sub
	
	If opcode = &h64 Then  IORI : exit sub
	If opcode = &h65 Then  IORI : exit sub
	If opcode = &h66 Then  IORI : exit sub
	If opcode = &h67 Then  IORI : exit sub
	
	If opcode = &h68 Then  IORR : exit sub
	If opcode = &h69 Then  IORR : exit sub
	If opcode = &h6A Then  IORR : exit sub
	If opcode = &h6B Then  IORR : exit sub
	
	If opcode = &h6C Then  IORA : exit sub
	If opcode = &h6D Then  IORA : exit sub
	If opcode = &h6E Then  IORA : exit sub
	If opcode = &h6F Then  IORA : exit sub
	
	If opcode = &h70 Then  REDD : exit sub
	If opcode = &h71 Then  REDD : exit sub
	If opcode = &h72 Then  REDD : exit sub
	If opcode = &h73 Then  REDD : exit sub
	
	If opcode = &h74 Then  CPSU : exit sub
	If opcode = &h75 Then  CPSL : exit sub
	If opcode = &h76 Then  PPSU : exit sub
	If opcode = &h77 Then  PPSL : exit sub
	
	If opcode = &h78 Then  BSNR : exit sub
	If opcode = &h79 Then  BSNR : exit sub
	If opcode = &h7A Then  BSNR : exit sub
	If opcode = &h7B Then  BSNR : exit sub
	
	If opcode = &h7C Then  BSNA : exit sub
	If opcode = &h7D Then  BSNA : exit sub
	If opcode = &h7E Then  BSNA : exit sub
	If opcode = &h7F Then  BSNA : exit sub
	
	If opcode = &h80 Then  ADDZ : exit sub
	If opcode = &h81 Then  ADDZ : exit sub
	If opcode = &h82 Then  ADDZ : exit sub
	If opcode = &h83 Then  ADDZ : exit sub
	
	If opcode = &h84 Then  ADDI : exit sub
	If opcode = &h85 Then  ADDI : exit sub
	If opcode = &h86 Then  ADDI : exit sub
	If opcode = &h87 Then  ADDI : exit sub
	
	If opcode = &h88 Then  ADDR : exit sub
	If opcode = &h89 Then  ADDR : exit sub
	If opcode = &h8A Then  ADDR : exit sub
	If opcode = &h8B Then  ADDR : exit sub
	
	If opcode = &h8C Then  ADDA : exit sub
	If opcode = &h8D Then  ADDA : exit sub
	If opcode = &h8E Then  ADDA : exit sub
	If opcode = &h8F Then  ADDA : exit sub
	
	If opcode = &h90 Then  nada : exit sub ' error
	If opcode = &h91 Then  nada : exit sub ' error
	If opcode = &h92 Then  LPSU : exit sub
	If opcode = &h93 Then  LPSL : exit sub
	
	If opcode = &h94 Then  DAR  : exit sub
	If opcode = &h95 Then  DAR  : exit sub
	If opcode = &h96 Then  DAR  : exit sub
	If opcode = &h97 Then  DAR  : exit sub
	
	If opcode = &h98 Then  BCFR : exit sub
	If opcode = &h99 Then  BCFR : exit sub
	If opcode = &h9A Then  BCFR : exit sub
	If opcode = &h9B Then  ZBRR : exit sub
	
	If opcode = &h9C Then  BCFA : exit sub
	If opcode = &h9D Then  BCFA : exit sub
	If opcode = &h9E Then  BCFA : exit sub
	If opcode = &h9F Then  BXA  : exit sub
	
	If opcode = &hA0 Then  SUBZ : exit sub
	If opcode = &hA1 Then  SUBZ : exit sub
	If opcode = &hA2 Then  SUBZ : exit sub
	If opcode = &hA3 Then  SUBZ : exit sub
	
	If opcode = &hA4 Then  SUBI : exit sub
	If opcode = &hA5 Then  SUBI : exit sub
	If opcode = &hA6 Then  SUBI : exit sub
	If opcode = &hA7 Then  SUBI : exit sub
	
	If opcode = &hA8 Then  SUBR : exit sub
	If opcode = &hA9 Then  SUBR : exit sub
	If opcode = &hAA Then  SUBR : exit sub
	If opcode = &hAB Then  SUBR : exit sub
	
	If opcode = &hAC Then  SUBA : exit sub
	If opcode = &hAD Then  SUBA : exit sub
	If opcode = &hAE Then  SUBA : exit sub
	If opcode = &hAF Then  SUBA : exit sub
	
	If opcode = &hB0 Then  WRTC : exit sub
	If opcode = &hB1 Then  WRTC : exit sub
	If opcode = &hB2 Then  WRTC : exit sub
	If opcode = &hB3 Then  WRTC : exit sub
	
	If opcode = &hB4 Then  TPSU : exit sub
	If opcode = &hB5 Then  TPSL : exit sub
	If opcode = &hB6 Then  nada : exit sub
	If opcode = &hB7 Then  nada : exit sub
	
	If opcode = &hB8 Then  BSFR : exit sub
	If opcode = &hB9 Then  BSFR : exit sub
	If opcode = &hBA Then  BSFR : exit sub
	If opcode = &hBB Then  ZBSR : exit sub
	
	If opcode = &hBC Then  BSFA : exit sub
	If opcode = &hBD Then  BSFA : exit sub
	If opcode = &hBE Then  BSFA : exit sub
	If opcode = &hBF Then  BSXA : exit sub
	
	If opcode = &hC0 Then  NOP  : exit sub  ' NOP
	If opcode = &hC1 Then  STRZ : exit sub
	If opcode = &hC2 Then  STRZ : exit sub
	If opcode = &hC3 Then  STRZ : exit sub
	
	If opcode = &hC4 Then  nada : exit sub
	If opcode = &hC5 Then  nada : exit sub
	If opcode = &hC6 Then  nada : exit sub
	If opcode = &hC7 Then  nada : exit sub
	
	If opcode = &hC8 Then  STRR : exit sub
	If opcode = &hC9 Then  STRR : exit sub
	If opcode = &hCA Then  STRR : exit sub
	If opcode = &hCB Then  STRR : exit sub
	
	If opcode = &hCC Then  STRA : exit sub
	If opcode = &hCD Then  STRA : exit sub
	If opcode = &hCE Then  STRA : exit sub
	If opcode = &hCF Then  STRA : exit sub
	
	If opcode = &hD0 Then  RRL  : exit sub
	If opcode = &hD1 Then  RRL  : exit sub
	If opcode = &hD2 Then  RRL  : exit sub
	If opcode = &hD3 Then  RRL  : exit sub
	
	If opcode = &hD4 Then  WRTE : exit sub
	If opcode = &hD5 Then  WRTE : exit sub
	If opcode = &hD6 Then  WRTE : exit sub
	If opcode = &hD7 Then  WRTE : exit sub
	
	If opcode = &hD8 Then  BIRR : exit sub
	If opcode = &hD9 Then  BIRR : exit sub
	If opcode = &hDA Then  BIRR : exit sub
	If opcode = &hDB Then  BIRR : exit sub
	
	If opcode = &hDC Then  BIRA : exit sub
	If opcode = &hDD Then  BIRA : exit sub
	If opcode = &hDE Then  BIRA : exit sub
	If opcode = &hDF Then  BIRA : exit sub
	
	If opcode = &hE0 Then  COMZ : exit sub
	If opcode = &hE1 Then  COMZ : exit sub
	If opcode = &hE2 Then  COMZ : exit sub
	If opcode = &hE3 Then  COMZ : exit sub
	
	If opcode = &hE4 Then  COMI : exit sub
	If opcode = &hE5 Then  COMI : exit sub
	If opcode = &hE6 Then  COMI : exit sub
	If opcode = &hE7 Then  COMI : exit sub
	
	If opcode = &hE8 Then  COMR : exit sub
	If opcode = &hE9 Then  COMR : exit sub
	If opcode = &hEA Then  COMR : exit sub
	If opcode = &hEB Then  COMR : exit sub
	
	If opcode = &hEC Then  COMA : exit sub
	If opcode = &hED Then  COMA : exit sub
	If opcode = &hEE Then  COMA : exit sub
	If opcode = &hEF Then  COMA : exit sub
	
	If opcode = &hF0 Then  WRTD : exit sub
	If opcode = &hF1 Then  WRTD : exit sub
	If opcode = &hF2 Then  WRTD : exit sub
	If opcode = &hF3 Then  WRTD : exit sub
	
	If opcode = &hF4 Then  TMI  : exit sub
	If opcode = &hF5 Then  TMI  : exit sub
	If opcode = &hF6 Then  TMI  : exit sub
	If opcode = &hF7 Then  TMI  : exit sub
	
	If opcode = &hF8 Then  BDRR : exit sub
	If opcode = &hF9 Then  BDRR : exit sub
	If opcode = &hFA Then  BDRR : exit sub
	If opcode = &hFB Then  BDRR : exit sub
	
	If opcode = &hFC Then  BDRA : exit sub
	If opcode = &hFD Then  BDRA : exit sub
	If opcode = &hFE Then  BDRA : exit sub
	If opcode = &hFF Then  BDRA : exit sub


   nada() ' en caso de opcode <0 o >255, raro, pero posible
End Sub


' $00   nada  instruccion desconocida  
Sub nada
    ' error de instruccion
    locate 20,20
    print "INVALID OPCODE:";hex(INST1,2);" - ";INST1;" en ";hex(DIRCPU,4)
    sleep
    ret=(INV_OPCODE or dircpu)
End Sub
' -----

' $01   LODZ r1        
Sub LODZ
      result=r2650.r(regnum)
      r2650.r(0)=result
      setcc(result)
End Sub
' -----

' $04   LODI,r0        
Sub LODI
      result=inst2
      r2650.r(regnum)=result
      setcc(result)
End Sub
' -----

' $08   LODR,r0        
Sub LODR
      LODA() ' es igual que LODA
End Sub
' -----

' $0C   LODA,r0        
Sub LODA
      result=cogebyte(effaddr)
      r2650.r(regnum)=result
      setcc(result)
      ciclos+=ind
      ret=MEM_READ or effaddr
End Sub
' -----

' $10   LDPL  (solo 2650B)   
Sub LDPL
      r2650.psl=cogebyte(effaddr)
      ciclos+=ind
      ret=MEM_READ or effaddr
End Sub
' -----

' $11   STPL  (solo 2650B)  
Sub STPL
      ponbyte(effaddr,r2650.psl)
      ciclos+=ind
      ret=MEM_WRITE or effaddr
End Sub
' -----

' $12   SPSU           
Sub SPSU
      r2650.r(0)=r2650.psu
      setcc(r2650.r(0))
End Sub
' -----

' $13   SPSL           
Sub SPSL
      r2650.r(0)=r2650.psl
      setcc(r2650.r(0))
End Sub
' -----

' $14   RETC,eq        
Sub RETC
      if ((inst1 and &h03)=((r2650.psl shr 6) and &h03)) Then
        address=pull()
      end if
End Sub
' -----

' $17   RETC UN        
Sub RETCUN
      address=pull()
End Sub
' -----

' $18   BCTR,eq        
Sub BCTR
      BCTA() ' igual
End Sub
' -----

' $1C   BCTA,eq        
Sub BCTA
      if (inst1 and &h03)=((r2650.psl shr 6) and &h03) Then
        address=effaddr
        ciclos+=ind
      end if
End Sub
' -----

' $1B BCTR UN
sub BCTRUN
      BCTAUN() ' igual
End Sub
' -----

' $1F BCTA UN
sub BCTAUN
      address=effaddr
      ciclos+=ind
End Sub
' -----

' $20   EORZ r0        
Sub EORZ
      result=r2650.r(regnum) and (-1-1*r2650.r(0)) or (-1-1*r2650.r(regnum)) and r2650.r(0)
      r2650.r(0)=result
      setcc(result)
End Sub
' -----

' $24   EORI,r0        
Sub EORI
      result=r2650.r(regnum) and (-1-1*inst2) or (-1-1*r2650.r(regnum)) and inst2
      r2650.r(regnum)=result
      setcc(result)
End Sub
' -----

' $28   EORR,r0        
Sub EORR
      EORA() ' igual que EORA
End Sub
' -----

' $2C   EORA,r0        
Sub EORA
      result=r2650.r(regnum) and (-1-1*cogebyte(effaddr)) or (-1-1*r2650.r(regnum)) and cogebyte(effaddr)
      r2650.r(regnum)=result
      setcc(result)
      ciclos+=ind
      ret=MEM_READ or effaddr
End Sub
' -----

' $30   REDC,r0        
Sub REDC
      result=puerto_in
      r2650.r(regnum)=result
      setcc(result)
      ret=PC_READ
End Sub
' -----

' $34   RETE,eq        
Sub RETE
      if (inst1 and &h03)=((r2650.psl shr 6) and &h03) Then
        r2650.psu And=-1-1*II:'clear interrupt inhibit in the psu
        address=pull()
      End If
End Sub
' -----

' $37   RETE UN       
Sub RETEUN
      r2650.psu And=-1-1*II:'clear interrupt inhibit in the psu
      address=pull()
End Sub
' -----

' $38   BSTR,eq        
Sub BSTR
       BSTA() ' igual
End Sub
' -----

' $3C   BSTA,eq        
Sub BSTA
      if (inst1 and &h03)=((r2650.psl shr 6) and &h03) Then
        push(address)
        address=effaddr
        ciclos+=ind
      End if
End Sub
' -----

' $3b   BSTR UN        
Sub BSTRUN
      BSTAUN() ' igual
End Sub
' -----

' $3f   BSTA UN        
Sub BSTAUN
      push(address)
      address=effaddr
      ciclos+=ind
End Sub
' -----

' $40   HALT           
Sub HALT
    ret=HALT2 or dircpu
    print "CPU HALT!!!!!"
    sleep 100,1
End Sub
' -----

' $41   ANDZ r1        
Sub ANDZ
      result=r2650.r(regnum) and r2650.r(0)
      r2650.r(0)=result
      setcc(result)
End Sub
' -----

' $44   ANDI,r0        
Sub ANDI
      result=r2650.r(regnum) and inst2
      r2650.r(regnum)=result
      setcc(result)
End Sub
' -----

' $48   ANDR,r0        
Sub ANDR
      ANDA() 'igual que ANDA
End Sub
' -----

' $4C   ANDA,r0        
Sub ANDA
      result=r2650.r(regnum) and cogebyte(effaddr)
      r2650.r(regnum)=result
      setcc(result)
      ciclos+=ind
      ret=MEM_READ or effaddr
End Sub
' -----

' $50   RRR,r0         
Sub RRR
      result=rr(r2650.r(regnum))
      r2650.r(regnum)=result
      setcc(result)
End Sub
' -----

' $54   REDE,r0        
Sub REDE
      if (inst2<128) Then
        ret=inst2
      else
        ret=(inst2-128)*(puerto_out/16)+128
      end if
      r2650.r(regnum)=extio(ret)
      setcc(r2650.r(regnum))
      ret Or=EXT_READ
End Sub
' -----

' $58   BRNR,r0        
Sub BRNR
      BRNA() ' igual
End Sub
' -----

' $5C   BRNA,r0         
Sub BRNA
      if r2650.r(regnum)<>0 Then
        address=effaddr
        ciclos+=ind
      End If
End Sub
' -----

' $60   IORZ r0         
Sub IORZ
      result=r2650.r(regnum) or r2650.r(0)
      r2650.r(0)=result
      setcc(result)
End Sub
' -----

' $64   IORI,r0         
Sub IORI
      result=r2650.r(regnum) or inst2
      r2650.r(regnum)=result
      setcc(result)
End Sub
' -----

' $68   IORR,r0         
Sub IORR
     IORA() ' igual a IORA
End Sub
' -----

' $6C   IORA,r0         
Sub IORA
      result=r2650.r(regnum) or cogebyte(effaddr)
      r2650.r(regnum)=result
      setcc(result)
      ciclos+=ind
      ret=MEM_READ or effaddr
End Sub
' -----

' $70   REDD,r0         
Sub REDD
      result=puerto_in
      r2650.r(regnum)=result
      setcc(result)
      ret=PD_READ
End Sub
' -----

' $74   CPSU            
Sub CPSU
      r2650.psu And=r2650.psu and (-1-1*inst2)
End Sub
' -----

' $75   CPSL            
Sub CPSL
      r2650.psl And=r2650.psl and (-1-1*inst2)
End Sub
' -----

' $76   PPSU            
Sub PPSU
      r2650.psu=r2650.psu or inst2
End Sub
' -----

' $77   PPSL            
Sub PPSL
      r2650.psl=r2650.psl or inst2
End Sub
' -----

' $78   BSNR,r0         
Sub BSNR
      BSNA() ' igual
End Sub
' -----

' $7C   BSNA,r0         
Sub BSNA
      if r2650.r(regnum)<>0 Then
        push(address)
        address=effaddr
        ciclos+=ind
      End If
End Sub
' -----

' $80   ADDZ r0 
Sub ADDZ
      result=adds(r2650.r(0),r2650.r(regnum))
      r2650.r(0)=result
      setcc(result)
End Sub
' -----

' $84   ADDI,r0 
Sub ADDI
      result=adds(r2650.r(regnum),inst2)
      r2650.r(regnum)=result
      setcc(result)
End Sub
' -----

' $88   ADDR,r0 
Sub ADDR
      ADDA() ' igual
End Sub
' -----

' $8C   ADDA,r0 
Sub ADDA
      result=adds(r2650.r(regnum),cogebyte(effaddr))
      r2650.r(regnum)=result
      setcc(result)
      ciclos+=ind
      ret=MEM_READ or effaddr
End Sub
' -----

' $92   LPSU    
Sub LPSU
      r2650.psu=r2650.r(0)
End Sub
' -----

' $93   LPSL    
Sub LPSL
      r2650.psl=r2650.r(0)
End Sub
' -----

' $94   DAR,r0  
Sub DAR
    result=r2650.r(regnum)
    if (r2650.psl and CYF)=0 Then
      result=(result and &h0f) or ((result+&ha0) and &hf0)
    end if
    if (r2650.psl and IDC)=0 Then
      result=(result and &hf0) or ((result+&h0a) and &h0f)
    end if
   r2650.r(regnum)=result
End Sub
' -----

' $98   BCFR,eq 
Sub BCFR
      BCFA() ' igual a BCFA
End Sub
' -----

' $9B   ZBRR    
Sub ZBRR
     BXA() ' igual a BXA
End Sub
' -----

' $9C   BCFA,eq 
Sub BCFA
      if (inst1 and &h03) <> ((r2650.psl shr 6) and &h03) Then
        address=effaddr
        ciclos+=ind
      End If
End Sub
' -----

' $9F   BXA,r3  
Sub BXA
      address=effaddr
      ciclos+=ind
End Sub
' -----

' $A0   SUBZ r0 
Sub SUBZ
      result=subs(r2650.r(0),r2650.r(regnum))
      r2650.r(0)=result
      setcc(result)
End Sub
' -----

' $A4   SUBI,r0 
Sub SUBI
      result=subs(r2650.r(regnum),inst2)
      r2650.r(regnum)=result
      setcc(result)
End Sub
' -----

' $A8   SUBR,r0 
Sub SUBR
      SUBA() ' igual
End Sub
' -----

' $AC   SUBA,r0 
Sub SUBA
      result=subs(r2650.r(regnum),cogebyte(effaddr))
      r2650.r(regnum)=result
      setcc(result)
      ciclos+=ind
      ret=MEM_READ or effaddr
End Sub
' -----

' $B0   WRTC,r0 
Sub WRTC
      puerto_out=r2650.r(regnum)
      ret=PC_WRITE
End Sub
' -----

' $B4   TPSU    
Sub TPSU
      result=r2650.psu and inst2
      r2650.psl And=-1-1*(CC0 or CC1):'clear cc bits
      if result <> inst2 Then
        r2650.psl Or=&h80:'indicate not all selected bits are'1'
      end if
End Sub
' -----

' $B5   TPSL    
Sub TPSL
      result=r2650.psl and inst2
      r2650.psl And=-1-1*(CC0 or CC1):'clear cc bits
      if result <>inst2 Then
        r2650.psl Or=&h80:'indicate not all selected bits are'1'
      end if
End Sub
' -----

' $B8   BSFR,eq 
Sub BSFR
      BSFA() ' igual a BSFA
End Sub
' -----

' $BB   ZBSR    
Sub ZBSR
     BSXA() ' igual a BSXA
End Sub
' -----

' $BC   BSFA,eq 
Sub BSFA
      if (inst1 and &h03) <> ((r2650.psl shr 6) and &h03) Then
        push(address)
        address=effaddr
        ciclos+=ind
      End If
End Sub
' -----

' $BF   BSXA,r3 
Sub BSXA
      push(address)
      address=effaddr
      ciclos+=ind
End Sub
' -----

' $C0   NOP     
Sub NOP 
  ' nada
End Sub
' -----

' $C1   STRZ r1 
Sub STRZ
      r2650.r(regnum)=r2650.r(0)
End Sub
' -----

' $C8   STRR,r0 
Sub STRR
      STRA() ' igual
End Sub
' -----

' $CC   STRA,r0 
Sub STRA
      ponbyte(effaddr,r2650.r(regnum))
      ciclos+=ind
      ret=MEM_WRITE or effaddr
End Sub
' -----

' $D0   RRL,r0  
Sub RRL
      result=rl(r2650.r(regnum))
      r2650.r(regnum)=result
      setcc(result)
End Sub
' -----

' $D4   WRTE,r0 
Sub WRTE
      if(inst2<128) Then
        ret=inst2
      else
        ret=(inst2-128)*(puerto_out/16)+128
      end if
      extio(ret)=r2650.r(regnum)
      
      ' uso exclusivo emulador de terminal RS232
      'if ret=9 then tengocaracter=1
      
      ret Or=EXT_WRITE
End Sub
' -----

' $D8   BIRR,r0 
Sub BIRR
      BIRA() ' igual
End Sub
' -----

' $DC   BIRA,r0 
Sub BIRA
      r2650.r(regnum)+=1
      if r2650.r(regnum) <>0 Then
        address=effaddr
        ciclos+=ind
      End If
End Sub
' -----

' $E0   COMZ,r0 
Sub COMZ
      result=compare(r2650.r(0),r2650.r(regnum))
      setcc(result)
End Sub
' -----

' $E4   COMI,r0 
Sub COMI
      result=compare(r2650.r(regnum),inst2)
      setcc(result)
End Sub
' -----

' $E8   COMR,r0 
Sub COMR
      COMA() ' igual
End Sub
' -----

' $EC   COMA,r0 
Sub COMA
      result=compare(r2650.r(regnum),cogebyte(effaddr))
      setcc(result)
      ciclos+=ind
      ret=MEM_READ or effaddr
End Sub
' -----

' $F0   WRTD,r0 
Sub WRTD
      puerto_out=r2650.r(regnum)
      ret=PD_WRITE
End Sub
' -----

' $F4   TMI,r0  
Sub TMI
      result=r2650.r(regnum) and inst2
      r2650.psl And=-1-1*(CC0 or CC1):'clear cc bits
      if result <> inst2 Then
        r2650.psl Or=&h80:'indicate not all selected bits are'1'
      end if
End Sub
' -----

' $F8   BDRR,r0 
Sub BDRR
      BDRA() ' igual
End Sub
' -----

' $FC   BDRA,r0 
Sub BDRA
      r2650.r(regnum)-=1
      if r2650.r(regnum)<>0 Then
        address=effaddr
        ciclos+=ind
      End If
End Sub
' -----

' FIN DE JUEGO DE INSTRUCCIONES









' DEPURADOR
Sub disassemble(address as integer)
   
  dim as ubyte inst1
  dim as ubyte inst2
  dim as ubyte inst3
  dim as integer olddir ' short?
  dim as ubyte numbytes
  
  olddir=address
  inst1=cogebyte(address)
  numbytes=opcode(inst1).bytes
  
  if(numbytes>1) Then
    address +=1
    inst2=cogebyte(address)
    if(numbytes>2) Then
      address +=1
      inst3=cogebyte(address)
    End If
  end if
  
  locate 1,1
    
  ' direccion de CPU y bytes (de 1 a 3)
  Print hex(olddir,4);": ";hex(inst1,2);" ";
  
  if (numbytes>1) Then
    Print hex(inst2,2);" ";
  else
     print "   ";
  end if
  
  if (numbytes>2) Then
    Print hex(inst3,2);" ";
  else
     print "   ";
  end if
  
  ' opcode
  Print tab(17);opcode(inst1).ins;",";
  
  ' valor de registro o direccion
  Select Case (opcode(inst1).formato)
     case F_R, _
          F_A, _
          F_B, _
          F_C, _
          F_EB, _
          F_ER
       Print hex(effaddr,4);
    case F_I :
       Print hex(inst2,2);
    Case Else:
       Print effaddr;
  End Select
  
  Select Case (opcode(inst1).formato)
     case F_R, _
          F_A, _
          F_B, _
          F_C, _
          F_EB, _
          F_ER
         if (inst2 and &h80) <>0 Then
           Print "* ";
         else
           Print "  ";
         end if
    Case Else:
        Print "  ";
  End Select
  
  Select Case (opcode(inst1).formato)
     case F_A, _
          F_C 
      Select Case (inst2 and &h60)
        case &h20 :
          Print "# +";effaddr;
        case &h40 :
          Print "# -";effaddr;
        case &h60 :
          Print "#  ";effaddr;
        Case Else:
          Print "   ";effaddr;
      End Select
    Case Else:
      Print " ";effaddr;
  end select
  
  print "      " ' borramos el final de linea
  
  Print "R0 :";hex(r2650.r(0),2);"  ";
  Print "R1 :";hex(r2650.r(1),2);"  ";
  Print "R2 :";hex(r2650.r(2),2);"  ";
  Print "R3 :";hex(r2650.r(3),2)
  PRINT "        ";
  Print "R1':";hex(r2650.r(4),2);"  ";
  Print "R2':";hex(r2650.r(5),2);"  ";
  Print "R3':";hex(r2650.r(6),2)

  Print "PSU:";hex(r2650.psu,2);"  ";
  Print "PSL:";hex(r2650.psl,2)

  print "+---+---+---+---+---+---+---+---+"
  print "|S  |F  |II |UF1|UF2|SP2|SP1|SP0|"
  print "+---+---+---+---+---+---+---+---+"
  print " ";
  print iif(r2650.psu and SENSE,1,0);"  ";
  print iif(r2650.psu and FLAG,1,0);"  ";
  print iif(r2650.psu and II,1,0);"  ";
  print iif(r2650.psu and UF1,1,0);"  ";
  print iif(r2650.psu and UF2,1,0);"  ";
  print iif(r2650.psu and 4,1,0);"  ";
  print iif(r2650.psu and 2,1,0);"  ";
  print iif(r2650.psu and 1,1,0)
  print "+---+---+---+---+---+---+---+---+"
  print "|CC1|CC0|IDC|RS |WC |OVF|COM|CYF|"
  print "+---+---+---+---+---+---+---+---+"
  print " ";
  print iif(r2650.psl and CC1,1,0);"  ";
  print iif(r2650.psl and CC0,1,0);"  ";
  print iif(r2650.psl and IDC,1,0);"  ";
  print iif(r2650.psl and RS,1,0);"  ";
  print iif(r2650.psl and WC,1,0);"  ";
  print iif(r2650.psl and OVF,1,0);"  ";
  print iif(r2650.psl and COM,1,0);"  ";
  print iif(r2650.psl and CYF,1,0)





  print "---------------------------------"
  print "Ciclos opcode";ciclos;" totales";opciclos
  print "---------------------------------"
  'print "Codigo de retorno:";hex(ret,8)

End Sub



' VARIOS CPU

' LEEMOS LAS INSTRUCCIONES Y SUS CICLOS/BYTES
Sub CALCINS
	Dim ins As String
	Dim bytes As uByte
	Dim formato As uByte
	Dim ciclos As uByte
	
	For f=0 To 255
		Read ins,bytes,formato,ciclos
		opcode(f).ins=ins
		opcode(f).bytes=bytes
		opcode(f).formato=formato
		opcode(f).ciclos=ciclos
	Next
	
End Sub

' inicializa la CPU
Sub Reset2650
  
  For f=0 To 6
  	r2650.r(f)=0
  Next
  
  For f=0 To 7
  	r2650.st(f)=0
  Next
  
  r2650.psl=0
  r2650.psu=0
  
  dircpu=0 ' direccion actual ejecutada por la CPU, de inicio la "0"
End Sub


' calcula las direcciones de acceso a memoria 
Sub calc_addr()

  Dim tmpaddr as short 
  Dim numbytes As Integer

  direxe=dircpu
  
  ind=0
  ret=(NORMAL or direxe)
  address=direxe
  inst1=cogebyte(address)
  numbytes =opcode(inst1).bytes
  ciclos=opcode(inst1).ciclos
  address=((address+1) and &h1fff)  or (address and &h6000)
  
  if(numbytes>1) Then
    inst2=cogebyte(address)
    address=((address+1) and &h1fff)  or (address and &h6000)
    if(numbytes>2) Then
      inst3=cogebyte(address)
      address=((address+1) and &h1fff)  or (address and &h6000)
    End If
  end if
  
  regnum=inst1 and &h03 ' registros R0 al 3
  
  ' si es registro PRIMO R1' a R3'
  if regnum <>0 Then
    if((r2650.psl and RS)<>0) Then
      regnum+=3
    end if
  end if
      
      
  ' obtenemos la direccion que sigue a la instruccion    
  Select Case opcode(inst1).formato
    case F_R  : ' relative
      if((inst2 and &h40)=0) Then
        tmpaddr=((address+(inst2 and &h3f)) and &h1fff)  or (address and &h6000)
      else
        tmpaddr=(address-(((-1-1*inst2) and &h3f)+1))  or (address and &h6000)
      end if
      if((inst2 and &h80)=0) Then
        effaddr=tmpaddr
      else ' indirect
        effaddr=((cogebyte(tmpaddr) shl 8) or cogebyte(((tmpaddr+1) and &h1fff) or (tmpaddr and &h6000))) and &h7fff
        ind=2 ' corregimos los ciclos, sumando 2 ciclos mas
      End If
      '''''''''''''
    case F_A  : ' absolute, non branch instructions
      tmpaddr=(((inst2 shl 8) or inst3) and &h1fff)  or (address and &h6000)
      if((inst2 and &h80)=0) Then
        effaddr=tmpaddr
      else ' indirect
        effaddr=((cogebyte(tmpaddr) shl 8) or cogebyte(((tmpaddr+1) and &h1fff) or (tmpaddr and &h6000))) and &h7fff
        ind=2 ' corregimos los ciclos, sumando 2 ciclos mas
      end if
      if((inst2 and &h60)<>0) Then ' indexed
        if((inst2 and &h60)=&h20) Then
         r2650.r(regnum)+=1
        end if
        if((inst2 and &h60)=&h40) Then
          r2650.r(regnum)-=1
        end if
        effaddr+=r2650.r(regnum)
        effaddr And=&h7fff
        regnum=0
      end if
      ''''''''''''''''
     case F_B, _   ' absolute branch
          F_C, _   ' absolute program
          F_EB     ' absolute indexed
      tmpaddr=((inst2 shl 8) or inst3) and &h7fff
      if((inst2 and &h80)=0) Then
        effaddr=tmpaddr
      else ' indirect
        effaddr=((cogebyte(tmpaddr) shl 8) or cogebyte(((tmpaddr+1) and &h1fff) or (tmpaddr and &h6000))) and &h7fff
        ind=2 ' corregimos los ciclos, sumando 2 ciclos mas
      end if
      if opcode(inst1).formato=F_EB Then
        effaddr=effaddr+r2650.r(regnum) and &h7fff
      end if
      '''''''''''''''''
    case F_ER : ' zero branch
      if((inst2 and &h40)=0) Then
        tmpaddr=inst2 and &h3f
      else
        tmpaddr=(&h2000-(((-1-1*inst2) and &h3f)+1))
      end if
      if((inst2 and &h80)=0) Then
        effaddr=tmpaddr
      else ' indirect
        effaddr=((cogebyte(tmpaddr) shl 8) or cogebyte(((tmpaddr+1) and &h1fff) or (tmpaddr and &h6000))) and &h7fff
        ind=2 ' corregimos los ciclos, sumando 2 ciclos mas
      end if
  end select

end sub


sub run2650()
	
	calc_addr()
	'if address=&h2b1 then dormir=1
	execins() ' ejecuta la INS		
   
   dircpu=address ' address=effaddr direccion efectiva calculada (por ejemplo, un salto)
   'if direxe=&h020e then dormir=1

	if debug then disassemble(direxe)

	opciclos+=ciclos

End sub


