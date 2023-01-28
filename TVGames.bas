screen 19,32,2

dim shared debug as integer=1
dim shared dormir as integer=1 ' para detener o avanzar la depuracion

' rom y ram
dim shared ram(32768) as ubyte ' la cpu 2650 solo direccion 15bits, 32k
dim shared maxrom as integer   ' direccion maxima ocupada por la ROM (se coge del tamaño de fichero rom)
dim shared maxram as integer=((8*1024)-1) ' maxima ram instalada

dim shared dircpu as integer ' direccion actual de la CPU
dim shared direxe as integer ' direccion a ejecutar por la CPU

' puertos I/O
dim shared puerto_in  as integer ' de pruebas aun
dim shared puerto_out as integer ' de pruebas aun
dim shared extio(255) as integer ' de pruebas aun

dim shared opciclos as integer=0 ' ciclos totales ya ejecutados por la CPU
dim shared   ciclos as integer=0 ' ciclos ejecutados en cada INS
dim shared asdd as integer
' variables genericas
dim shared as integer a,b,c,d,e,f,g,h,i
dim shared as string sa,sb,sc,sd
dim shared as integer primeravez=1

#include "S2650_emu.bas"
#include "PVI2636.bas"

' leemos la ROM del monitor del TV GAMES ELEKTOR (2k)
sa=" " ' leemos de byte en byte (es mas rapido los 2048 de golpe, pero mas lioso)
dim nom as string
  nom="monitor.bin"
  'nom="interton\SOCCERB.bin"
  'nom="interton\MONSTERM.bin"
  'nom="Phunsy_Emulator\roms\monitor.bin"
  'nom="interton\pinballb.bin"
  'nom="interton\CARRACES.bin"
  'nom="interton\boxing.bin"
  'nom="interton\METROPOL.bin"
  'nom="SBC2650mon.bin"
open nom for binary access read as 1
  maxrom=lof(1)
   for f=0 to maxrom
      get #1,f+1,sa
      ram(f)=asc(sa)
   Next
close 1

' activar esta linea para cargar cintas
dim elektor as integer=1

dim longcinta as integer
dim iniciocinta as integer
dim inicioprog as integer
dim cinta as string
dim dircinta as string
sb=" "
if elektor then
   ' para cargar una cinta del TVGAMES
   dircinta = "tvc-apps\"
   'cinta = dircinta + "03-1-PlayWithThePVI"+".tvc"
   cinta = dircinta + "03-6-DemonstrationProgram"+".tvc"
   'cinta = dircinta + "07-D-Disassembler"+".tvc"
   'cinta = dircinta + "07-C-PVIProgramming"+".tvc"
   'cinta = dircinta + "07-E-TestPatterns"+".tvc"
   'cinta = dircinta + "09-6-Labyrinth"+".tvc"
   'cinta = dircinta + "10-1x-ChangeWordsForHangman"+".tvc"
   'cinta = dircinta + "07-A-Shapes"+".tvc"
   'cinta = dircinta + "07-F-Lotto"+".tvc"
   'cinta = dircinta + ""+".tvc"
   'cinta = dircinta + ""+".tvc"
   open cinta for binary access read as 1
      longcinta=lof(1)
         get #1,2,sa
         get #1,3,sb                 
      iniciocinta=asc(sa)*256+asc(sb)
         get #1,4,sa
         get #1,5,sb          
      inicioprog=asc(sa)*256+asc(sb)
      for f=6 to longcinta ' quitamos los 5 bytes iniciales del estado , dir de carga y dir de ejecucion (1+2+2)
         get #1,f,sa
         ram(f+iniciocinta-6)=asc(sa)
      Next
   close 1
end if


' cargamos las instrucciones de la CPU
CALCINS()

' inicializamos la CPU
Reset2650()

if elektor then DIRCPU=inicioprog else dircpu=0 ' cintas o monitor elektor

dim as integer ini=&h1f00 ' direccion de inicio de RAM en el TVGAMES ELEKTOR
dim as integer aa,bb
'ram(&h8a2)=&h09
'ram(&h8a3)=&h00
dim shared panta as integer=0
aa=0:bb=0
DEBUG=0
dormir=0
' ejecutamos emulador
while 1=1
 
  run2650()
  
  pvi()
  rast+=1:if rast=350 then rast=0

  ' PVI2636 genera una interrupcion al finalizar VBLANK, y la tratamos aqui
  'if siinterrupt=1 then interrupt():siinterrupt=0
  ' el TV GAMES de ELEKTOR se queda esperando a SENSE en la $71, si no avanza, no dibuja
  primeravez+=1
  if primeravez=280 then primeravez=0:r2650.psu and=-1-1*sense':locate 10,10:print "sense:";rnd(1)  ' activamos SENSE, como si fueramos PVI
  
  ' si activo las IRQ en el ELEKTOR, las cosas fallan
  ' si lo activo en el interton, las cosas funciona???
  'if siinterrupt then siinterrupt=0:interrupt()':locate 20,20:print "inter:";rnd(1) ':primeravez=1

  panta+=1
  if panta>40000 then
  updatescreen()
  panta=0
  end if
  
  if debug then
     ' ponemos el contenido de la ram en HEX
     locate 1,1
     a=0
     for f=0+ini to 255+ini
        if a=0 then print tab (45);hex(f,4);"  ";
        print hex(ram(f),2);
        a+=1
        if a=16 then a=0:print
     Next
     ' lo mismo, pero en ascii
     locate 1,1
     a=0
     for f=0+ini to 255+ini
        if a=0 then print tab (84);
        print iif(ram(f)>31,chr(ram(f)),".");
        a+=1
        if a=16 then a=0:print
     Next
     ini and=&h7fff
     
         
     locate 20,1:print "PUERTO IN :";puerto_in
     locate 21,1:print "PUERTO OUT:";puerto_out
     for f=0 to 14
        print extio(f)
     Next
  
  end if
  ''''''''''''''''''''
 
  sa=inkey

  if sa="." then ini-=16
  if sa="-" then ini+=16
  if sa="/" then interrupt()
  if sa="*" then dormir=iif(dormir=0,1,0)
  
  
'  goto no:
'  locate bb+20,aa+25
'  a=(extio(8))
'  if tengocaracter=2 then if int(rnd(32)>16) then tengocaracter=0
'  if a=17 then extio(8) or=2:extio(9)=0 ':dormir=1' enviamos 19 (XON) listo para recibir
'  if tengocaracter=1 then
'     tengocaracter=0
'     a=(extio(9))
'     sb=chr(a)
'     if (a>31 and a<129) or a=10 or a=13 then
'      if a=10 then   
'         aa=0:bb+=1:if bb=15 then bb=0
'      else
'         print sb
'         aa+=1:if aa=80 then aa=0:bb+=1
'      end if
'      if bb=15 then bb=0:a=0
'     end if
'  end if
' si: 
'  if tengocaracter<>2 then extio(9)=0
'  if sa<>"" then 
'     'extio(8) xor=2 ' ponemos 17 (XOFF)  
'     'ram(&h1003)=asc(sa)  
'     extio(9)=asc(sa)
'    'dormir=1
'     tengocaracter=2 
'  EndIf
'no:
'  
  
  if dormir then sleep
  if sa=chr(27) then end
wend
