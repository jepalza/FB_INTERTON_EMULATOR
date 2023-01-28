' compensacion de posicion de pantalla (depuarcion solo)
dim shared posh as integer
dim shared posv as integer
posh=400
posv=265


' maximo campo de vision dentro del cuadro principal
#define I_XSIZE 227
#define I_YSIZE 252

#define HIDDEN_X 5

' cuadro principal con borde incluido (Interton y Elektor)
#define BOXWIDTH 180
#define BOXHEIGHT 269


#define I_NOISE          &h1E80

#define I_P1LEFTKEYS     &h1E88
#define I_P1MIDDLEKEYS   &h1E89
#define I_P1RIGHTKEYS    &h1E8A
#define I_CONSOLE        &h1E8B
#define I_P2LEFTKEYS     &h1E8C
#define I_P2MIDDLEKEYS   &h1E8D
#define I_P2RIGHTKEYS    &h1E8E

#define I_SPRITE0        &h1F00
#define I_SPRITE0AX      &h1F0A
#define I_SPRITE0BX      &h1F0B
#define I_SPRITE0AY      &h1F0C
#define I_SPRITE0BY      &h1F0D
#define I_SPRITE1        &h1F10
#define I_SPRITE1AX      &h1F1A
#define I_SPRITE1BX      &h1F1B
#define I_SPRITE1AY      &h1F1C
#define I_SPRITE1BY      &h1F1D
#define I_SPRITE2        &h1F20
#define I_SPRITE2AX      &h1F2A
#define I_SPRITE2BX      &h1F2B
#define I_SPRITE2AY      &h1F2C
#define I_SPRITE2BY      &h1F2D
#define I_SPRITE3        &h1F40
#define I_SPRITE3AX      &h1F4A
#define I_SPRITE3BX      &h1F4B
#define I_SPRITE3AY      &h1F4C
#define I_SPRITE3BY      &h1F4D
#define I_VERTGRID       &h1F80
#define I_HORIZ1         &h1FA8
#define I_HORIZ2         &h1FA9
#define I_HORIZ3         &h1FAA
#define I_HORIZ4         &h1FAB
#define I_HORIZ5         &h1FAC
#define I_SIZES          &h1FC0
#define I_SPR01COLOURS   &h1FC1
#define I_SPR23COLOURS   &h1FC2
#define I_SCORECTRL      &h1FC3
#define I_BGCOLOUR       &h1FC6
#define I_PITCH          &h1FC7
#define I_SCORELT        &h1FC8
#define I_SCORERT        &h1FC9
#define I_BGCOLLIDE      &h1FCA
#define I_SPRITECOLLIDE  &h1FCB
#define I_BGCOLLIDE2     &h1FDA
#define I_SPRITECOLLIDE2 &h1FDB
#define I_BGCOLLIDE3     &h1FEA
#define I_SPRITECOLLIDE3 &h1FEB
#define I_BGCOLLIDE4     &h1FFA
#define I_SPRITECOLLIDE4 &h1FFB
#define I_P1PADDLE       &h1FCC
#define I_P2PADDLE       &h1FCD

' colores invertidos
dim shared as UInteger  tintas(1 to 8)={ _
   rgb(012,012,012) , _ ' BLACK
   rgb(012,012,234) , _ ' BLUE
   rgb(012,234,012) , _ ' GREEN
   rgb(012,234,234) , _ ' CYAN
   rgb(234,012,012) , _ ' RED
   rgb(234,012,234) , _ ' PURPLE
   rgb(234,234,012) , _ ' YELLOW
   rgb(234,234,234) }   ' WHITE
   
   'rgb(255,255,255) , _ ' BLACK
   'rgb(255,255,000) , _ ' BLUE
   'rgb(255,000,255) , _ ' GREEN
   'rgb(255,000,000) , _ ' CYAN
   'rgb(000,255,255) , _ ' RED
   'rgb(000,255,000) , _ ' PURPLE
   'rgb(000,000,255) , _ ' YELLOW
   'rgb(000,000,000) }   ' WHITE
   

dim shared as ushort ie_spritedata(3)={ I_SPRITE0,I_SPRITE1,I_SPRITE2,I_SPRITE3 }

type parasprites
    delay as integer
    state as integer
    x as integer
    y as ubyte
    colour as ubyte
end type
dim shared sprite(3) as parasprites

dim shared as ubyte screenmap(BOXWIDTH,BOXHEIGHT) ' almacen de pantalla completa

dim shared as ubyte colltable(I_XSIZE)
Dim Shared as integer fgc ' foreground color
Dim Shared As integer whichsprite, x, y, xx, xs
dim shared as ubyte digitcolour ',OutputBuffer(7)
dim shared as ubyte rumble1,rumble2 ', mmb, lmb, wps
dim shared as ubyte siinterrupt=0
'dim shared as ubyte analog,autofire
dim shared as integer collisions
'dim shared as ulong flagline,paused,swapped,warp
'dim shared as string friendly(2+13+1+1)
'Dim Shared as integer connected
Dim shared as ubyte thesize(3)
dim shared as ubyte spriteimage(3)
Dim Shared As integer _
              cpl,_
              fudge,_
              frameskip,_
              hostcontroller(2),_
              joy1,_
              key1,_
              key2,_
              key3,_
              key4,_
              p1sprite,_
              p2sprite,_
              machine,_
              memmap,_
              offset,_
              overcalc,_
              rast,_
              rastn,_
              recmode,_
              recsize,_
              requirebutton,_
              sensitivity,_
              suppress,_
              VBLANK,_
              consoleopen,_
              crippled,_
              inframe,_
              limitrefresh,_
              loopb,_
              runtoframe,_
              trace
              
dim shared as integer ax(2) 'analog paddle X-coords
dim shared as integer ay(2) 'analog paddle Y-coords
dim shared as UBYTE sx(2),sy(2)
dim shared as ushort jf(2),keypads(2,23)

'dim shared as ULONG downframes,elapsed,frames,totalframes

'dim shared keytable(16) as ubyte 
dim shared display(BOXHEIGHT*BOXWIDTH) as ubyte ' almacen de pantalla
dim shared pens(2,16) as ubyte 
'Dim Shared CatalogPtr As Integer
'Dim Shared colourset As Integer 
'Dim Shared lines As Integer
'Dim Shared source As Integer
'Dim Shared dest As Integer
Dim Shared colour As Integer

Dim shared gridline(0 to 219) As Integer = { _
    20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,  0,0, _
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  2,2, _
    3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  4,4, _
    5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,  6,6, _
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  8,8, _
    9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,10,10, _
    11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,  12,12, _
    13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,  14,14, _
    15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,  16,16, _
    17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,  18,18, _
    19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19 _
  }


Dim Shared led(0 to 19) As string
   led( 0)="aaaabbbbcccc"
   led( 1)="aaaabbbbcccc"
   led( 2)="aaaabbbbcccc"
   led( 3)="aaaabbbbcccc"
   led( 4)="llll    dddd"
   led( 5)="llll    dddd"
   led( 6)="llll    dddd"
   led( 7)="llll    dddd"
   led( 8)="kkkkmmmmeeee"
   led( 9)="kkkkmmmmeeee"
   led(10)="kkkkmmmmeeee"
   led(11)="kkkkmmmmeeee"
   led(12)="jjjj    ffff"
   led(13)="jjjj    ffff"
   led(14)="jjjj    ffff"
   led(15)="jjjj    ffff"
   led(16)="iiiihhhhgggg"
   led(17)="iiiihhhhgggg"
   led(18)="iiiihhhhgggg"
   led(19)="iiiihhhhgggg"


dim shared as integer digit_to_segment(0 to 9)= { _
        &h0fff,_ '0
        &h007c,_ '1
        &h17df,_ '2
        &h15ff,_ '3
        &h1c7d,_ '4
        &h1df7,_ '5
        &h1ff7,_ '6
        &h007f,_ '7
        &h1fff,_ '8
        &h1dff _ '9
        }
'        &h1ff1,//B
'        &h0fc7,//C
'        &h17fc,//D
'        &h1fd7,//E
'        &h1f17//F
'

VBLANK=220
frameskip=50

declare sub pviwrite(address as integer, dato as ubyte)
declare function pviread(address as integer) as ubyte
declare sub ie_drawdigit(x as integer, y as integer, d as integer, lines as integer)
declare sub drawsprites()
declare sub ie_drawline(colour as integer)
declare sub ie_drawpixel(x as integer, y as integer, colour as integer)
declare sub updatescreen()
declare sub drawgrid()
declare sub ie_playerinput(source As Integer,dest as integer)

sub updatescreen()
   
   dim tinta as UInteger 
   dim h as integer

   h=0
   for f=0 to BOXHEIGHT-1
      for g=0 to BOXWIDTH-1
         tinta=(tintas(screenmap(g,f)))
         pset (h+posh,f+posv),tinta  ' dobles pixeles para anchar la pantalla
         pset (h+posh+1,f+posv),tinta
         h+=2
      Next
      h=0
   Next

End Sub

sub pvi()
    dim t as ubyte
  
    'if (opciclos mod frameskip)<>0 then exit sub
        
    'updatescreen()
        
    'inframe=1
    if primeravez=0 then
       primeravez=1
    rumble1=0
    rumble2=0
      
    pviwrite(I_BGCOLLIDE,     0)
    pviwrite(I_SPRITECOLLIDE, 0)
    pviwrite(I_BGCOLLIDE2,    0)
    pviwrite(I_SPRITECOLLIDE2,0)
    pviwrite(I_BGCOLLIDE3,    0)
    pviwrite(I_SPRITECOLLIDE3,0)
    pviwrite(I_BGCOLLIDE4,    0)
    pviwrite(I_SPRITECOLLIDE4,0)

    r2650.psu And=-1-1*(SENSE)
    
      ' si activo esta linea, se autopulsan unas teclas y se arrancan las demos
      ie_playerinput(1,0):ie_playerinput(0,1)
    
    for whichsprite=0 To 3
        sprite(whichsprite).y=pviread(ie_spritedata(whichsprite)+12)
        sprite(whichsprite).y+=1
        sprite(whichsprite).state=0
    next
    end if
  
    ' 0 a 19 borde vacio superior
    ' 20 a 219 zona visible del grid
    ' 220 a 271 borde vacio inferior
    ' 272 a 291 VBLANK inferior
    ' 292 a 311 VBLANK superior
  
    'for rast=0 To 312
        'breakrastline()
        'slice=cpl-overcalc
        'cpu()
        'CHECKINPUT

        for x=0 To I_XSIZE-1
          colltable(x)=0
        next
        
         if (rast<BOXHEIGHT) Then
          drawgrid() ' dibuja el fondo
            if ((rast>=20 and rast<=39) or (rast>=200 and rast<=219)) then
              if(pviread(I_SCORECTRL) and 1) Then
                y=200
              else
                y=20
              End If
              if (rast=y) Then
                if(pviread(I_BGCOLOUR) and 8) Then
                    digitcolour=(pviread(I_BGCOLOUR) shr 4) and 7
                else
                    digitcolour=0
                End If
              end if
              if (rast>=y and rast<(y+20)) Then 
                    ie_drawdigit(    32-HIDDEN_X+28,y,pviread(I_SCORELT) shr 4  ,rast-y)
                    ie_drawdigit(    32-HIDDEN_X+44,y,pviread(I_SCORELT) and &hf,rast-y)
                    if (pviread(I_SCORECTRL) and 2) Then
                        ie_drawdigit(32-HIDDEN_X+60,y,pviread(I_SCORERT) shr 4  ,rast-y)
                        ie_drawdigit(32-HIDDEN_X+76,y,pviread(I_SCORERT) and &hf,rast-y)
                    else
                        ie_drawdigit(32-HIDDEN_X+76,y,pviread(I_SCORERT) shr 4  ,rast-y)
                        ie_drawdigit(32-HIDDEN_X+92,y,pviread(I_SCORERT) and &hf,rast-y)
                    end if
              end if
            end if
         end if
         
        drawsprites()

        if (collisions) Then
            t=0
            for x=9 To I_XSIZE-1
                if((colltable(x) and &h30)=&h30) Then
                    t Or=32
                    if(p1sprite=0 or p1sprite=1) Then rumble1=1
                    if(p2sprite=0 or p2sprite=1) Then rumble2=1
                End If
                if((colltable(x) and &h50)=&h50) Then
                    t Or=16
                    if(p1sprite=0 or p1sprite=2) Then rumble1=1
                    if(p2sprite=0 or p2sprite=2) Then rumble2=1
                end if
                if((colltable(x) and &h90)=&h90) Then
                    t Or=8
                    if(p1sprite=0 or p1sprite=3) Then rumble1=1
                    if(p2sprite=0 or p2sprite=3) Then rumble2=1
                end if
                if((colltable(x) and &h60)=&h60) Then
                    t Or=4
                    if(p1sprite=1 or p1sprite=2) Then rumble1=1
                    if(p2sprite=1 or p2sprite=2) Then rumble2=1
                End If
                if((colltable(x) and &hA0)=&hA0) Then
                    t Or=2
                    if(p1sprite=1 or p1sprite=3) Then rumble1=1
                    if(p2sprite=1 or p2sprite=3) Then rumble2=1
                End If
                if((colltable(x) and &hC0)=&hC0) Then
                    t Or=1
                    if(p1sprite=2 or p1sprite=3) Then rumble1=1
                    if(p2sprite=2 or p2sprite=3) Then rumble2=1
                end if
            next
            
            pviwrite(I_SPRITECOLLIDE, (ram(I_SPRITECOLLIDE ) or t))
            pviwrite(I_SPRITECOLLIDE2,(ram(I_SPRITECOLLIDE2) or t))
            pviwrite(I_SPRITECOLLIDE3,(ram(I_SPRITECOLLIDE3) or t))
            pviwrite(I_SPRITECOLLIDE4,(ram(I_SPRITECOLLIDE4) or t))
            
            if (rast>=20 and rast<=219) Then
               t=0
                for x=0 To I_XSIZE-1
                    if((colltable(x) and &h18)=&h18) Then
                         t Or=128
                         if(p1sprite=0) Then rumble1=1
                         if(p2sprite=0) Then rumble2=1
                    End If
                    if((colltable(x) and &h28)=&h28) Then
                         t Or=64
                         if(p1sprite=1) Then rumble1=1
                         if(p2sprite=1) Then rumble2=1
                    End If
                    if((colltable(x) and &h48)=&h48) Then
                         t Or=32
                         if(p1sprite=2) Then rumble1=1
                         if(p2sprite=2) Then rumble2=1
                    End If
                    if((colltable(x) and &h88)=&h88) Then 
                         t Or=16
                         if(p1sprite=3) Then rumble1=1
                         if(p2sprite=3) Then rumble2=1
                    End If
                next
                pviwrite(I_BGCOLLIDE, (ram(I_BGCOLLIDE ) or t))
                pviwrite(I_BGCOLLIDE2,(ram(I_BGCOLLIDE2) or t))
                pviwrite(I_BGCOLLIDE3,(ram(I_BGCOLLIDE3) or t))
                pviwrite(I_BGCOLLIDE4,(ram(I_BGCOLLIDE4) or t))
            end if
        end if         
                      
        if (rast=VBLANK) Then
            pviwrite(I_SPRITECOLLIDE ,ram(I_SPRITECOLLIDE ) or &h40)
            pviwrite(I_SPRITECOLLIDE2,ram(I_SPRITECOLLIDE2) or &h40)
            pviwrite(I_SPRITECOLLIDE3,ram(I_SPRITECOLLIDE3) or &h40)
            pviwrite(I_SPRITECOLLIDE4,ram(I_SPRITECOLLIDE4) or &h40)
            r2650.psu Or=SENSE
            if (r2650.psu and II)=0 Then siinterrupt=1
            'updatescreen()
        End If     

    'next rast
    
    'frames+=1
    'inframe=0

        'if ((warp=0 or limitrefresh=0) and (opciclos mod frameskip=0)) then updatescreen()
    
    'if siinterrupt then interrupt()
    'if crippled Then uncripple()
    
End sub


sub drawgrid()
   Dim i As Integer
   Dim j As Integer
   Dim k As Integer
   Dim m As Integer
   dim w as integer
   
   'dim flagline as integer
   'flagline=0

   ' if(flagline) Then     
   '   if(pviread(I_BGCOLOUR) and 8) Then 
   '         ie_drawline(15-( pviread(I_BGCOLOUR)        and 7))
   '         fgc=        15-((pviread(I_BGCOLOUR) shr 4) and 7)
   '     else
   '         ie_drawline(15)
   '         exit sub
   '   end if   
   ' else
      if (pviread(I_BGCOLOUR) and 8) Then 
            ie_drawline(7-( pviread(I_BGCOLOUR)        and 7))
            fgc=        7-((pviread(I_BGCOLOUR) shr 4) and 7)
        else
            ie_drawline(7)
            exit sub
      End If
    'end if
    
    'fgc=int(rnd(1)*7)
      
    ' no se dibuja fuera del cuadro central visible (en bordes exteriores)
    if(rast<20 or rast>219) Then exit sub

    if ((pviread(I_NOISE) and &h20) ) then
     if (fgc>=8) Then
          fgc=15-fgc
        else
          fgc=7-fgc
     end if
    end if
              'fgc=7-fgc
              
    i=gridline(rast)
    k=pviread(I_HORIZ1+(i shr 2))
    
    Select Case (k and &hC0)
      case &h40
        w=2
      case &hC0
        w=4
      Case Else
        w=1
    end select
   
    Select Case (i and 3)
       case 0
        if(k and 1) Then w=8
        
       case 1
        if (((rast-20) mod 40)<=10) Then
           if(k and 2) Then w=8
         else
           if(k and 4) Then w=8
        End if
        
       case 2
        if (k and 8) Then w=8
        
       case 3
        if (((rast-20) mod 40)<=30) Then
           if (k and 16) Then w=8
         else
           if (k and 32) Then w=8
        end if       
    end select
   
    x=32
    m=128
    for j=0 to 15
       if (pviread(I_VERTGRID+(i *2)+(j shr 3)) and m) Then
        for xx=x To (x+w) -1
            colltable(xx) Or=8
            ie_drawpixel (xx-HIDDEN_X, rast, fgc)
        next
       end if
       if (j=7) Then m=256
       x+=8
       m = m shr 1
    next

end sub        

sub ie_drawdigit(x as integer, y as integer, d as integer, lines as integer)
    Dim j As Integer
    dim ncar as short

    if (d>=10) Then exit sub
    
    for j=0 To 11 ' 11??
     ncar=(asc(mid(led(lines),j+1,1))-asc("a"))
     if (digit_to_segment(d) and (1 shl ncar)) then
          colltable(x+j-HIDDEN_X) Or=8
          ie_drawpixel(x+j,y+lines,digitcolour)
     end if
    next
    
end sub
           
sub drawsprites()
  dim i as integer
  dim as integer xr,xs
  'dim as integer thesize(4)
  'dim as ubyte spriteimage(4)
  
   asdd=0

    for whichsprite=0 To 3
        Select Case (sprite(whichsprite).state)
           case 0:
            if (rast=sprite(whichsprite).y) Then
                sprite(whichsprite).delay=0
                sprite(whichsprite).state=1
                if(whichsprite=0) Then
                  thesize(0)=1 shl  (pviread(I_SIZES)        and 3)
                end if
                if (whichsprite=1) Then
                  thesize(1)=1 shl ((pviread(I_SIZES) shr 2) and 3)
                end if
                if (whichsprite=2) Then
                  thesize(2)=1 shl ((pviread(I_SIZES) shr 4) and 3)
                end if
                if (whichsprite=3) Then
                  thesize(3)=1 shl ((pviread(I_SIZES) shr 6) and 3)
                end if
                asdd=2
            else
               'continue for    
               asdd=0        
                 
            End If
            
           case 1 to 10:
             if(sprite(whichsprite).delay=0) Then
                spriteimage(whichsprite)=pviread(ie_spritedata(whichsprite)+sprite(whichsprite).state-1)
                if (whichsprite=0) Then
                  sprite(0).colour=((pviread(I_SPR01COLOURS) shr 3) and 7)
                end if
                if (whichsprite=1) Then
                  sprite(1).colour= (pviread(I_SPR01COLOURS)        and 7)
                end if
                if (whichsprite=2) Then
                  sprite(2).colour=((pviread(I_SPR23COLOURS) shr 3) and 7)
                end if
                if (whichsprite=3) Then
                  sprite(3).colour= (pviread(I_SPR23COLOURS)        and 7)
                end if             
             end if
             xs=pviread(ie_spritedata(whichsprite)+10)+1
             if (xs<I_XSIZE) Then
                for i=0 To 7
                  if (spriteimage(whichsprite) and (128 shr i)) Then
                    for xr=0 To thesize(whichsprite)-1
                        if(xs>=I_XSIZE) Then goto done1
                        colltable(xs) Or=(&h10 shl whichsprite) or sprite(whichsprite).colour
                        if (rast<BOXHEIGHT and xs>=HIDDEN_X and xs<(HIDDEN_X+BOXWIDTH) ) then
                             '''''''''''''''''''''''''''''''''''''''''''''''''''''
                              ie_drawpixel (xs-HIDDEN_X,rast,colltable(xs) and 7 )
                             '''''''''''''''''''''''''''''''''''''''''''''''''''''
                        end if
                        xs+=1
                    next
                  else
                        xs+=thesize(whichsprite)
                  end if
                next
             end if
        done1:    
            sprite(whichsprite).delay+=1
            if (sprite(whichsprite).delay>=thesize(whichsprite)) Then
                sprite(whichsprite).delay=0
                sprite(whichsprite).state+=1
                asdd=2
                if(sprite(whichsprite).state=11) Then
                    sprite(whichsprite).y=pviread(ie_spritedata(whichsprite)+13)
                    sprite(whichsprite).y+=1
                    pviwrite(I_BGCOLLIDE ,ram(I_BGCOLLIDE ) or (8 shr whichsprite))
                    pviwrite(I_BGCOLLIDE2,ram(I_BGCOLLIDE2) or (8 shr whichsprite))
                    pviwrite(I_BGCOLLIDE3,ram(I_BGCOLLIDE3) or (8 shr whichsprite))
                    pviwrite(I_BGCOLLIDE4,ram(I_BGCOLLIDE4) or (8 shr whichsprite))
                    if (r2650.psu and II)=0 Then siinterrupt=1
                end if
            end if
            'continue for           
            asdd=0   
            
           case 11:
            if (sprite(whichsprite).y<=252) Then
                sprite(whichsprite).delay=0
                sprite(whichsprite).state=12
                asdd=2
            else
               'continue for          
               asdd=0    
            End If

           case 12:
            if sprite(whichsprite).y=0 and rast<I_YSIZE Then ' revisar
                sprite(whichsprite).state=13
                if (whichsprite=0) Then
                  thesize(0)=1 shl ( pviread(I_SIZES)        and 3)
                end if
                if (whichsprite=1) Then
                  thesize(1)=1 shl ((pviread(I_SIZES) shr 2) and 3)
                end if
                if (whichsprite=2) Then
                  thesize(2)=1 shl ((pviread(I_SIZES) shr 4) and 3)
                end if
                if (whichsprite=3) Then
                  thesize(3)=1 shl ((pviread(I_SIZES) shr 6) and 3)
                end if
                asdd=2
            else
                sprite(whichsprite).y-=1
                asdd=0
                'continue for
            end if

           case 13 to 22:
            if (sprite(whichsprite).delay=0) Then
                spriteimage(whichsprite)=pviread(ie_spritedata(whichsprite)+sprite(whichsprite).state-13)
                if (whichsprite=0) Then
                  sprite(0).colour=((pviread(I_SPR01COLOURS) shr 3) and 7)
                end if
                if (whichsprite=1) Then
                  sprite(1).colour=( pviread(I_SPR01COLOURS)        and 7)
                end if
                if (whichsprite=2) Then
                  sprite(2).colour=((pviread(I_SPR23COLOURS) shr 3) and 7)
                end if
                if (whichsprite=3) Then
                  sprite(3).colour=( pviread(I_SPR23COLOURS)        and 7)
                end if
            end if       
            xs=pviread(ie_spritedata(whichsprite)+11)+1
            if (xs<I_XSIZE) Then
                for i=0 To 7
                  if(spriteimage(whichsprite) and (128 shr i)) Then
                    for xr=0 To thesize(whichsprite)-1
                      if(xs>=I_XSIZE) Then goto done2
                      colltable(xs) Or=(&h10 shl whichsprite) or sprite(whichsprite).colour
                      if (rast<BOXHEIGHT and xs>=HIDDEN_X and xs<HIDDEN_X+BOXWIDTH) then
                        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                         ie_drawpixel (xs-HIDDEN_X, rast, colltable(xs) and 7 )
                        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                      end if
                      xs+=1
                    next
                  else
                      xs+=thesize(whichsprite)
                  end if
                next
            end if
       done2:
            sprite(whichsprite).delay+=1
            if (sprite(whichsprite).delay>=thesize(whichsprite)) Then
                sprite(whichsprite).delay =0
                sprite(whichsprite).state+=1
                if(sprite(whichsprite).state=23) Then
                    sprite(whichsprite).y=pviread(ie_spritedata(whichsprite)+13)
                    sprite(whichsprite).y+=1
                    pviwrite(I_BGCOLLIDE ,ram(I_BGCOLLIDE ) or (8 shr whichsprite))
                    pviwrite(I_BGCOLLIDE2,ram(I_BGCOLLIDE2) or (8 shr whichsprite))
                    pviwrite(I_BGCOLLIDE3,ram(I_BGCOLLIDE3) or (8 shr whichsprite))
                    pviwrite(I_BGCOLLIDE4,ram(I_BGCOLLIDE4) or (8 shr whichsprite))
                    if (r2650.psu and II)=0 Then siinterrupt=1
                    sprite(whichsprite).state=11
                    'asdd=2
                end if
            end if
         case else:   
            asdd=0
        end select
        if asdd=2 then whichsprite-=1:asdd=0
        'if sprite(whichsprite).state=11 then whichsprite-=1
    next
    
end sub
       


sub pviwrite(address as integer, dato as ubyte)
   'if address<&h1f00 then print "error pvi write:";hex(address,4):sleep
   ponbyte(address,dato)
end sub


function pviread(address as integer) as ubyte
   'if address<&h1f00 then print "error pvi read:"; hex(address,4):sleep
   return cogebyte(address)
end function   
 

' de aqui a abajo ya esta
sub ie_drawpixel(xm as integer, ym as integer, colour as integer)
    if (screenmap(xm,ym)=colour) Then exit sub

    screenmap(xm,ym)=colour
    
    'if (suppress and x<10) Then
    '   display (x+(y * BOXWIDTH))=pens(colourset,screenmap(10,y))
    'else
    '   display (x+(y * BOXWIDTH))=pens(colourset,colour)
    'end if
End sub


Sub ie_drawline(colour as integer)
    dim xg as integer

    'if (colour>=8) Then
    '    colour=15-colour '0..7
    '  else
    '    colour= 7-colour '0..7
    'end if
    'if colour>7 then print "error en colores":sleep
    
    for xg=0 To BOXWIDTH-1
      screenmap(xg,rast)=colour
    next
    
    'fin=(rast+1)*BOXWIDTH
    
    'for x=rast*BOXWIDTH To fin-1
    '  display(x)=pens(colourset,colour)
    'next
    
End Sub

function keydown(tecla as ubyte) as ubyte
   return int(rnd(1)*256)
End Function

sub ie_playerinput(source As Integer,dest as integer)
   dim as ushort jg
   dim as ubyte t
  
        'if ((dest=0 and connected=NET_CLIENT) or (dest=1 and connected=NET_SERVER) ) then exit sub
        
        'if     (hostcontroller(source)=CONTROLLER_1STJOY) Then 
        '    jg=jf(0)
        'elseif (hostcontroller(source)=CONTROLLER_2NDJOY) Then
        '    jg=jf(1)
        'else
        '    jg=0
        'end if
        
        ponbyte(I_P1LEFTKEYS  +(dest *4),&h0F)
        ponbyte(I_P1MIDDLEKEYS+(dest *4),&h0F)
        ponbyte(I_P1RIGHTKEYS +(dest *4),&h0F)
        
        'if (autofire) Then
        '   if ((!requirebutton or (jg  and JOYFIRE1) or KeyDown(keypads(source,0)) or (hostcontroller(source)=CONTROLLER_TRACKBALL and lmb )) and ((frames%totalframes)<downframes) )
        '       '1st firebutton
        '        ponbyte(keytable(key1).ie_address+(dest *4)) Or=keytable(key1).ie_mask
        '   end if
        '   if ((jg and JOYFIRE1) or KeyDown(keypads(source,0)) or (hostcontroller(source)=CONTROLLER_TRACKBALL and lmb)) then
        '       '1st firebutton
        '        ponbyte(keytable(key1).ie_address+(dest *4)) Or=keytable(key1).ie_mask
        '   end if
        '   if ((jg and JOYFIRE2) or (KeyDown(keypads(source,21))) or (hostcontroller(source)=CONTROLLER_TRACKBALL  and mmb)) then
        '       '2nd firebutton
        '       ponbyte(keytable(key2).ie_address+(dest *4)) Or=keytable(key2).ie_mask
        '   end if
        '   if ((jg  and JOYFIRE3) or (KeyDown(keypads(source,22)))) then
        '       '3rd firebutton
        '       ponbyte(keytable(key3).ie_address+(dest *4)) Or=keytable(key3).ie_mask
        '   end if
        '   if ((jg  and JOYFIRE4) or (KeyDown(keypads(source,23)))) then
        '      '4th firebutton
        '       ponbyte(keytable(key4).ie_address+(dest *4)) Or=keytable(key4).ie_mask
        '   end if
        'end if
        
        'left column
        t=cogebyte(I_P1LEFTKEYS+(dest *4))
        t Or=(&h80*KeyDown(keypads(source, 1))):'"1"  key(Interton) or "RCAS"  key(Elektor)
        t Or=(&h40*KeyDown(keypads(source, 4))):'"4"  key(Interton) or "BP1/2" key(Elektor)
        t Or=(&h20*KeyDown(keypads(source, 7))):'"7"  key(Interton) or "PC"    key(Elektor)
        t Or=(&h10*KeyDown(keypads(source,10))):'"Cl" key(Interton) or "-"     key(Elektor)
        pviwrite((I_P1LEFTKEYS+(dest *4)),t)
        
        'middle column
        t=cogebyte(I_P1MIDDLEKEYS+(dest *4))
        t Or=(&h80*KeyDown(keypads(source, 2))):'"2" key(Interton) or "WCAS" key(Elektor)
        t Or=(&h40*KeyDown(keypads(source, 5))):'"5" key(Interton) or "REG"  key(Elektor)
        t Or=(&h20*KeyDown(keypads(source, 8))):'"8" key(Interton) or "MEM"  key(Elektor)
        t Or=(&h10*KeyDown(keypads(source,11))):'"0" key(Interton) or "+"    key(Elektor)
        pviwrite((I_P1MIDDLEKEYS+(dest *4)),t)
        
        'right column
        t=cogebyte(I_P1RIGHTKEYS+(dest *4))
        t Or=(&h80*KeyDown(keypads(source, 3))):'"3"  key(Interton) or "C" key(Elektor)
        t Or=(&h40*KeyDown(keypads(source, 6))):'"6"  key(Interton) or "8" key(Elektor)
        t Or=(&h20*KeyDown(keypads(source, 9))):'"9"  key(Interton) or "4" key(Elektor)
        t Or=(&h10*KeyDown(keypads(source,12))):'"En" key(Interton) or "0" key(Elektor)
        pviwrite((I_P1RIGHTKEYS+(dest *4)),t)
        
        'engine_dopaddle(source,dest)
        
        if (r2650.psu and FLAG) Then 
            pviwrite((I_P1PADDLE+dest),ay(dest))
            sy(dest)=ay(dest)
        else
            pviwrite((I_P1PADDLE+dest),ax(dest))
            sx(dest)=ax(dest)
        end if
            
end sub           
            
            
            
            
sub ie_emuinput()
    dim as UBYTE therecv(3), thesend(3)
    
    'readkybd()
    '
    'ReadJoystick(0)
    'ReadJoystick(1)
    
    'must always do ie_playerinput(foo,0)then ie_playerinput(foo,1).
    'if(swapped) Then
    '    ie_playerinput(1,0)
    '    ie_playerinput(0,1)
    'else
    '    ie_playerinput(0,0)
    '    ie_playerinput(1,1)
    'end if
    
    'if(recmode not(RECMODE_PLAY) Then
    'if(KeyDown(console(0)) or (jf(0) and JOYSTART) or (jf(1) and JOYSTART)) Then 'START/START
    '    t=&h4F
    '    else
    '    t=&h0F
    '    if(KeyDown(console(1)) or (jf(0) and JOYA) or (jf(1) and JOYA)) Then 'SELECT/UC
    '    t Or=&h80
    '    if(machine=ELEKTOR) Then
    '    if(KeyDown(console(2)) or (jf(0) and JOYB) or (jf(1) and JOYB)) Then '-/LC
    '        t Or=&h20
    '        if(KeyDown(console(3))) Then '-/RESET(soft)
    '        t Or=&h10
    '    pviwrite(I_CONSOLE,t)
    '    domouse()
    'if(connected=NET_SERVER) Then
    ''we are left player
    '    thesend(0)=((ponbyte(I_P1LEFTKEYS)  and &h0F) shl 4) or (ponbyte(I_P1MIDDLEKEYS) and &h0F):'bits 3..0.bits 3..0(masking not really needed)
    '    thesend(1)= (ponbyte(I_P1RIGHTKEYS) and &h0F) or (ponbyte(I_CONSOLE) and &hF0):'bits 7..4.bits 7..4(masking not really needed)
    '    thesend(2)=  ponbyte(I_P1PADDLE):'bits 7..0.bits 7..0
'Dim *)&therecv As Byte
'Dim (char *)&thesend As Byte
'Dim 3)) As Byte
    '    pviwrite(I_P2LEFTKEYS,(UBYTE)(((therecv(0) and &hF0) shr 4) or &h0F)):'bits 7..4.bits 3..0
    '        pviwrite(I_P2MIDDLEKEYS,(UBYTE)((therecv(0) and &h0F) or &h0F)):'bits 3..0.bits 3..0
    '        pviwrite(I_P2RIGHTKEYS,(UBYTE)((therecv(1) and &h0F) or &h0F)):'bits 3..0.bits 3..0(masking not really needed)
    '        pviwrite(I_P2PADDLE,therecv(2)):'bits 7..0.bits 7..0
    '        pviwrite(I_CONSOLE,(UBYTE)((therecv(1) and &hF0) or ponbyte(I_CONSOLE)))
    'elif(connected=NET_CLIENT) Then
    'we are right player
'        thesend(0)=((ponbyte(I_P2LEFTKEYS) and &h0F) shl 4)'bits 3..0.bits 7..4(masking not really needed)
'                    or (ponbyte(I_P2MIDDLEKEYS) and &h0F):'bits 3..0.bits 3..0(masking not really needed)
'        thesend(1)=(ponbyte(I_P2RIGHTKEYS) and &h0F)'bits 3..0.bits 3..0(masking not really needed)
'                    or (ponbyte(I_CONSOLE) and &hF0):'bits 7..4.bits 7..4(masking not really needed)
'        thesend(2)=ponbyte(I_P2PADDLE):'bits 7..0.bits 7..0
'Dim *)&therecv As Byte
'Dim (char *)&thesend As Byte
'Dim 3)) As Byte
'        pviwrite(I_P1LEFTKEYS,(UBYTE)(((therecv(0) and &hF0) shr 4) or &h0F)):'bits 7..4.bits 3..0
'            pviwrite(I_P1MIDDLEKEYS,(UBYTE)((therecv(0) and &h0F) or &h0F)):'bits 3..0.bits 3..0
'            pviwrite(I_P1RIGHTKEYS,(UBYTE)((therecv(1) and &h0F) or &h0F)):'bits 3..0.bits 3..0(masking not really needed)
'            pviwrite(I_P1PADDLE,therecv(2)):'bits 7..0.bits 7..0
'            pviwrite(I_CONSOLE,(UBYTE)((therecv(1) and &hF0) or ponbyte(I_CONSOLE)))
'    if(recmode=RECMODE_PLAY) Then
'    pviwrite(I_CONSOLE,IOBuffer(offset+=1))
'    e:End Ifelif(recmode=RECMODE_RECORD) Then
    
        'OutputBuffer(0)=ponbyte(I_P1PADDLE)
        'OutputBuffer(1)=(ponbyte(I_P1LEFTKEYS) and &hF0) or ((ponbyte(I_P1MIDDLEKEYS) and &hF0) shr 4):'bits 7..4.bits 3..0(masking not really needed)
        'OutputBuffer(2)=ponbyte(I_P1RIGHTKEYS):'4bits are wasted
        'OutputBuffer(3)=ponbyte(I_P2PADDLE)
        'OutputBuffer(4)=(ponbyte(I_P2LEFTKEYS) and &hF0) or ((ponbyte(I_P2MIDDLEKEYS) and &hF0) shr 4):'bits 7..4.bits 3..0(masking not really needed)
        'OutputBuffer(5)=ponbyte(I_P2RIGHTKEYS
        'OutputBuffer(6)=ponbyte(I_CONSOLE)
        '
        'DISCARD fwrite(OutputBuffer,7,1,MacroHandle)
        
'        7bytes per frame *50frames per second=350bytes/sec.
'        Plus,for Elektor,1byte every time the E_RANDOMn hardware
'        registers are read.

end sub
