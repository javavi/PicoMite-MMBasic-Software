'File Manager v.1.13 for PicoMiteVGA by JAVAVI (c)2024"
'For start FM from flash memory enter this instructions
'> FLASH SAVE 2
'> Option F9 "FLASH RUN 2"+Chr$(13)
Clear
Option default integer
MODE 1: Font 1: TILE Height 12
'----------------------
Const SStimeout=30000
Const FW=MM.Info(FONTWIDTH): FH=MM.Info(FONTHEIGHT)
Const CHR=MM.HRes\FW: CVR=MM.VRes\FH
Const RMax=200
Dim string PSide$="L"     LENGTH 1
Dim string LSort$=">"     LENGTH 1
Dim string RSort$=">"     LENGTH 1
Dim string LDisk$="A:"    LENGTH 2
Dim string RDisk$="A:"    LENGTH 2
Dim string LDir$="/"      LENGTH 64
Dim string RDir$="/"      LENGTH 64
Dim string LFList$(RMax)  LENGTH 64
Dim string RFList$(RMax)  LENGTH 64
Dim integer LFLS%(2),RFLS%(2) 'File List Struct (RQ,DQ,FQ)
Dim integer LFLIndx,RFLIndx
Dim integer LPPos,RPPos,LFLTop,RFLTop
Dim integer CKey,Tick1s
Dim integer c(15)=(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Colour Map c(),c()
MENU_ITEMS:
Data "> MEMORY"
Data "> FLASH LIST"
Data "> OPTION LIST"
Data "> ? Time$"
Data "> ? Date$"
Data ""
'============================
BEGIN:
Timer =0
SetTick 1000,ISR_Tick1s,1
On ERROR SKIP 1
If MM.Info(SDCARD)="Ready" Then RDisk$="B:"
On ERROR CLEAR
PanelsReStart()
'----------------------
Do
CKey=Asc(Inkey$)
If CKey<>0 Then
Select Case CKey
Case 9    'Tap
  SetPControl("TAB")
Case 13   'Enter
  EnterControl()
Case 27   'Escape
  CLS : End
Case 42   'PrintScr
  Save image "PrScr"
Case 128  'Up
  SetPControl("UP")
Case 129  'Dn
  SetPControl("DWN")
Case 130  'Left
  SetPControl("PREV")
Case 131  'Right
  SetPControl("NEXT")
Case 134  'Home
  SetPControl("TOP")
Case 135  'End
  SetPControl("BOT")
Case 136  'PgUp
  SetPControl("PREV")
Case 137  'PgDn
  SetPControl("NEXT")
Case 139,8'Alt or BS
  SetAltPControl()
Case 145  'F1-Help
  SetPControl("DIS")
  PrintHelp()
  PanelsReStore()
Case 146  'F2-ReName
  SetPControl("DIS")
  W_F2_ReNAME()
  PanelsReStart()
Case 147  'F3-List
  SetPControl("DIS")
  CLS
  On ERROR IGNORE
  List GetCurrFullName$(PSide$)
  On ERROR ABORT
  Do :Loop While Inkey$=""
  PanelsReStore()
Case 148  'F4-Edit
  SetPControl("DIS")
  W_F4_Edit()
  PanelsReStart()
Case 149  'F5-Copy
  SetPControl("DIS")
  W_F5_Copy()
  PanelsReStart()
Case 150  'F6-Move
  SetPControl("DIS")
  W_F6_Move()
  PanelsReStart()
Case 151  'F7-MkDir
  SetPControl("DIS")
  W_F7_MkDIR()
  PanelsReStart()
Case 152  'F8-Delete
  SetPControl("DIS")
  W_F8_DELETE()
  PanelsReStart()
Case 153  'F9-Menu
  W_F9_MENU()
Case Else
  'Print @(0,456)CKey;"  ";
End Select
Timer =0
EndIf
'----------------------
If Tick1s Then PrintTime()
If Timer>SStimeout Then
  SSMatrix()
  CLS :Timer =0
  PanelsReStore()
EndIf
Loop
'ENTER KEY CONTROL ==========
Sub EnterControl()
Local string TEMP$
If PSide$="L" Then TEMP$=LFList$(LFLIndx) Else TEMP$=RFList$(RFLIndx)
'Action with files
If Instr(LCase$(TEMP$),".bas") Then Run TEMP$
If Instr(LCase$(TEMP$),".bmp") Then
  CLS : Load image TEMP$
  Do : Loop While Inkey$=""
  PanelsReStart():SetPControl("ENA")
  End Sub
EndIf
If Instr(LCase$(TEMP$),".mod") Then
  Play modfile TEMP$
  Do : Loop While Inkey$=""
  Play Stop
  End Sub
EndIf
If Instr(LCase$(TEMP$),".mp3") Then
  On ERROR ignore
  Play mp3 TEMP$
  W_ERROR_MSG()
  Do : Loop While Inkey$=""
  Play Stop
  End Sub
EndIf
'Execute Commands from MENU
If Mid$(TEMP$,1,2)="> " Then
  TEMP$=Mid$(TEMP$,3,Len(TEMP$)-2)
  CLS : Execute TEMP$
  Do : Loop While Inkey$=""
  PanelsReStart():SetPControl("ENA")
  End Sub
EndIf
'Run Programm from MENU
If Mid$(TEMP$,1,2)="* " Then
  Run TEMP$=Mid$(TEMP$,3,Len(TEMP$)-2)
EndIf
'Change Directory
If Mid$(TEMP$,1,1)="/"  Then Chdir Mid$(TEMP$,2,63)
If Mid$(TEMP$,1,2)=".." Then Chdir ".."
If PSide$="L" Then
  LDir$=Mid$(Cwd$,3,Len(Cwd$)-2)+"/"
  LPanelShow()
Else
  RDir$=Mid$(Cwd$,3,Len(Cwd$)-2)+"/"
  RPanelShow()
EndIf
SetPControl("ENA")
End Sub
'================================
Sub SetPControl(CTRL$)
Local STRING COM$
Local SIZE
If PSide$="L" Then
'---- Left Panel Control --------
Select Case CTRL$
Case "TAB"
  PSide$="R"
  SetPControl("DIS")
3  SetPControl("ENA")
Case "UP"
  TILE 1,1+LPPos,c(15),c(0),38,1
  If LFLIndx>0 Then
    Inc LFLIndx,-1
    If LPPos>0 Then
      Inc LPPos,-1
    Else
      If LFLTop>0 Then Inc LFLTop,-1
      FLPrint(LFList$(),LFLTop,1,1,38,32)
    EndIf
  EndIf
  TILE 1,1+LPPos,c(0),c(15),38,1
Case "DWN"
  TILE 1,1+LPPos,c(15),c(0),38,1
  If LFLIndx<LFLS%(0) Then
    Inc LFLIndx
    If LPPos<32 Then
      Inc LPPos
    Else
      Inc LFLTop
      FLPrint(LFList$(),LFLTop,1,1,38,32)
    EndIf
  EndIf
  TILE 1,1+LPPos,c(0),c(15),38,1
Case "TOP"
  TILE 1,1+LPPos,c(15),c(0),38,1
  LFLIndx=0:LFLTop=0:LPPos=0
  FLPrint(LFList$(),LFLTop,1,1,38,32)
  TILE 1,1+LPPos,c(0),c(15),38,1
Case "BOT"
  TILE 1,1+LPPos,c(15),c(0),38,1
  LFLIndx=LFLS%(0)
  If LFLS%(0)>32 Then
    LPPos=32:LFLTop=LFLS%(0)-32
  Else
    LPPos=LFLS%(0)-LFLTop
  EndIf
  FLPrint(LFList$(),LFLTop,1,1,38,32)
  TILE 1,1+LPPos,c(0),c(15),38,1
Case "PREV"
  TILE 1,1+LPPos,c(15),c(0),38,1
  If LFLIndx>=32  And LFLTop>=32 Then
    Inc LFLTop,-32:Inc LFLIndx,-32
  Else
    LFLIndx=0:LFLTop=0:LPPos=0
  EndIf
  FLPrint(LFList$(),LFLTop,1,1,38,32)
  TILE 1,1+LPPos,c(0),c(15),38,1
Case "NEXT"
  TILE 1,1+LPPos,c(15),c(0),38,1
  If (LFLIndx-LPPos)+32<LFLS%(0) Then
    Inc LFLTop,32:Inc LFLIndx,32
    If LFLTop+LPPos>LFLS%(0) Then LFLIndx=LFLS%(0):LPPos=LFLIndx-LFLTop
  Else
    LFLIndx=LFLS%(0):LPPos=LFLS%(0)-LFLTop
  EndIf
  FLPrint(LFList$(),LFLTop,1,1,38,32)
  TILE 1,1+LPPos,c(0),c(15),38,1
End Select
'---- Right Panel Control -------
Else
Select Case CTRL$
Case "TAB"
  PSide$="L"
  SetPControl("DIS")
  SetPControl("ENA")
Case "UP"
  TILE 41,1+RPPos,c(15),c(0),38,1
  If RFLIndx>0 Then
    Inc RFLIndx,-1
    If RPPos>0 Then
      Inc RPPos,-1
    Else
      If RFLTop>0 Then Inc RFLTop,-1
      FLPrint(RFList$(),RFLTop,41,1,38,32)
    EndIf
  EndIf
  TILE 41,1+RPPos,c(0),c(15),38,1
Case "DWN"
  TILE 41,1+RPPos,c(15),c(0),38,1
  If RFLIndx<RFLS%(0) Then
    Inc RFLIndx
    If RPPos<32 Then
      Inc RPPos
    Else
      Inc RFLTop
      FLPrint(RFList$(),RFLTop,41,1,38,32)
    EndIf
  EndIf
  TILE 41,1+RPPos,c(0),c(15),38,1
Case "TOP"
  TILE 41,1+RPPos,c(15),c(0),38,1
  RFLIndx=0:RFLTop=0:RPPos=0
  FLPrint(RFList$(),RFLTop,41,1,38,32)
  TILE 41,1+RPPos,c(0),c(15),38,1
Case "BOT"
  TILE 41,1+RPPos,c(15),c(0),38,1
  RFLIndx=RFLS%(0)
  If RFLS%(0)>32 Then
    RPPos=32:RFLTop=RFLS%(0)-32
  Else
    RPPos=RFLS%(0)-RFLTop
  EndIf
  FLPrint(RFList$(),RFLTop,41,1,38,32)
  TILE 41,1+RPPos,c(0),c(15),38,1
Case "PREV"
  TILE 41,1+RPPos,c(15),c(0),38,1
  If RFLIndx>=32 And RFLTop>=32 Then
    Inc RFLTop,-32:Inc RFLIndx,-32
  Else
    RFLIndx=0:RFLTop=0:RPPos=0
  EndIf
  FLPrint(RFList$(),RFLTop,41,1,38,32)
  TILE 41,1+RPPos,c(0),c(15),38,1
Case "NEXT"
  TILE 41,1+RPPos,c(15),c(0),38,1
  If (RFLIndx-RPPos)+32<RFLS%(0) Then
    Inc RFLTop,32:Inc RFLIndx,32
    If RFLTop+RPPos>RFLS%(0) Then RFLIndx=RFLS%(0):RPPos=RFLIndx-RFLTop
  Else
    RFLIndx=RFLS%(0):RPPos=RFLS%(0)-RFLTop
  EndIf
  FLPrint(RFList$(),RFLTop,41,1,38,32)
  TILE 41,1+RPPos,c(0),c(15),38,1
End Select
EndIf
'---- Common Actions --------
Select Case CTRL$
Case "DIS"
  TILE  1,1+LPPos,c(15),c(0),38,1
  TILE 41,1+RPPos,c(15),c(0),38,1
Case "ENA"
  If PSide$="L" Then
    Drive LDisk$
    TILE  1,1+LPPos,c(0),c(15),38,1
  Else
    Drive RDisk$
    TILE 41,1+RPPos,c(0),c(15),38,1
  EndIf
End Select
'Print Command Line ---------
If PSide$="L" Then
  COM$=LDisk$+LDir$+">"
  Print @(0,456);COM$+Space$(80-Len(COM$))
Else
  COM$=RDisk$+RDir$+">"
  Print @(0,456);COM$+Space$(80-Len(COM$))
EndIf
'Print Panels Info ----------
If PSide$="L" Then
  COM$=LFList$(LFLIndx)
  If Len(COM$)<39 Then
    COM$=Mid$(COM$,1,38)
    COM$=COM$+Space$(38-Len(COM$))
  Else
    COM$=Mid$(COM$,1,37)+"}"
  EndIf
  Print @(8,420);COM$;
  Print @(8,432);Space$(38);
  COM$=LFList$(LFLIndx)
  SIZE=MM.Info(FILESIZE COM$)
  Print @(8,432);
  If SIZE>0 Then Print "Size:";SIZE Else Print "<DIR>"
  Print @(160,432);MM.Info(MODIFIED COM$)
Else
  COM$=RFList$(RFLIndx)
  If Len(COM$)<39 Then
    COM$=Mid$(COM$,1,38)
    COM$=COM$+Space$(38-Len(COM$))
  Else
    COM$=Mid$(COM$,1,37)+"}"
  EndIf
  Print @(328,420);COM$;
  Print @(328,432);Space$(38)
  COM$=RFList$(RFLIndx)
  SIZE=MM.Info(FILESIZE COM$)
  Print @(328,432);
  If SIZE>0 Then Print "Size:";SIZE Else Print "<DIR>"
  Print @(480,432);MM.Info(MODIFIED COM$)
EndIf
End Sub
'ALT PANEL FUNCTIONS --------
Sub  SetAltPControl()
PrintAltFMenu()
Do :CKey=Asc(Inkey$):Loop While CKey=0
Select Case CKey
Case 145    'F1 L.Disk
  SetPControl("DIS")
  LDisk$=WDiskSelect(2,15)
  LPanelShow():SetPControl("ENA")
Case 146    'F2 R.Disk
  SetPControl("DIS")
  RDisk$=WDiskSelect(42,15)
  RPanelShow():SetPControl("ENA")
Case 147    'F3 L.Sort
  SetPControl("DIS")
  LSort$=SortSwitch$(LSort$)
  LPanelShow():SetPControl("ENA")
Case 148    'F4 R.Sort
  SetPControl("DIS")
  RSort$=SortSwitch$(RSort$)
  RPanelShow():SetPControl("ENA")
End Select
PrintFMenu()
End Sub
'FUNCTIONS_KEY_SUBRUTINES ===
Function WDiskSelect(x,y) As string
Local TEMP$
Local N,P
  OpenWindow("DISK:",x%,y%,35,5)
  Drive "A:":Inc N
  x=x+1
  y=y+1:Print @(x*8,y*12)"   Drive   |   Size   |   Free    "
  y=y+1:Print @(x*8,y*12)"-----------|----------|-----------"
  y=y+1:Print @(x*8,y*12)" A: FlashFS|";
  TEMP$=Str$(MM.Info(DISK SIZE)\1024)+"|":Print Space$(11-Len(TEMP$));TEMP$;
  TEMP$=Str$(MM.Info(FREE SPACE)\1024):Print Space$(10-Len(TEMP$));TEMP$;
  y=y+1
On ERROR SKIP 1
If MM.Info(SDCARD)="Ready" Then Inc N
On ERROR CLEAR
If N>1 Then
  Drive "B:"
  Print @(x*8,y*12)" B: SD Card|";
  TEMP$=Str$(MM.Info(DISK SIZE)\1024)+"|":Print Space$(11-Len(TEMP$));TEMP$;
  TEMP$=Str$(MM.Info(FREE SPACE)\1024):Print Space$(10-Len(TEMP$));TEMP$;
Else
  Print @(x*8,y*12)" B: SD Card|   NO Disk Drive !    ";
EndIf
Do
  If P=0 Then
    TILE x,y-1,c(0),c(7),34,1
    TILE x,y,c(15),c(0),34,1
    TEMP$="A:"
  Else
    TILE x,y-1,c(15),c(0),34,1
    TILE x,y,c(0),c(7),34,1
    TEMP$="B:"
  EndIf
  Do :CKey=Asc(Inkey$):Loop While CKey=0
  If N>1 Then P=P Xor 1
Loop Until CKey=13
TILE x,y-1,c(15),c(0),34,1
TILE x,y,c(15),c(0),34,1
WDiskSelect=TEMP$
End Function
'----------------------
Function SortSwitch$(SS$)
  Select Case SS$
  Case ">"
    SortSwitch$="<"
  Case "<"
    SortSwitch$="*"
  Case "*"
    SortSwitch$=">"
  End Select
End Function
'=======================
Sub W_F2_ReNAME()
Local string TEMP$
  TEMP$=GetCurrName$(PSide$)
  If Mid$(TEMP$,1,2)=".." Then End Sub
  Color c(15),c(1)
  OpenWindow("ReName:",4,14,71,7)
  Chdir GetCurrFullPath$(PSide$)
  Print @(48,192);GetCurrFullPath$(PSide$);
  Print @(48,216);"AS";
  Print @(64,228);">";
  Color c(15),c(0):Print @(72,204);Space$(64);
  Print @(72,204);GetCurrName$(PSide$);
  Color c(14),c(0):Print @(72,228);Space$(64);
  Print @(72,228);
  If Mid$(TEMP$,1,1)="/" Then
    TEMP$=InputE$(TEMP$,64)
    If TEMP$<>"" Then
      If Mid$(TEMP$,1,1)="/" Then TEMP$=Mid$(TEMP$,2,63)
      On ERROR ignore
      Rename Mid$(GetCurrName$(PSide$),2,63) As TEMP$
    EndIf
  Else
    TEMP$=InputE$(TEMP$,64)
    If TEMP$<>"" Then
      On ERROR ignore
      Rename GetCurrName$(PSide$) As TEMP$
    EndIf
  EndIf
  W_ERROR_MSG()
  Color c(15)
End Sub
'----------------------
Sub W_F4_Edit()
Select Case Mid$(GetCurrName$(PSide$),1,1)
Case "."
  Local string TEMP$
  Color c(15),c(1)
  OpenWindow("EDIT New File",4,14,71,6)
  Print @(48,192);GetCurrFullPath$(PSide$);
  Print @(64,216);">"
  Color c(14),c(0):Print @(72,216);Space$(64);
  Print @(72,216);:TEMP$=InputE$("",64)
  Color c(15),c(0)
  If TEMP$<>"" Then Edit File TEMP$
Case "/"
  End Sub
Case Else
  Edit File GetCurrName$(PSide$)
End Select
End Sub
'----------------------
Sub W_F5_Copy()
Local string TEMP$
  Color c(15),c(1)
  OpenWindow("Copy a file:",4,14,71,10)
  Chdir GetCurrFullPath$(PSide$)
  Print @(48,192);GetCurrFullPath$(PSide$);
  Print @(48,216);"to";
  Print @(48,240);GetCurrFullPath$(OpSide$());
  Print @(64,252);">";
  Color c(15),c(0):Print @(72,204);Space$(64);
  Print @(72,204);GetCurrName$(PSide$);
  Color c(14),c(0):Print @(72,252);Space$(64);
  Print @(72,252);:TEMP$=InputE$(GetCurrName$(PSide$),64)
  Color c(15),c(0)
  If TEMP$<>"" Then
    TEMP$=GetCurrFullPath$(OpSide$())+TEMP$
  On ERROR ignore
  Copy GetCurrFullName$(PSide$) To TEMP$
  W_ERROR_MSG()
  EndIf
End Sub
'----------------------
Sub W_F6_MOVE()
Local string TEMP$
  Color c(15),c(1)
  OpenWindow("Move:",4,14,71,9)
  Print @(48,192);GetCurrFullPath$(PSide$);
  Print @(48,216);"to";
  Print @(48,240);GetCurrFullPath$(OpSide$());
  Print @(64,252);">";
  Color c(15),c(0):Print @(72,204);Space$(64);
  Print @(72,204);GetCurrName$(PSide$);
  Color c(14),c(0):Print @(72,252);Space$(64);
  Print @(72,252);:TEMP$=InputE$(GetCurrName$(PSide$),64)
  Color c(15),c(0)
  If TEMP$<>"" Then
    TEMP$=GetCurrFullPath$(OpSide$())+TEMP$
    On ERROR ignore
    Copy GetCurrFullName$(PSide$) To TEMP$
    If Not MM.Errno Then
      Kill GetCurrFullName$(PSide$)
    EndIf
    W_ERROR_MSG()
  EndIf
End Sub
'----------------------
Sub W_F7_MkDIR()
Local string TEMP$
  Color c(15),c(1)
  OpenWindow("Make a Directory:",4,14,71,6)
  TEMP$=GetCurrFullPath$(PSide$)
  Chdir TEMP$
  Print @(48,192);TEMP$
  Print @(64,216);">";
  Color c(14),c(0):Print Space$(64);
  Print @(72,216);: TEMP$=InputE$("",64)
  Color c(15),c(0)
  If TEMP$<>"" Then Mkdir TEMP$
End Sub
'----------------------
Sub W_F8_DELETE()
  Color c(15),c(1)
  OpenWindow("Delete:",4,14,71,6)
  Chdir GetCurrFullPath$(PSide$)
  Print @(48,192);GetCurrFullPath$(PSide$);
  Print @(64,216);">";
  Color c(14),c(0):Print @(72,216);Space$(64);
  Print @(72,216);GetCurrName$(PSide$);
  Color c(15)
  Do :CKey=Asc(Inkey$):Loop While CKey=0
  If CKey=13 Then
    On ERROR ignore
    If Mid$(GetCurrName$(PSide$),1,1)="/" Then
      Kill Mid$(GetCurrName$(PSide$),2,63)
      W_ERROR_MSG()
    Else
      Kill GetCurrName$(PSide$)
      W_ERROR_MSG()
    EndIf
  EndIf
End Sub
'----------------------
Sub W_F9_MENU()
  Chdir GetCurrFullPath$(PSide$)
  If PSide$="L" Then
    LPPos=0:LFLIndx=0:LFLTop=0
    GetMenuList(LFList$(),LFLS%())
    LPrintPanel
  Else
    RPPos=0:RFLIndx=0:RFLTop=0
    GetMenuList(RFList$(),RFLS%())
    RPrintPanel
  EndIf
  SetPControl("ENA")
End Sub
'----------------------
Sub GetMenuList(ML$(),LS%())
Local n
ML$(0)=".."
Restore MENU_ITEMS
For n=1 To 32
  Read ML$(n)
  If ML$(n)="" Then Exit For
Next
LS%(0)=n-1
LS%(1)=0:LS%(2)=0
End Sub
'Window ERROR MSG -----------
Sub W_ERROR_MSG()
Local integer x,l
If MM.Errno Then
  Color c(15),c(8)
  l=Len(MM.ErrMsg$)+4: x=40-(l\2)
  OpenWindow("ERROR:",x,24,l,5)
  Print @((x+2)*8,26*12);
  Print MM.ErrMsg$;
  Color c(15),c(0)
  On ERROR CLEAR
  Do : Loop While Inkey$=""
  PanelsReStart()
EndIf
On ERROR ABORT
End Sub
'============================
Sub GetFList(Disk$,Folder$,SSort$,FList$(),FLS%())
Local string FName$
Local integer RQt,DQt,FQt
Drive Disk$
Chdir Folder$
FList$(RQt)=".."
FName$=Dir$("*",DIR)
Do While FName$<>""
  Inc RQt: Inc DQt: If RQt=RMax Then Exit Do
  FList$(RQt)="/"+FName$
  FName$=Dir$()
Loop
FName$=Dir$("*",FILE)
Do While FName$<>""
  Inc RQt: Inc FQt: If RQt=RMax Then Exit Do
  FList$(RQt)=FName$
  FName$=Dir$()
Loop
FList$(RQt+1)=""
FLS%(0)=RQt
FLS%(1)=DQt
FLS%(2)=FQt
If RQt>0 Then
  If SSort$=">" Then Sort FList$(),,2,1,RQt
  If SSort$="<" Then Sort FList$(),,3,1,RQt
EndIf
End Sub
'============================
Function GetCurrFullPath$(SIDE$) As string
If SIDE$="L" Then
  GetCurrFullPath$=LDisk$+LDir$
Else
  GetCurrFullPath$=RDisk$+RDir$
EndIf
End Function
'----------------------
Function GetCurrName$(SIDE$) As string
If SIDE$="L" Then
  GetCurrName$=LFList$(LFLIndx)
Else
  GetCurrName$=RFList$(RFLIndx)
EndIf
End Function
'----------------------
Function GetCurrFullName$(SIDE$) As string
If SIDE$="L" Then
  GetCurrFullName$=GetCurrFullPath$(SIDE$)+LFList$(LFLIndx)
Else
  GetCurrFullName$=GetCurrFullPath$(SIDE$)+RFList$(RFLIndx)
EndIf
End Function
'----------------------
Function OpSide$() As string
If PSide$="L" Then OpSide$="R" Else OpSide$="L"
End Function
'INPUT+ESCape----------------------
Function InputE$(inString$,size%)
Local k$, DELflag%=0
InputE$=Left$(inString$,size%)
Print InputE$;
If Len(InputE$)<size% Then Print "_";Chr$(8);
Do
  Do
    If DELflag% Then Exit Do
    k$=Inkey$
  Loop While k$=""
  Select Case Asc(k$)
  Case 8  'BS
    If Len(InputE$)>0 Then
      If Len(InputE$)=size% Then
        Print k$;"_";k$;
      Else
        Print " ";k$;k$;"_";k$;
      EndIf
      InputE$=Left$(InputE$,Len(InputE$)-1)
    Else
      DELflag%=0
    EndIf
  Case 13 'ENTER
    Exit Do
  Case 27 'ESC
    InputE$=""
    Exit Do
  Case 32 To 126
    If Len(InputE$)<size% Then Print k$;: InputE$=InputE$+k$
    If Len(InputE$)<size% Then Print "_";Chr$(8);
  Case 127'DEL
    DELflag%=1:k$=Chr$(8)
  End Select
Loop
End Function
'============================
'Panels Interface ReWrite
Sub PanelsReStart()
  PrintFMenu()
  LPanelShow()
  RPanelShow()
  SetPControl("ENA")
End Sub
'Panels Interface Restore
Sub PanelsReStore()
  PrintFMenu()
  LPrintPanel
  RPrintPanel
  SetPControl("ENA")
End Sub
'----------------------LPanel
Sub LPanelShow()
SetPControl("DIS")
LPPos=0:LFLIndx=0:LFLTop=0
GetFList(LDisk$,LDir$,LSort$,LFList$(),LFLS%())
LPrintPanel
End Sub
Sub LPrintPanel
OpenWindow(LDisk$+" ["+LSort$+"]",0,0,39,37)
WBar(0,34,39)
FLPrint(LFList$(),LFLTop, 1,1,38,32)
Print @(48,444)" Folders:";LFLS%(1);", Files:";LFLS%(2);" "
End Sub
'----------------------RPanel
Sub RPanelShow()
SetPControl("DIS")
RPPos=0:RFLIndx=0:RFLTop=0
GetFList(RDisk$,RDir$,RSort$,RFList$(),RFLS%())
RPrintPanel
End Sub
Sub RPrintPanel
OpenWindow(RDisk$+" ["+RSort$+"]",40,0,39,37)
WBar(40,34,39)
FLPrint(RFList$(),RFLTop,41,1,38,32)
Print @(368,444)" Folders:";RFLS%(1);", Files:";RFLS%(2);" "
End Sub
'============================
Sub OpenWindow(Titl$,xc,yc,wc,hc)
Local integer i,x=(xc+1)*8,y=(yc+1)*12
Local string TEMP$=Space$(wc-1)
  For i=0 To hc-2     'Clear space in frame
    Print @(x,y) TEMP$;
    Inc y,12
  Next
  WFrame(xc,yc,wc,hc)
  Print @((xc+2)*8,yc*12)Chr$(181);Titl$;Chr$(198);
End Sub
'----------------------
Sub WFrame(xc,yc,wc,hc)
Local integer x,y
  Print @(xc*8,yc*12)Chr$(201);
  For x=xc+1 To xc+wc-1
    Print Chr$(205);
  Next
  Print @((xc+wc)*8,yc*12)Chr$(187);
  For y=yc+1 To yc+hc-1
    Print @(xc*8,y*12)Chr$(186);
    Print @((xc+wc)*8,y*12)Chr$(186);
  Next
  Print @(xc*8,(yc+hc)*12)Chr$(200);
  For x=xc+1 To xc+wc-1
    Print Chr$(205);
  Next
  Print @((xc+wc)*8,(yc+hc)*12)Chr$(188);
End Sub
'----------------------
Sub WBar(xc,yc,wc)
Local integer x
  Print @(xc*8,yc*12)Chr$(199);
  For x=xc+1 To xc+wc-1
    Print Chr$(196);
  Next
  Print @((xc+wc)*8,yc*12)Chr$(182);
End Sub
'============================
'PRINT File List on Panel
Sub FLPrint(Flist$(),top,xc,yc,wc,hc)
Local string FName$
Local integer y,FNum
FNum=top
For y=yc To yc+hc
  Print @(xc*8,y*12);
  FName$=FList$(FNum)
  If FName$<>"" Then
    FName$=Mid$(FName$,1,wc)
    FName$=FName$+Space$(wc-Len(Fname$))
    Print FName$
    Inc FNum
  Else
    Print Space$(wc)
  EndIf
Next
End Sub
'PRINT HELP -----------
Sub PrintHelp()
Color c(15),c(1)
OpenWindow("Help",10,6,59,20)
Print @(96,96) "File Manager v.1.13 for PicoMiteVGA by JaVaVi(c)2024"
Print @(96,132)"[Arrows],[PgUp],[PgDn],[Home],[End] Keys - Navigation."
Print @(96,156)"[Tab] Key - Switches between left & right panels."
Print @(96,180)"[F1]...[F9] Keys - Conrol Functions."
Print @(96,204)"[Alt]+[F1]...[F9] Keys - Alternate Conrol Functions."
Print @(96,228)"[Enter] Key - Entering a folder, running a file."
Print @(96,252)"[BS]&[Del] Keys - for editing lines in the input field."
Print @(96,276)"[Esc] Key - Escape from input & Exit to command prompt."
Color 0,c(14):Print @(520,84);"    ":Color 0,c(7):Print @(520,96);"    "
Do :Loop Until Inkey$<>""
Color c(15),c(0)
End Sub
'PRINT FUNCTION KEY MENU
Sub PrintFMenu()
Print @(0,480-12);
Print  "F1"; :Color 0,c(7):Print "Help  ";:Color c(15),0
Print " F2"; :Color 0,c(7):Print "ReName";:Color c(15),0
Print " F3"; :Color 0,c(7):Print "List  ";:Color c(15),0
Print " F4"; :Color 0,c(7):Print "Edit  ";:Color c(15),0
Print " F5"; :Color 0,c(7):Print "Copy  ";:Color c(15),0
Print " F6"; :Color 0,c(7):Print "Move  ";:Color c(15),0
Print " F7"; :Color 0,c(7):Print "MkDir ";:Color c(15),0
Print " F8"; :Color 0,c(7):Print "Delete";:Color c(15),0
Print " F9"; :Color 0,c(7):Print "Menu  ";:Color c(15),0
'Print " Esc";:Color 0,c(7):Print "EXIT ";:Color c(15),0
End Sub
'----------------------
Sub PrintAltFMenu()
Print @(0,480-12);
Print  "F1"; :Color 0,c(7):Print "Left  ";:Color c(15),0
Print " F2"; :Color 0,c(7):Print "Right ";:Color c(15),0
Print " F3"; :Color 0,c(7):Print "L.Sort";:Color c(15),0
Print " F4"; :Color 0,c(7):Print "R.Sort";:Color c(15),0
Print " F5"; :Color 0,c(7):Print "      ";:Color c(15),0
Print " F6"; :Color 0,c(7):Print "      ";:Color c(15),0
Print " F7"; :Color 0,c(7):Print "      ";:Color c(15),0
Print " F8"; :Color 0,c(7):Print "      ";:Color c(15),0
Print " F9"; :Color 0,c(7):Print "      ";:Color c(15),0
End Sub
'PRINT Watch Time -----
Sub PrintTime()
  Tick1s=0
  Colour c(14),c(1)
  Print @(600,0);Mid$(Time$,1,5);
  Colour c(15),c(0)
End Sub
'INTERRUP ROUTINES
Sub ISR_Tick1s
  Inc Tick1s
End Sub
'============================
'SCREEN SAVER - Matrix
Sub SSMatrix
Local matr(CHR),fade(CHR),clr,x
For x=1 To CHR:matr(x)=CVR*Rnd:fade(x)=&hF*Rnd:Next
Do
  For x=1 To CHR
    clr=&h1000*(fade(x)-&hF) And &hFF00
    Colour clr
    Print @(x*FW-FW,matr(x)*FH)Chr$(Rnd*223+32);
    If matr(x)>CVR  Then matr(x)=0 Else Inc matr(x)
    If fade(x)=&hF0 Then fade(x)=0 Else Inc fade(x)
  Next
  Pause 10
Loop While Inkey$=""
Colour c(15)
End Sub
