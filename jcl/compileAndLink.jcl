//IGYWAOC PROC LNGPRFX='IGY640',LIBPRFX='CEE'
//*********************************************************************
//*  Compile Procedure for Advent of Code 2023                        *
//*********************************************************************
//COBOL   EXEC PGM=IGYCRCTL,REGION=0M,PARM='OPTFILE'
//SYSIN    DD  PATH='/z/z28982/aoc2023/src/&PGMNAME..cbl',
//             PATHOPTS=ORDONLY,FILEDATA=TEXT,RECFM=F
//SYSOPTF  DD *
 LC(0)
 LIST
 MAP(HEX)
 XREF(SHORT)
 ARCH(14)
 OPT(2)
 STGOPT
 NOSMARTBIN
/*
//STEPLIB  DD  DSNAME=&LNGPRFX..SIGYCOMP,DISP=SHR
//         DD  DSNAME=&LIBPRFX..SCEERUN,DISP=SHR
//         DD  DSNAME=&LIBPRFX..SCEERUN2,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  DSNAME=&&LOADSET,UNIT=SYSALLDA,
//             DISP=(MOD,PASS),SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT4   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT5   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT6   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT7   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT8   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT9   DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT10  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT11  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT12  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT13  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT14  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSUT15  DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//SYSMDECK DD  UNIT=SYSALLDA,SPACE=(CYL,(1,1)),VOL=(,,,1)
//CEEDUMP  DD DUMMY
//SYSUDUMP DD DUMMY
//SUCCESS IF RC < 8 THEN
//LKED    EXEC PGM=IEWBLINK,REGION=0M
//SYSLIB   DD  DSNAME=&LIBPRFX..SCEELKEX,DISP=SHR
//         DD  DSNAME=&LIBPRFX..SCEELKED,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DSNAME=&SYSUID..AOC.LOAD(&PGMNAME),DISP=SHR
//CEEDUMP  DD DUMMY
//SYSUDUMP DD DUMMY
//        ENDIF