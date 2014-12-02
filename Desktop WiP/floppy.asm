PORT 03F0-03F7 - FDC 1	(1st Floppy Disk Controller)	second FDC at 0370
Note:	floppy disk controller is usually an 8272, 8272A, NEC765 (or
	  compatible), or an 82072 or 82077AA for perpendicular recording at
	  2.88M
SeeAlso: PORT 0370h-0377h

03F0  R-  diskette controller status A (PS/2) (see #P0857)
03F0  R-  diskette controller status A (PS/2 model 30) (see #P0858)
03F0  R-  diskette EHD controller board jumper settings (82072AA) (see #P0859)
03F1  R-  diskette controller status B (PS/2) (see #P0860)
03F1  R-  diskette controller status B (PS/2 model 30) (see #P0861)
03F2  -W  diskette controller DOR (Digital Output Register) (see #P0862)
03F3  ?W  tape drive register (on the 82077AA)
	bit 7-2	 reserved, tri-state
	bit 1-0	 tape select
		=00  none, drive 0 cannot be a tape drive.
		=01  drive1
		=10  drive2
		=11  drive3
03F4  R-  diskette controller main status register (see #P0865)
	Note:	in non-DMA mode, all data transfers occur through
		  PORT 03F5h and the status registers (bit 5 here
		  indicates data read/write rather than than
		  command/status read/write)
03F4  -W  diskette controller data rate select register (see #P0866)
03F5  R-  diskette command/data register 0 (ST0) (see #P0867)
	status register 1 (ST1) (see #P0868)
	status register 2 (ST2) (see #P0869)
	status register 3 (ST3) (see #P0870)
03F5  -W  diskette command register.  The commands summarized here are
	  mostly multibyte commands. This is for brief recognition only.
	  (see #P0873)
03F6  --  reserved on FDC
03F6  rW  FIXED disk controller data register (see #P0871)
03F7  RW  harddisk controller (see #P0872)
03F7  R-  diskette controller DIR (Digital Input Register, PC/AT mode)
		 bit 7 = 1 diskette change
		 bit 6-0   tri-state on FDC
03F7  R-  diskette controller DIR (Digital Input Register, PS/2 mode)
	  (see #P0863)
03F7  R-  diskette controller DIR (Digital Input Register, PS/2 model 30)
	  (see #P0864)
03F7  -W  configuration control register (PC/AT, PS/2)
		 bit 7-2       reserved, tri-state
		 bit 1-0 = 00  500 Kb/S mode (MFM)
			 = 01  300 Kb/S mode (MFM)
			 = 10  250 Kb/S mode (MFM)
			 = 11  1   Mb/S mode (MFM) (on 82072/82077AA)
	conflict bit 0	   FIXED DISK drive 0 select
03F7  -W  configuration control register (PS/2 model 30)
		 bit 7-3       reserved, tri-state
		 bit 2	       NOPREC (has no function. set to 0 by hardreset)
		 bit 1-0 = 00  500 Kb/S mode (MFM)
			 = 01  300 Kb/S mode (MFM)
			 = 10  250 Kb/S mode (MFM)
			 = 11  1   Mb/S mode (MFM) (on 82072/82077AA)
	conflict bit 0	   FIXED DISK drive 0 select

Bitfields for diskette controller status A (PS/2):
Bit(s)	Description	(Table P0857)
 7	interrupt pending
 6	-DRV2	second drive installed
 5	step
 4	-track 0
 3	head 1 select
 2	-index
 1	-write protect
 0	+direction
SeeAlso: #P0858,#P0860

Bitfields for diskette controller status A (PS/2 model 30):
Bit(s)	Description	(Table P0858)
 7	interrupt pending
 6	DRQ
 5	step F/F
 4	-track 0
 3	head 1 select
 2	+index
 1	+write protect
 0	-direction
SeeAlso: #P0857,#P0859,#P0861

Bitfields for diskette EHD controller board jumper settings:
Bit(s)	Description	(Table P0859)
 7-6	drive 3
 5-4	drive 2
 3-2	drive 1
 1-0	drive 0
	 00  1.2Mb
	 01  720Kb
	 10  2.8Mb
	 11  1.4Mb
SeeAlso: #P0857

Bitfields for diskette controller status B (PS/2):
Bit(s)	Description	(Table P0860)
 7-6	reserved (1)
 5	drive select (0=A:, 1=B:)
 4	write data
 3	read data
 2	write enable
 1	motor enable 1
 0	motor enable 0
SeeAlso: #P0857,#P0861

Bitfields for diskette controller status B (PS/2 model 30):
Bit(s)	Description	(Table P0861)
 7	-DRV2 second drive installed
 6	-DS1
 5	-DS0
 4	write data F/F
 3	read data F/F
 2	write enable F/F
 1	-DS3
 0	-DS2
SeeAlso: #P0858,#P0860

Bitfields for diskette controller Digital Output Register (DOR):
Bit(s)	Description	(Table P0862)
 7-6	reserved on PS/2
 7	drive 3 motor enable
 6	drive 2 motor enable
 5	drive 1 motor enable
 4	drive 0 motor enable
 3	diskette DMA enable (reserved PS/2)
 2	=1  FDC enable	(controller reset)
	=0  hold FDC at reset
 1-0	drive select (0=A 1=B ..)
SeeAlso: #P0857,#P0865,#P0866,#P0863

Bitfields for diskette controller Digital Input Register (PS/2 mode):
Bit(s)	Description	(Table P0863)
 7	= 1 diskette change
 6-3	= 1
 2	datarate select1
 1	datarate select0
 0	= 0 high density select (500Kb/s, 1Mb/s)
 0	(conflict) FIXED DISK drive 0 select
SeeAlso: #P0864,#P0862

Bitfields for diskette controller Digital Input Register (PS/2 model 30):
Bit(s)	Description	(Table P0864)
 7	= 0 diskette change
 6-4	= 0
 3	-DMA gate (value from DOR register)
 2	NOPREC (value from CCR register)
 1	datarate select1
 0	datarate select0
 0	(conflict) FIXED DISK drive 0 select
SeeAlso: #P0863

Bitfields for diskette controller main status register:
Bit(s)	Description	(Table P0865)
 7	=1  RQM	 data register is ready
	=0  no access is permitted
 6	=1  transfer is from controller to system
	=0  transfer is from system to controller
 5	non-DMA mode
 4	diskette controller is busy
 3	drive 3 busy (reserved on PS/2)
 2	drive 2 busy (reserved on PS/2)
 1	drive 1 busy (= drive is in seek mode)
 0	drive 0 busy (= drive is in seek mode)
SeeAlso: #P0862

Bitfields for diskette controller data rate select register:
Bit(s)	Description	(Table P0866)
 7-2	reserved on 8272
 7	software reset (self clearing)	82072/82077AA
 6	power down			82072/82077AA
 5	(8272/82077AA) reserved (0)
	(82072) PLL select bit
 4-2	write precompensation value, 000 default
 1-0	data rate select
	=00  500 Kb/S MFM	250 Kb/S FM
	=01  300 Kb/S MFM	150 Kb/S FM
	=10  250 Kb/S MFM	125 Kb/S FM
	=11  1Mb/S	MFM	illegal	 FM on 8207x
SeeAlso: #P0862

Bitfields for diskette command/data register 0 (ST0):
Bit(s)	Description	(Table P0867)
 7-6	last command status
	00  command terminated successfully
	01  command terminated abnormally
	10  invalid command
	11  terminated abnormally by change in ready signal
 5	seek completed
 4	equipment check occurred after error
 3	not ready
 2	head number at interrupt
 1-0	unit select (0=A 1=B .. ) (on PS/2: 01=A  10=B)
SeeAlso: #P0868,#P0869,#P0870

Bitfields for diskette status register 1 (ST1):
Bit(s)	Description	(Table P0868)
 7	end of cylinder; sector# greater then sectors/track
 6	=0
 5	CRC error in ID or data field
 4	overrun
 3	=0
 2	sector ID not found
 1	write protect detected during write
 0	ID address mark not found
SeeAlso: #P0867,#P0869,#P0870

Bitfields for diskette status register 2 (ST2):
Bit(s)	Description	(Table P0869)
 7	=0
 6	deleted Data Address Mark detected
 5	CRC error in data
 4	wrong cylinder detected
 3	scan command equal condition satisfied
 2	scan command failed, sector not found
 1	bad cylinder, ID not found
 0	missing Data Address Mark
SeeAlso: #P0867,#P0868,#P0870

Bitfields for diskette status register 3 (ST3):
Bit(s)	Description	(Table P0870)
 7	fault status signal
 6	write protect status
 5	ready status
 4	track zero status
 3	two sided status signal
 2	side select (head select)
 1-0	unit select (0=A 1=B .. )
SeeAlso: #P0867,#P0868,#P0869

Bitfields for fixed disk controller data register:
Bit(s)	Description	(Table P0871)
 7-4	reserved
 3	=0  reduce write current
	=1  head select 3 enable
 2	disk reset enable
 1	disk initialization disable
 0	reserved
SeeAlso: #P0862,#P0872

Bitfields for hard disk controller:
Bit(s)	Description	(Table P0872)
 6	FIXED DISK write gate
 5	FIXED DISK head select 3 / reduced write current
 4	FIXED DISK head select 2
 3	FIXED DISK head select 1
 2	FIXED DISK head select 0
 1	FIXED DISK drive 1 select
 0	FIXED DISK drive 0 select
SeeAlso: #P0871

(Table P0873)
Values for diskette commands:
	MFM = MFM mode selected, opposite of MF mode
	HDS = head select
	DS  = drive select
	MT  = multi track operation
	SK  = skip deleted data address mark
   Command	     # bytes	D7  6	5   4	3   2	1   0
 read track		9	0  MFM	0   0	0   0	1   0
				0   0	0   0	0 HDS DS1 DS0
 specify		3	0   0	0   O	O   O	1   1
 sense drive status	2	0   0	0   0	0   1	0   0
				0   0	0   0	0 HDS DS1 DS0
 write data		9	MT MFM	0   0	0   1	0   1
				0   0	0   0	0 HDS DS1 DS0
 read data		9	MT MFM SK   0	0   1	1   0
				0   0	0   0	0 HDS DS1 DS0
 recalibrate		2	0   0	0   0	0   1	1   1
				0   0	0   0	0   0 DS1 DS0
 sense interrupt status 1	0   0	0   0	1   0	0   0
 write deleted data	9	MT MFM	0   0	1   0	0   1
				0   0	0   0	0 HDS DS1 DS0
 read ID		2	0  MFM	0   0	1   0	1   0
				0   0	0   0	0 HDS DS1 DS0
 read deleted data	9	MT MFM SK   0	1   1	0   0
				0   0	0   0	0 HDS DS1 DS0
 format track		10	0  MFM	0   0	1   1	0   1
				0   0	0   0	0 HDS DS1 DS0
 dumpreg **		1	0   0	0   0	1   1	1   0
 seek			3	0   0	0   0	1   1	1   1
				0   0	0   0	0 HDS DS1 DS0
 version** (see #P0874) 1	0   0	0   1	0   0	0   0
 scan equal *		9	MT MFM SK   1	0   0	0   1
				0   0	0   0	0 HDS DS1 DS0
 perpendicular mode **	2	0   0	0   1	0   0	1   0
				0   0	0   0	0   0 WGATE GAP
 configure **		4	0   0	0   1	0   0	1   1
				0   0	0   0	0   0	0   0
 unlock FIFO **		1	0   0	0   1	0   1	0   0
 verify			9	MT MFM SK   1	0   1	1   0
				EC  0	0   0	0 HDS DS1 DS0
 partid ** (see #P0874) 1	0   0	0   1	1   0	0   0
 scan low or equal *	9	MT MFM SK   1	1   0	0   1
				0   0	0   0	0 HDS DS1 DS0
 scan high or equal *	9	MT MFM SK   1	1   1	0   1
				0   0	0   0	0 HDS DS1 DS0
 exit standby mode ***	1	0   0	1   1	0   1	0   0
 enter standby mode ***	1	0   0	1   1	0   1	0   1
 hard reset ***		1	0   0	1   1	0   1	1   0
 lock FIFO **		1	1   0	0   1	0   1	0   0
 relative seek **	3	1  DIR	0   0	1   1	1   1
				0   0	0   0	0 HDS DS1 DS0
BEWARE: not every invalid command is treated as invalid!
 *   Note: the scan commands aren't mentioned for the 82077AA.
 **  Note: EHD controller commands.
 *** Note: Supported by NEC72065B only.

(Table P0874)
Values for FDC Controller chip type identification:
  version lFIFO partid Chip type
    80h	   80h	  -    NEC D765, Intel 8272A or compatible
    80h	   00h	  -    Intel 82072
    81h	    -	  -    Very Early Intel 82077 or compatible
    90h	   80h	  -    Old Intel 82077, no FIFO
    90h	    ?	  ?    NEC 72065B
    90h	   00h	 80h   New Intel 82077 (82077AA if port 3x3h bits 1-0 are R/W)
    90h	   00h	 41h   Intel 82078
    90h	   00h	 73h   National Semiconductor PC87306
    90h	   00h	other  Intel 82078 compatible
    A0h	    -	  -    SMC FDC37c65C+
Note:	Before issuing the partid command, one must first issue an unlock
	  FIFO, immediately followed by a lock FIFO instruction. The status
	  byte returned by the lock FIFO instruction is used in the table above
SeeAlso: #P0873
