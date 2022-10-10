This is the NoICE Monitor for the TRS-80/Tandy Color Computer.

```
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
*     10-Oct-22 Michael Furman - Fixes for Correct 6551 UART Operation.
*               The original code from RG was resetting and reprogramming
*               the UART for every operation.  This causes the UART to stop
*               transmitting in the middle of a byte resulting in garbage.
*               The second fix - Each time the monitor is entered, whether
*               it is via cold start or any interrupt the current registers
*               are sent back.  This should be the FN_READ_RG command but
*               FN_GET_STAT was being sent instead, causing NoICE to get
*               confused.
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
```


