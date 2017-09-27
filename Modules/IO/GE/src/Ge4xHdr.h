/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*
 * The definitions in this file were derived from signa.tpl from
 * David Clunie's Dicom3Tools
 *
 * Copyright (c) 1993-2006, David A. Clunie DBA PixelMed Publishing. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of
 *    conditions and the following disclaimers.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of
 *    conditions and the following disclaimers in the documentation and/or other materials
 *    provided with the distribution.
 *
 * 3. Neither the name of PixelMed Publishing nor the names of its contributors may
 *    be used to endorse or promote products derived from this software.
 *
 * This software is provided by the copyright holders and contributors "as is" and any
 * express or implied warranties, including, but not limited to, the implied warranties
 * of merchantability and fitness for a particular purpose are disclaimed. In no event
 * shall the copyright owner or contributors be liable for any direct, indirect, incidental,
 * special, exemplary, or consequential damages (including, but not limited to, procurement
 * of substitute goods or services; loss of use, data or profits; or business interruption)
 * however caused and on any theory of liability, whether in contract, strict liability, or
 * tort (including negligence or otherwise) arising in any way out of the use of this software,
 * even if advised of the possibility of such damage.
 *
 * This software has neither been tested nor approved for clinical use or for incorporation in
 * a medical device. It is the redistributor's or user's responsibility to comply with any
 * applicable local, state, national or international regulations.
 *
 */

#ifndef Ge4xHdr_h
#define Ge4xHdr_h
#include "ITKIOGEExport.h"

enum GE_4X_STUDYHDR_OFFSET
{
  SIGNA_STHDR_ID = 0,              /*  Study Header Identifier */
  SIGNA_STHDR_REV = 7,             /*  Study Header Revision Number xx.xx.xx */
  SIGNA_STHDR_NUMBLKS = 11,        /*  Number of Study Header Blocks */
  SIGNA_STHDR_CREATORPID = 12,     /*  Study Header Creator (Process Name:PID) */
  SIGNA_STHDR_CREATORTID = 28,     /*  Study Header Creator (Task ID) */
  SIGNA_STHDR_RAWSTID = 29,        /*  Original Raw Data Study Number (null unless different from study number) */
  SIGNA_STHDR_STUDY_NUM = 32,      /*  Study Number */
  SIGNA_STHDR_RAWID = 35,          /*  Raw Data System ID */
  SIGNA_STHDR_SYSTEMID = 37,       /*  System Generation ID */
  SIGNA_STHDR_DATE_ASCII = 39,     /*  Date of Study (ascii dd-mmm-yy) */
  SIGNA_STHDR_STUDYDATEDAY = 44,   /*  Date of Study (integer day) */
  SIGNA_STHDR_STUDYDATEMONTH = 45, /*  Date of Study (integer month) */
  SIGNA_STHDR_STUDYDATEYEAR = 46,  /*  Date of Study (integer year-1900) */
  SIGNA_STHDR_TIME_ASCII = 47,     /*  Time of Study (ascii hh:mm:ss) */
  SIGNA_STHDR_STUDYTIMEHOUR = 51,  /*  Time of Study (integer hr) */
  SIGNA_STHDR_STUDYTIMEMIN = 52,   /*  Time of Study (integer min) */
  SIGNA_STHDR_STUDYTIMESEC = 53,   /*  Time of Study (integer sec) */
  SIGNA_STHDR_PATIENT_NAME = 54,   /*  Patient Name */
  SIGNA_STHDR_PATIENT_ID = 70,     /*  Patient ID */
  SIGNA_STHDR_ID_PAD = 76,         /*  Patient ID padding for future exp. */
  SIGNA_STHDR_PATIENT_AGE = 78,    /*  Age of patient - xxx years or xxx[dDwWmMyY] */
  SIGNA_STHDR_SEX = 80,            /*  Sex of patient - M or F */
  SIGNA_STHDR_WEIGHT = 81,         /*  Weight of the patient in grams */
  SIGNA_STHDR_REF = 83,            /*  Referred by */
  SIGNA_STHDR_DIAG = 99,           /*  Diagnostician */
  SIGNA_STHDR_OP = 115,            /*  Operator */
  SIGNA_STHDR_DESC = 131,          /*  Description */
  SIGNA_STHDR_HIST = 161,          /*  History */
  SIGNA_STHDR_CRTIME = 221,        /*  Creation time in seconds. */
  SIGNA_STHDR_SITE_NAME = 223,     /*  Hospital name */
  SIGNA_STHDR_STATUS = 239,        /*  Patient status */
  SIGNA_STHDR_REQNUM = 240,        /*  Req. Number from Scan Rx first page */
  SIGNA_STHDR_CHECKSUM = 511       /*  checksum */
};

enum GE_4X_SERIESHDR_OFFSET
{
  SIGNA_SEHDR_ID = 0,               /*  Series Header Identifier */
  SIGNA_SEHDR_REV = 7,              /*  Series Header Revision Number xx.xx.xx */
  SIGNA_SEHDR_NUMBLKS = 11,         /*  Number of Series Header Blocks */
  SIGNA_SEHDR_CREATOR_PID = 12,     /*  Series Header Creator (Process Name:PID) */
  SIGNA_SEHDR_CREATOR_TID = 28,     /*  Series Header Creator (Task ID) */
  SIGNA_SEHDR_RAWNUM = 29,          /*  Original Series Number */
  SIGNA_SEHDR_SERIES_NUM = 31,      /*  Series Number */
  SIGNA_SEHDR_RAWID = 33,           /*  Raw Data System ID */
  SIGNA_SEHDR_SYSTEMID = 35,        /*  System Generation ID */
  SIGNA_SEHDR_SATE_ASCII = 37,      /*  Date of Series (ascii dd-mmm-yy) */
  SIGNA_SEHDR_SERIESDATEDAY = 42,   /*  Date of Series (integer day) */
  SIGNA_SEHDR_SERIESDATEMONTH = 43, /*  Date of Series (integer month) */
  SIGNA_SEHDR_SERIESDATEYEAR = 44,  /*  Date of Series (integer year-1900) */
  SIGNA_SEHDR_TIME_ASCII = 45,      /*  Time of Series (ascii hh:mm:ss) */
  SIGNA_SEHDR_SERIESTIMEHOUR = 49,  /*  Time of Series (integer hr) */
  SIGNA_SEHDR_SERIESTIMEMIN = 50,   /*  Time of Series (integer min) */
  SIGNA_SEHDR_SERIESTIMESEC = 51,   /*  Time of Series (integer sec) */
  SIGNA_SEHDR_DESC = 52,            /*  Series Description */
  SIGNA_SEHDR_SERIESTYPE = 112,     /*  Series Type */
  SIGNA_SEHDR_COILTYPE = 113,       /*  Coil Type */
  SIGNA_SEHDR_COILNAME = 114,       /*  Coil Name (HEAD,BODY,coil name) (Not always present) */
  SIGNA_SEHDR_CONTRAST = 122,       /*  Contrast Description */
  SIGNA_SEHDR_PLANETYPE = 138,      /*  Plane Type */
  SIGNA_SEHDR_PLANENAME = 139,      /*  Plane Name (AXIAL,SAGITTAL,CORONAL,OBLIQUE,SCREEN SAVE) */
  SIGNA_SEHDR_IMAGEMODE = 147,      /*  Image Mode */
  SIGNA_SEHDR_FIELDSTRENGTH = 148,  /*  Magnetic Field Strength (Gauss) */
  SIGNA_SEHDR_PULSESEQUENCE = 149,  /*  Pulse Sequence */
  SIGNA_SEHDR_SEQUENCETYPE = 150,   /*  Pulse sequence subtype */
  SIGNA_SEHDR_FOV = 151,            /*  Field of view mm */
  SIGNA_SEHDR_CENTERRL = 153,       /*  Center RL R+ (relative to landmark) */
  SIGNA_SEHDR_CENTERAP = 155,       /*  Center AP A+ (relative to landmark) */
  SIGNA_SEHDR_CENTERSI = 157,       /*  Center SI S+ (relative to landmark) */
  SIGNA_SEHDR_ORIENT = 159,         /*  Orientation (0=supine,1=prone,2=lt decubitus,3=rt decubitus) */
  SIGNA_SEHDR_POSITION = 160,       /*  Position */
  SIGNA_SEHDR_LREF = 161,           /*  Longitudinal Anatomical Reference */
  SIGNA_SEHDR_VREF = 177,           /*  Vertical Anatomical Reference */
  SIGNA_SEHDR_VLAND = 193,          /*  Vertical Landmark (relative to tabletop) mm */
  SIGNA_SEHDR_HLAND = 195,          /*  Horizontal Landmark (relative to table center) mm */
  SIGNA_SEHDR_TABLELOC = 197,       /*  Physical Table Location relative to home */
  SIGNA_SEHDR_SCANMATRIXX = 199,    /*  Scan Matrix - X */
  SIGNA_SEHDR_SCANMATRIXY = 200,    /*  Scan Matrix - Y */
  SIGNA_SEHDR_IMAGEMATRIX = 201,    /*  Image Matrix */
  SIGNA_SEHDR_NUMIMAGES = 202,      /*  No. of Images Allocated */
  SIGNA_SEHDR_GATINGTYPE = 203,     /*  Gating Type */
  SIGNA_SEHDR_PSMODE = 204,         /*  Pulse sequence mode */
  SIGNA_SEHDR_PSDNAME = 205,        /*  PSD name from NAME = aPPL psd name */
  SIGNA_SEHDR_LANDCOUNT = 211,      /*  Landmark counter */
  SIGNA_SEHDR_PROTOCOL = 213,       /*  Protocol name for Scan */
  SIGNA_SEHDR_SCOILTYPE = 223,      /*  Surface coil type */
  SIGNA_SEHDR_SUPPTECH = 224,       /*  Suppression technique */
  SIGNA_SEHDR_SAT = 225,            /*  Bitmap of SAT selections */
  SIGNA_SEHDR_SCIC = 226,           /*  Surface Coil Intensity Correction Flag */
  SIGNA_SEHDR_RSATLOC = 227,        /*  R saturation pulse location  - mm from landmark */
  SIGNA_SEHDR_LSATLOC = 228,        /*  L saturation pulse location  - mm from landmark */
  SIGNA_SEHDR_ASATLOC = 229,        /*  A saturation pulse location  - mm from landmark */
  SIGNA_SEHDR_PSATLOC = 230,        /*  P saturation pulse location  - mm from landmark */
  SIGNA_SEHDR_SSATLOC = 231,        /*  S saturation pulse location  - mm from landmark */
  SIGNA_SEHDR_ISATLOC = 232,        /*  I saturation pulse location  - mm from landmark */
  SIGNA_SEHDR_SATTHICK_X = 233,     /*  X saturation pulse thickness - mm */
  SIGNA_SEHDR_SATTHICK_Y = 234,     /*  Y saturation pulse thickness - mm */
  SIGNA_SEHDR_SATTHICK_Z = 235,     /*  Z saturation pulse thickness - mm */
  SIGNA_SEHDR_VASMODE = 236,        /*  Vascular mode */
  SIGNA_SEHDR_FLOWAXIS = 237,       /*  Phase contrast flow axis */
  SIGNA_SEHDR_VENC = 238,           /*  unused venc ... see image header */
  SIGNA_SEHDR_GATINGTYPE2 = 239,    /*  more pulse sequence types */
  SIGNA_SEHDR_CHECKSUM = 511        /*  checksum */
};

enum GE_4X_IMAGEHDR_OFFSET
{
  SIGNA_IMHDR_ID = 0,                /*  Image Header Identifier */
  SIGNA_IMHDR_REV = 7,               /*  Image Header Revision Number xx.xx.xx */
  SIGNA_IMHDR_NUMBLKS = 11,          /*  Number of Image Header Blocks */
  SIGNA_IMHDR_CREATOR_PID = 12,      /*  Image Header Creator (Process Name:PID) */
  SIGNA_IMHDR_CREATOR_TID = 28,      /*  Image Header Creator (Task ID) */
  SIGNA_IMHDR_SATE_ASCII = 29,       /*  Date of Image (ascii dd-mmm-yy) */
  SIGNA_IMHDR_IMAGEDATEDAY = 34,     /*  Date of Image (integer day) */
  SIGNA_IMHDR_IMAGEDATEMONTH = 35,   /*  Date of Image (integer month) */
  SIGNA_IMHDR_IMAGEDATEYEAR = 36,    /*  Date of Image (integer year-1900) */
  SIGNA_IMHDR_TIME_ASCII = 37,       /*  Time of Image (ascii hh:mm:ss) */
  SIGNA_IMHDR_IMAGETIMEHOUR = 41,    /*  Time of Image (integer hr) */
  SIGNA_IMHDR_IMAGETIMEMIN = 42,     /*  Time of Image (integer min) */
  SIGNA_IMHDR_IMAGETIMESEC = 43,     /*  Time of Image (integer sec) */
  SIGNA_IMHDR_IMAGE_NUM = 44,        /*  Image Number */
  SIGNA_IMHDR_SERIES_NUM = 46,       /*  Series NumbxHdrer of Image */
  SIGNA_IMHDR_RAWID = 48,            /*  Raw Data System ID */
  SIGNA_IMHDR_SYSTEM_ID = 50,        /*  System Generation ID */
  SIGNA_IMHDR_LOCATIONRMIN = 52,     /*  Start Location X, Right min */
  SIGNA_IMHDR_LOCATIONRMAX = 54,     /*  End Location X, Right max */
  SIGNA_IMHDR_LOCATIONAMIN = 56,     /*  Start Location Y, Anterior min */
  SIGNA_IMHDR_LOCATIONAMAX = 58,     /*  End Location Y, Anterior max */
  SIGNA_IMHDR_LOCATIONSMIN = 60,     /*  Start Location Z, Superior min */
  SIGNA_IMHDR_LOCATIONSMAX = 62,     /*  End Location Z, Superior max */
  SIGNA_IMHDR_SLICELOC = 73,         /*  Image Location relative to landmark */
  SIGNA_IMHDR_TABLEPOS = 75,         /*  Table Position */
  SIGNA_IMHDR_SLICE_THICK = 77,      /*  Thickness in mm */
  SIGNA_IMHDR_SLICE_SPACING = 79,    /*  Spacing in mm */
  SIGNA_IMHDR_ROUND = 81,            /*  Round */
  SIGNA_IMHDR_TR = 82,               /*  Repetition/Recovery Time uS */
  SIGNA_IMHDR_SCANTIME = 84,         /*  Scan Time uS */
  SIGNA_IMHDR_TE = 86,               /*  Echo Delay uS */
  SIGNA_IMHDR_TI = 88,               /*  Inversion Time uS */
  SIGNA_IMHDR_OT0 = 90,              /*  Other time 0 - Reserved for future use. */
  SIGNA_IMHDR_OT1 = 92,              /*  Other time 1 - Reserved for future use. */
  SIGNA_IMHDR_OT2 = 94,              /*  Other time 2 - Reserved for future use. */
  SIGNA_IMHDR_OT3 = 96,              /*  Other time 3 - Reserved for future use. */
  SIGNA_IMHDR_NUMECHOS = 98,         /*  Number of echos. */
  SIGNA_IMHDR_ECHONUM = 99,          /*  Echo number. */
  SIGNA_IMHDR_NUMSLICES = 100,       /*  Number of slices in scan group. */
  SIGNA_IMHDR_NUMAVG = 101,          /*  Number of averages (before fractional nex) */
  SIGNA_IMHDR_RMODE = 102,           /*  Research mode used */
  SIGNA_IMHDR_PSD_FILE = 103,        /*  Name of PSD file. */
  SIGNA_IMHDR_PSD_DAY = 119,         /*  Creation Date of PSD file dd */
  SIGNA_IMHDR_PSD_MONTH = 120,       /*  Creation Date of PSD file mm */
  SIGNA_IMHDR_PSD_YEAR = 121,        /*  Creation Date of PSD file yy-1900 */
  SIGNA_IMHDR_PSD_HOUR = 122,        /*  Creation Date of PSD file hh */
  SIGNA_IMHDR_PSD_MINUTE = 123,      /*  Creation Date of PSD file mm */
  SIGNA_IMHDR_PSD_SECOND = 124,      /*  Creation Date of PSD file ss */
  SIGNA_IMHDR_GRAPHRX = 125,         /*  Graphically Prescribed (? any non-zero is yes) */
  SIGNA_IMHDR_SERIES_RX = 126,       /*  Series Numbers [3*3] from which prescribed */
  SIGNA_IMHDR_IMAGE_RX = 131,        /*  Image Numbers [3*3] from which prescribed */
  SIGNA_IMHDR_SHAPE = 136,           /*  Image Shape */
  SIGNA_IMHDR_X_DIM = 137,           /*  X pixel dimension */
  SIGNA_IMHDR_Y_DIM = 138,           /*  Y pixel dimension */
  SIGNA_IMHDR_PIXELSIZE = 139,       /*  Pixel Size - mm */
  SIGNA_IMHDR_COMPRESS = 141,        /*  Image Compressed (non-zero=technique) */
  SIGNA_IMHDR_BITPIX = 142,          /*  Bits per Pixel - not always filled in */
  SIGNA_IMHDR_WINDOW = 143,          /*  Default Window */
  SIGNA_IMHDR_LEVEL = 144,           /*  Default Level */
  SIGNA_IMHDR_NUMBLKS_FILE = 145,    /*  Number of Blocks in File */
  SIGNA_IMHDR_NEX = 146,             /*  Number of excitations (presumably post-fractional NEX) */
  SIGNA_IMHDR_PEAK_SAR = 148,        /*  Value of peak SAR watts/kg */
  SIGNA_IMHDR_AVG_SAR = 150,         /*  Value of average SAR watts/kg */
  SIGNA_IMHDR_SAR_MON = 152,         /*  SAR monitored */
  SIGNA_IMHDR_CONTIG_SLICES = 153,   /*  Contiguous slices (? any non-zero is yes) */
  SIGNA_IMHDR_CARDIAC_RATE = 154,    /*  Cardiac Heart Rate */
  SIGNA_IMHDR_TRIG_DELAY = 155,      /*  Total Delay Time After Trigger - ms between QRS peak and 1st excitation pulse */
  SIGNA_IMHDR_ARR = 157,             /*  Arrhythmia Rejection Ratio - % of avg RR during which trigger is recognized */
  SIGNA_IMHDR_CRTIME = 158,          /*  Cardiac Rep Time - 1 pulse every beat, 2 pulse very 2nd beat, etc */
  SIGNA_IMHDR_ICC = 159,             /*  Images per Cardiac Cycle (single scan only) */
  SIGNA_IMHDR_TOTAL_RR = 160,        /*  Number of RR's during the Scan */
  SIGNA_IMHDR_TRANS_DB = 162,        /*  Transmit attenuator setting .1 dB */
  SIGNA_IMHDR_REC_DB = 163,          /*  Receive attenuator setting .1dB */
  SIGNA_IMHDR_FIELD = 164,           /*  Magnetic Field Strength 10 microgauss */
  SIGNA_IMHDR_OFFSET = 166,          /*  Image offset - frequency/phase offset [-256...256] */
  SIGNA_IMHDR_DELAY = 167,           /*  Inter image/inter location delay - ms between excitation pulses within RR interval */
  SIGNA_IMHDR_PSD_NAME = 169,        /*  PSD name from NAME = aPPL psd name */
  SIGNA_IMHDR_FLIP = 175,            /*  Flip angle for GRASS */
  SIGNA_IMHDR_CORRECT = 176,         /*  Type of correction for surface coils ????? */
  SIGNA_IMHDR_SERIES = 178,          /*  Series no. of corrected/uncor images ????? */
  SIGNA_IMHDR_IMAGE = 180,           /*  Image no. of corrected/uncor images ????? */
  SIGNA_IMHDR_EX_COIL = 182,         /*  Extremity coil */
  SIGNA_IMHDR_LOC2_SER = 193,        /*  Series no. of second localizer */
  SIGNA_IMHDR_LOC2_IMG = 195,        /*  Image no. of second localizer */
  SIGNA_IMHDR_LOC_R = 197,           /*  R center coordinate on plane image mm */
  SIGNA_IMHDR_LOC_A = 199,           /*  A center coordinate on plane image mm */
  SIGNA_IMHDR_LOC_S = 201,           /*  S center coordinate on plane image mm */
  SIGNA_IMHDR_NORM_R = 203,          /*  R normal coordinate mm */
  SIGNA_IMHDR_NORM_A = 205,          /*  A normal coordinate mm */
  SIGNA_IMHDR_NORM_S = 207,          /*  S normal coordinate mm */
  SIGNA_IMHDR_TLHC_R = 209,          /*  TLHC R coordinate mm */
  SIGNA_IMHDR_TLHC_A = 211,          /*  TLHC A coordinate mm */
  SIGNA_IMHDR_TLHC_S = 213,          /*  TLHC S coordinate mm */
  SIGNA_IMHDR_TRHC_R = 215,          /*  TRHC R coordinate mm */
  SIGNA_IMHDR_TRHC_A = 217,          /*  TRHC A coordinate mm */
  SIGNA_IMHDR_TRHC_S = 219,          /*  TRHC S coordinate mm */
  SIGNA_IMHDR_BLHC_R = 221,          /*  BLHC R coordinate mm */
  SIGNA_IMHDR_BLHC_A = 223,          /*  BLHC A coordinate mm */
  SIGNA_IMHDR_BLHC_S = 225,          /*  BLHC S coordinate mm */
  SIGNA_IMHDR_HDRDISC = 227,         /*  Image header disclaimer */
  SIGNA_IMHDR_MIN_DELAY = 228,       /*  Minimum delay after trigger ms */
  SIGNA_IMHDR_NUM_PHASES = 229,      /*  Number of cardiac phases to reconstruct [1...32] */
  SIGNA_IMHDR_TE2 = 230,             /*  TE2 (VEMP) us */
  SIGNA_IMHDR_SWAP = 232,            /*  Swap phase/frequency axis - operators choice */
  SIGNA_IMHDR_PAUSE_INT = 233,       /*  Pause interval ms */
  SIGNA_IMHDR_PAUSE_TIME = 234,      /*  Pause time ms */
  SIGNA_IMHDR_CV_BITMAP = 236,       /*  Bitmap defining users CVs */
  SIGNA_IMHDR_USER0 = 237,           /*  PSD user variable 0 */
  SIGNA_IMHDR_USER1 = 239,           /*  PSD user variable 1 */
  SIGNA_IMHDR_USER2 = 241,           /*  PSD user variable 2 */
  SIGNA_IMHDR_USER3 = 243,           /*  PSD user variable 3 */
  SIGNA_IMHDR_USER4 = 245,           /*  PSD user variable 4 */
  SIGNA_IMHDR_USER5 = 247,           /*  PSD user variable 5 */
  SIGNA_IMHDR_USER6 = 249,           /*  PSD user variable 6 */
  SIGNA_IMHDR_USER7 = 251,           /*  PSD user variable 7 */
  SIGNA_IMHDR_USER8 = 253,           /*  PSD user variable 8 */
  SIGNA_IMHDR_USER9 = 255,           /*  PSD user variable 9 */
  SIGNA_IMHDR_OBLIQUE = 257,         /*  Oblique plane (most like plane) */
  SIGNA_IMHDR_CONTRAST = 258,        /*  Contrast used */
  SIGNA_IMHDR_CONTRAST_TYPE = 259,   /*  Contrast agent */
  SIGNA_IMHDR_CONTRAST_AMT = 264,    /*  Contrast amount */
  SIGNA_IMHDR_FILEFRMT = 266,        /*  File format */
  SIGNA_IMHDR_AUTOCF = 267,          /*  Auto center frequency */
  SIGNA_IMHDR_TRANSMIT_FREQ = 268,   /*  Actual transmit freq used on scan Hz */
  SIGNA_IMHDR_RECEIVE_FREQ = 270,    /*  Actual receive freq used on scan Hz */
  SIGNA_IMHDR_AUTOTRANSFREQ = 272,   /*  Recommended automated transmit freq Hz */
  SIGNA_IMHDR_AUTORECFREQ = 274,     /*  Recommended automated receive freq Hz */
  SIGNA_IMHDR_AUTOTRANSDB = 276,     /*  Recommended automated transmit attenuation .1 dB */
  SIGNA_IMHDR_AUTORECSDB = 278,      /*  Recommended automated receive attenuation .1 dB */
  SIGNA_IMHDR_HISTOGRAM = 280,       /*  Histogram present in raw header? */
  SIGNA_IMHDR_SWAPR = 281,           /*  Swapped phase/frequency - reality (rules or choice) */
  SIGNA_IMHDR_R1 = 282,              /*  R1 for prescan */
  SIGNA_IMHDR_R2 = 283,              /*  R2 for prescan */
  SIGNA_IMHDR_VBW = 284,             /*  Variable bandwidth - docs say enum=0=no,1=yes but seems to be kHz */
  SIGNA_IMHDR_R1MAN = 285,           /*  R1 manual */
  SIGNA_IMHDR_R2MAN = 286,           /*  R2 manual */
  SIGNA_IMHDR_AUTOPRESCAN = 287,     /*  auto/manual prescan flag */
  SIGNA_IMHDR_MODPRESCAN = 288,      /*  Changed prescan values */
  SIGNA_IMHDR_IMAGE_TYPE = 289,      /*  Image type */
  SIGNA_IMHDR_VASCCOLLAPSE = 290,    /*  Vascular collapse */
  SIGNA_IMHDR_THICKDISC = 291,       /*  Thickness disclaimer */
  SIGNA_IMHDR_VENC = 292,            /*  PC venc mm/sec */
  SIGNA_IMHDR_PRJANGLE = 293,        /*  tardis projection angle - degrees */
  SIGNA_IMHDR_CONCAT_SAT = 295,      /*  concatenated sat selected */
  SIGNA_IMHDR_FRACECHO = 296,        /*  TE Flag */
  SIGNA_IMHDR_ETL = 297,             /*  Echo train length */
  SIGNA_IMHDR_PHASES = 299,          /*  Slice multiplier to obtain phases for FAST - MP option (multiphase) - number of phases per location */
  SIGNA_IMHDR_PHASENUM = 300,        /*  Cardiac phase number of current image */
  SIGNA_IMHDR_NUMACQ = 301,          /*  Number of Acquisitions in scan */
  SIGNA_IMHDR_VASC_FLAG = 302,       /*  Vascular imaging flags */
  SIGNA_IMHDR_VENC_SCALE = 303,      /*  VENC scaling factor */
  SIGNA_IMHDR_CHECKSUM = 511         /*  checksum */
};

enum GE_4X_OFFSETS {
    SIGNA_STHDR_START=    6*256,
    SIGNA_SEHDR_START=    8*256,
    SIGNA_IHDR_START=     10*256
};

#endif  /* __Ge4xHdr_h */
