/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGE4ImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/
#include "itkGE4ImageIO.h"
#include "itkIOCommon.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkGEImageHeader.h"
#include "idbm_hdr_def.h"
#include "itkMvtSunf.h"
#include "itkDirectory.h"
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
//From uiig library "The University of Iowa Imaging Group-UIIG"

namespace itk 
{
  // Default constructor
  GE4ImageIO::GE4ImageIO()
  {
    //Purposefully left blank
  }

  GE4ImageIO::~GE4ImageIO()
  {
    //Purposefully left blank
    
  }

  bool GE4ImageIO::CanReadFile( const char* FileNameToRead )
  {
    char tmpStr[64];
    this->SetFileName(FileNameToRead);
    std::ifstream f(FileNameToRead,std::ifstream::binary);
    if(!f.is_open())
      return false;
    f.close();
    // This is a weak heuristic but should only be true for GE4 files
    // 
    // Get the Plane from the IMAGE Header.
    if(this->GetStringAt(f, SEHDR_START * 2 + SEHDR_PNAME * 2,tmpStr,16,false) == -1)
      return false;
    tmpStr[16] = '\0';
    // if none of these strings show up, most likely not GE4
    if (strstr (tmpStr, "CORONAL") == NULL &&
        strstr (tmpStr, "SAGITTAL") == NULL &&
        strstr (tmpStr, "AXIAL") == NULL)
      {
        return false;
      }
    //
    // doesn't appear to be any signature in the header so I guess
    // I have to assume it's readable
    return true;
  }

  struct GEImageHeader *GE4ImageIO::ReadHeader(const char *FileNameToRead)
  {
    // #define VERBOSE_DEBUGGING
#if defined(VERBOSE_DEBUGGING)
#define RGEDEBUG(x) x
#else
#define RGEDEBUG(x)
#endif
    struct GEImageHeader *hdr = new struct GEImageHeader;
    if(hdr == 0)
      RAISE_EXCEPTION();

    RGEDEBUG(char debugbuf[16384];)
      char tmpStr[IOCommon::ITK_MAXPATHLEN+1];
    int intTmp;
    short int tmpShort;
    float tmpFloat;
    if(FileNameToRead == 0 || strlen(FileNameToRead) == 0)
      return 0;

    //
    // save off the name of the current file...
    strcpy(hdr->filename,FileNameToRead);

    //
    // Next, can you open it?
    std::ifstream f(FileNameToRead,std::ifstream::binary);
    //
    // if any operation doesn't succeed we want to get the hell out.
    // I guess since ReadImageInformation returns no error code, the
    // only way out is to raise an exception
    if(!f.is_open())
      RAISE_EXCEPTION();

    this->GetStringAt(f, STHDR_START * 2 + STHDR_DATE * 2,tmpStr,10);
    tmpStr[10] = '\0';
    strcpy(hdr->date, tmpStr);

    RGEDEBUG(std::sprintf (debugbuf, "Date = %s\n", tmpStr); cerr << debugbuf;)
      // Get Patient-Name from the STUDY Header 
      this->GetStringAt(f, STHDR_START * 2 + STHDR_PNM * 2,tmpStr, 32);
    tmpStr[32] = '\0';
    strcpy(hdr->hospital, tmpStr);

    /* Get Patient-Number from the STUDY Header */
    this->GetStringAt(f, STHDR_START * 2 + STHDR_PID * 2,tmpStr, 12);
    tmpStr[12] = '\0';
    RGEDEBUG(std::sprintf (debugbuf, "Patient-Number = %s\n", tmpStr); cerr << debugbuf;)
      strcpy(hdr->patientId,tmpStr);

    /* Get the Exam-Number from the STUDY Header */
    this->GetStringAt(f, STHDR_START * 2 + STHDR_STNUM * 2,tmpStr, 6);
    tmpStr[6] = '\0';
    RGEDEBUG(std::sprintf (debugbuf, "Exam-Number = %s\n", tmpStr); cerr << debugbuf;)
      strcpy(hdr->scanId,tmpStr);

    /* Get the FOV from the SERIES Header */
    f.seekg ( SEHDR_START * 2 + SEHDR_FOV * 2, std::ios::beg);
    IOCHECK();
    f.read ((char *)&intTmp,sizeof(intTmp));
    IOCHECK();
    tmpFloat = MvtSunf (intTmp);

    hdr->xFOV = tmpFloat;
    hdr->yFOV = hdr->xFOV;
    RGEDEBUG(std::sprintf (debugbuf, "FOV = %fx%f\n", hdr->xFOV, hdr->yFOV); cerr << debugbuf;)

      /* Get the Plane from the IMAGE Header */
      this->GetStringAt(f, SEHDR_START * 2 + SEHDR_PNAME * 2,tmpStr,16);
    tmpStr[16] = '\0';
    if (strstr (tmpStr, "CORONAL") != NULL)
      {
  hdr->imagePlane = CORONAL;
      }
    else if (strstr (tmpStr, "SAGITTAL") != NULL)
      {
  hdr->imagePlane = SAGITTAL;
      }
    else if (strstr (tmpStr, "AXIAL") != NULL)
      {
  hdr->imagePlane = AXIAL;
      }
    else
      {
  hdr->imagePlane = CORONAL;
      }
    RGEDEBUG(std::sprintf (debugbuf, "Plane = %d\n", hdr->imagePlane); cerr << debugbuf;)

      /* Get the Scan Matrix from the IMAGE Header */
      this->GetShortAt(f,SEHDR_START * 2 + SEHDR_SMATRIX * 2,&(hdr->acqXsize));
    this->GetShortAt(f,(SEHDR_START * 2 + SEHDR_SMATRIX * 2)+sizeof(short),
         &(hdr->acqYsize));

    RGEDEBUG(std::sprintf (debugbuf, "Scan Matrix = %dx%d\n", hdr->acqXsize, hdr->acqYsize); cerr << debugbuf;)

      /* Get Series-Number from SERIES Header */
      this->GetStringAt(f, SEHDR_START * 2 + SEHDR_SERNUM * 2,tmpStr,3);
    tmpStr[3] = '\0';
    hdr->seriesNumber = atoi (tmpStr);
    RGEDEBUG(std::sprintf (debugbuf, "Series Number = %d\n", hdr->seriesNumber); cerr << debugbuf;)

      /* Get Image-Number from IMAGE Header */
      this->GetStringAt(f, IHDR_START * 2 + IHDR_IMNUM * 2,tmpStr,3);
    tmpStr[3] = '\0';
    hdr->imageNumber = atoi (tmpStr);
    RGEDEBUG(std::sprintf (debugbuf, "Image Number = %d\n", hdr->imageNumber); cerr << debugbuf;)

      /* Get Images-Per-Slice from IMAGE Header */
      this->GetStringAt(f, IHDR_START * 2 + IHDR_CPHASE * 2,tmpStr,3);
    tmpStr[3] = '\0';
    hdr->imagesPerSlice = atoi (tmpStr);
    RGEDEBUG(std::sprintf (debugbuf, "Images Per Slice = %d\n", hdr->imagesPerSlice); cerr << debugbuf;)

      /* Get the Slice Location from the IMAGE Header */
      // hack alert -- and this goes back to a hack in the original code
      // you read in an integer, but you DON'T byte swap it, and then pass
      // it into the MvtSunf function to get the floating point value.
      // to circumvent byte swapping in GetIntAt, use GetStringAt
      this->GetStringAt(f,IHDR_START * 2 + IHDR_LOCATN * 2,
      (char *)&intTmp,sizeof(int));

    hdr->sliceLocation = MvtSunf (intTmp);

    RGEDEBUG(std::sprintf (debugbuf, "Location = %f\n", hdr->sliceLocation); cerr << debugbuf;)

      this->GetStringAt(f,IHDR_START * 2 + IHDR_THICK * 2,
      (char *)&intTmp,sizeof(intTmp));

    hdr->sliceThickness = MvtSunf (intTmp);

    RGEDEBUG(std::sprintf (debugbuf, "Thickness = %f\n", hdr->sliceThickness); cerr << debugbuf;)

      /* Get the Slice Spacing from the IMAGE Header */
      this->GetStringAt(f, IHDR_START * 2 + IHDR_SPACE * 2,
      (char *)&intTmp,sizeof(int));


    hdr->sliceGap = MvtSunf (intTmp);

    RGEDEBUG(std::sprintf (debugbuf, "Slice Gap = %f\n", hdr->sliceGap); cerr << debugbuf;)

      /* Get TR from the IMAGE Header */
      this->GetStringAt(f, IHDR_START * 2 + IHDR_TR * 2,
      (char *)&intTmp,sizeof(int));


    hdr->TR = MvtSunf (intTmp);

    RGEDEBUG(std::sprintf (debugbuf, "TR = %f\n", hdr->TR); cerr << debugbuf;)

      /* Get TE from the IMAGE Header */
      this->GetStringAt(f, IHDR_START * 2 + IHDR_TE * 2,
      (char *)&intTmp,sizeof(int));



    hdr->TE = MvtSunf (intTmp);
    RGEDEBUG(std::sprintf (debugbuf, "TE = %f\n", hdr->TE); cerr << debugbuf;)

      /* Get TI from the IMAGE Header */
      this->GetStringAt(f, IHDR_START * 2 + IHDR_TI * 2,
      (char *)&intTmp,sizeof(int));



    hdr->TI = MvtSunf (intTmp);
    RGEDEBUG(std::sprintf (debugbuf, "TI = %f\n", hdr->TI); cerr << debugbuf;)

      /* Get Number of Echos from the IMAGE Header */
      this->GetShortAt(f, IHDR_START * 2 + IHDR_NECHO * 2, &(hdr->numberOfEchoes));
    RGEDEBUG(std::sprintf (debugbuf, "Number of Echos = %d\n", hdr->numberOfEchoes); cerr << debugbuf;)

      /* Get Echo Number from the IMAGE Header */
      this->GetShortAt(f, IHDR_START * 2 + IHDR_ECHON * 2,&(hdr->echoNumber));
    RGEDEBUG(std::sprintf (debugbuf, "Echo Number = %d\n", hdr->echoNumber); cerr << debugbuf;)

      /* Get PSD-Name from the IMAGE Header */
      this->GetStringAt(f, IHDR_START * 2 + IHDR_IHDR_DNAME * 2,tmpStr, 12);
    tmpStr[12] = '\0';
    RGEDEBUG(std::sprintf (debugbuf, "PSD Name = %s\n", tmpStr); cerr << debugbuf;)

      /* Get X Pixel Dimension from the IMAGE Header */
      this->GetShortAt(f, IHDR_START * 2 + IHDR_X * 2,&(hdr->imageXsize));
    RGEDEBUG(std::sprintf (debugbuf, "X Pixel Dimension = %d\n", hdr->imageXsize); cerr << debugbuf;)

      /* Get Y Pixel Dimension from the IMAGE Header */
      this->GetShortAt(f, IHDR_START * 2 + IHDR_Y * 2,&(hdr->imageYsize));
    RGEDEBUG(std::sprintf (debugbuf, "Y Pixel Dimension = %d\n", hdr->imageYsize); cerr << debugbuf;)

      /* Get Pixel Size from the IMAGE Header */
      this->GetStringAt(f, IHDR_START * 2 + IHDR_PIXSIZ * 2,
      (char *)&intTmp,sizeof(int));



    hdr->imageXres = MvtSunf (intTmp);
    hdr->imageYres = hdr->imageXres;
    RGEDEBUG(std::sprintf (debugbuf, "Pixel Size = %fx%f\n", hdr->imageXres, hdr->imageYres); cerr << debugbuf;)

      /* Get NEX from the IMAGE Header */
      this->GetStringAt(f, IHDR_START * 2 + IHDR_NEX * 2,
      (char *)&intTmp,sizeof(int));



    hdr->NEX = (short)MvtSunf (intTmp);
    RGEDEBUG(std::sprintf (debugbuf, "NEX = %d\n", hdr->NEX); cerr << debugbuf;)

      /* Get Flip Angle from the IMAGE Header */
      this->GetShortAt(f, IHDR_START * 2 + IHDR_FLPANG * 2,&tmpShort);

    if (tmpShort > 0)
      {
  hdr->flipAngle = (int) tmpShort;
      }
    else
      {
  hdr->flipAngle = 90;
      }
    RGEDEBUG(std::sprintf (debugbuf, "Flip Angle = %d\n", hdr->flipAngle); cerr << debugbuf;)

      //DEBUG: HACK -- what should pulse sequence be?  Is it valid for 4x filters
      // Just setting it to dummy value -- Hans
      //copy from ge5x strncpy (hdr->pulseSequence, &hdr[IM_HDR_START + IM_PSDNAME], 31);
      strncpy (hdr->pulseSequence, "UNKNOWN_GE4x_PULSE_SEQUENCE", 31);
    hdr->pulseSequence[31] = '\0';


  
    /* Get the Number of Images from the IMAGE Header */
    this->GetShortAt(f, IHDR_START * 2 + IHDR_SLQUANT * 2,&(hdr->numberOfSlices));
    RGEDEBUG(std::sprintf (debugbuf, "Number of SLices = %d\n", hdr->numberOfSlices); cerr << debugbuf;)

      //    status = stat (imageFile, &statBuf);
      //    if (status == -1)
      //      {
      //  return (NULL);
      //      }
      //
      //    hdr->offset = statBuf.st_size - (hdr->imageXsize * hdr->imageYsize * 2);
      //
      // find file length in line ...
      unsigned long file_length = itk::IOCommon::FileLength(FileNameToRead);

    hdr->offset = file_length -
      (hdr->imageXsize * hdr->imageYsize * 2);
    return hdr;
  }
  float GE4ImageIO
  ::  MvtSunf (int numb)
  {
    float x;
    int dg_exp, dg_sign, dg_mantissa;
    int sun_exp, sun_num;
#define signbit 020000000000
#define dmantissa 077777777
#define dexponent 0177
#define dmantlen 24
#define smantissa 037777777
#define sexponent 0377
#define smantlen 23
    ByteSwapper<int>::SwapFromSystemToBigEndian(&numb);
    dg_exp = (numb >> 24) & dexponent;
    dg_sign = numb & signbit;
    dg_mantissa = (numb & dmantissa) << 8;
    sun_exp = 4 * (dg_exp - 64);
    while ((dg_mantissa & signbit) == 0 && dg_mantissa != 0)
      {
  sun_exp--;
  dg_mantissa = dg_mantissa << 1;
      }
    sun_num = 0;
    sun_exp += 126;
    if (sun_exp < 0)
      {
  sun_exp = 0;
      }
    else if (sun_exp > 255)
      {
  sun_exp = 255;
      }
    dg_mantissa = dg_mantissa << 1;
    sun_num = dg_sign | (sun_exp << smantlen) | ((dg_mantissa >> 9) & smantissa);
    memcpy ((void *) &x, (void *) &sun_num, sizeof(x));
    return (x);
  }

} // end namespace itk
