/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGE5ImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/
#include "itkGE5ImageIO.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkDirectory.h"
#include <iostream>
#include <fstream>
#include <string.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <vector>
#include <string>

#include "pixeldata.h"
#include "genesis_hdr_def.h"
#include "itkGEImageHeader.h"
#include "itkIOCommon.h"

//From uiig library "The University of Iowa Imaging Group-UIIG"

namespace itk 
{
static const char GE_PROD_STR[]="SIGNA";
  // Default constructor
  GE5ImageIO::GE5ImageIO()
  {
  }

  GE5ImageIO::~GE5ImageIO()
  {
    //Purposefully left blank
    
  }

  static int checkGe5xImages (char const * const imageFileTemplate)
  {
    //
    // Does it exist?
    if(!itk::IOCommon::FileExists(imageFileTemplate))
      {
  return -1;
      }
    //
    // is it at least 5000 bytes?
    if(itk::IOCommon::FileLength(imageFileTemplate) < 5000)
      {
  return -1;
      }


    std::ifstream f(imageFileTemplate,std::ifstream::binary);
    if(!f.is_open())
      return -1;

    PixHdr imageHdr;              /* Header Structure for GE 5x images */
    char hdr[SU_HDR_LEN];         /* Header to hold GE Suite header */
    char prod[16];                /* Product name from Suite Header */

    // First pass see if image is a raw MR extracted via ximg
    f.read((char *)&imageHdr,sizeof(imageHdr));
    if(f.fail())
      {
  return -1;
      }
    ByteSwapper<int>::SwapFromSystemToBigEndian(&imageHdr.img_magic);
    if (imageHdr.img_magic == IMG_MAGIC)
    {
      return 0;
    }
    f.seekg(0,std::ios::beg);

    //
    // Second pass see if image was extracted via tape by Gene's tape
    // reading software.
    //
    f.read(hdr,SU_HDR_LEN);
    if(f.fail())
      {
  return -1;
      }
    strncpy (prod, hdr+SU_PRODID, 13);
    prod[13] = '\0';
    if (strcmp (prod, GE_PROD_STR) == 0)
    {
      return 0;
    }

    return -1;
}

  bool GE5ImageIO::CanReadFile( const char* FileNameToRead )
  {
    this->SetFileName(FileNameToRead);
    return checkGe5xImages(FileNameToRead) == 0 ? true : false;
  }

  static void
  swapPixHdr (PixHdr * hdr)
  {
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_magic));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_hdr_length));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_width));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_height));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_depth));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_compress));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_dwindow));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_dlevel));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_bgshade));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_ovrflow));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_undflow));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_top_offset));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_bot_offset));
    ByteSwapper<short int>::SwapFromSystemToBigEndian (&(hdr->img_version));
    ByteSwapper<unsigned short>::SwapFromSystemToBigEndian (&(hdr->img_checksum));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_id));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_id));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_unpack));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_unpack));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_compress));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_compress));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_histo));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_histo));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_text));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_text));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_graphics));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_graphics));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_dbHdr));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_dbHdr));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_levelOffset));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_user));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_user));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_suite));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_suite));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_exam));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_exam));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_series));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_series));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_p_image));
    ByteSwapper<int>::SwapFromSystemToBigEndian (&(hdr->img_l_image));

    return;
  }


  struct GEImageHeader *
  GE5ImageIO::ReadHeader (const char  *FileNameToRead)
  {
#define VERBOSE_DEBUGGING
#if defined(VERBOSE_DEBUGGING)
#define RGEDEBUG(x) x
#else
#define RGEDEBUG(x)
#endif

    PixHdr imageHdr;              /* Header Structure for GE 5x images */
    char hdr[IM_HDR_START + IM_HDR_LEN];  /* Header to hold GE header */
    struct GEImageHeader *curImage;
    bool pixelHdrFlag;
    int timeStamp;
    char tmpId[64];
    char *ptr;

    curImage = new GEImageHeader;
    if (curImage == NULL)
      RAISE_EXCEPTION();
    memset(curImage,0,sizeof(struct GEImageHeader));
    pixelHdrFlag = false;

  
    std::ifstream f(FileNameToRead,std::ifstream::binary);
    if(!f.is_open())
      RAISE_EXCEPTION();
    f.read((char *)&imageHdr,sizeof(imageHdr));
    IOCHECK();

    swapPixHdr(&imageHdr);

    if (imageHdr.img_magic == IMG_MAGIC)
      {
  f.seekg(imageHdr.img_p_suite,std::ios::beg);
  IOCHECK();
        pixelHdrFlag = true;
      }
    else
      {
  f.seekg(0,std::ios::beg);
      }
    f.read(hdr,IM_HDR_START + IM_HDR_LEN);
    IOCHECK();

    /* Set Patient-Name */
    strncpy (curImage->name, &hdr[EX_HDR_START + EX_PATNAME], EX_PATAGE - EX_PATNAME + 1);
    sprintf (curImage->scanner, "GE-5X");

    /* Set Hospital-Name */
    strncpy (curImage->hospital, &hdr[EX_HDR_START + EX_HOSPNAME], EX_DETECT - EX_HOSPNAME + 1);

    /* Set Images-Per-Slice */
    curImage->imagesPerSlice= hdr2Short(&hdr[IM_HDR_START + IM_CPHASE]);

    /* Set Date */
    timeStamp = hdr2Int (&hdr[SE_HDR_START + SE_ACTUAL_DT]);
    statTimeToAscii (&timeStamp, curImage->date);

    /* Set Patient-Id */
    strncpy (tmpId, &hdr[EX_HDR_START + EX_PATID], EX_PATNAME - EX_PATID + 1);
    tmpId[EX_PATNAME - EX_PATID + 1] = '\0';
    curImage->patientId[0] = '\0';

    ptr = strtok (tmpId, "-");

    while (ptr != NULL)
      {
        strcat (curImage->patientId, ptr);
        ptr = strtok (NULL, "-");
      }

    RGEDEBUG(fprintf (stderr, "Id <%s>\n", curImage->patientId);)

    curImage->seriesNumber = hdr2Short (&hdr[SE_HDR_START + SE_NO]);
    RGEDEBUG(fprintf (stderr, "Series Number %d\n", curImage->seriesNumber);)

    curImage->imageNumber = hdr2Short (&hdr[IM_HDR_START + IM_NO]);
    RGEDEBUG(fprintf (stderr, "Image Number %d\n", curImage->imageNumber);)

    curImage->sliceThickness = hdr2Float (&hdr[IM_HDR_START + IM_SLTHICK]);
    RGEDEBUG(fprintf (stderr, "Thickness %f\n", curImage->sliceThickness);)

    curImage->imageXsize = hdr2Short (&hdr[IM_HDR_START + IM_IMATRIX_X]);
    curImage->imageYsize = hdr2Short (&hdr[IM_HDR_START + IM_IMATRIX_Y]);
    RGEDEBUG(fprintf (stderr, "Acq Size %dx%d\n", curImage->acqXsize, curImage->acqYsize);)

    curImage->xFOV = hdr2Float (&hdr[IM_HDR_START + IM_DFOV]);
    curImage->yFOV = hdr2Float (&hdr[IM_HDR_START + IM_DFOV]);
    if (curImage->yFOV == 0.0)
      {
        curImage->yFOV = curImage->xFOV;
      }
    RGEDEBUG(fprintf (stderr, "FOV %fx%f\n", curImage->xFOV, curImage->yFOV);)

    curImage->acqXsize = (int) hdr2Float (&hdr[IM_HDR_START + IM_DIM_X]);
    curImage->acqYsize = (int) hdr2Float (&hdr[IM_HDR_START + IM_DIM_Y]);
    RGEDEBUG(fprintf (stderr, "Image Size %dx%d\n", curImage->imageXsize, curImage->imageYsize);)

    curImage->imageXres = hdr2Float (&hdr[IM_HDR_START + IM_PIXSIZE_X]);
    curImage->imageYres = hdr2Float (&hdr[IM_HDR_START + IM_PIXSIZE_Y]);
    RGEDEBUG(fprintf (stderr, "Image Res %fx%f\n", curImage->imageXres, curImage->imageYres);)

    curImage->imagePlane = hdr2Short (&hdr[IM_HDR_START + IM_PLANE]);
    RGEDEBUG(fprintf (stderr, "Plane %d\n", curImage->imagePlane);)

    //RECODE image plane to be brains2 compliant.!!
    switch (curImage->imagePlane)
      {
      case GE_CORONAL:
        curImage->imagePlane = itk::GE5ImageIO::CORONAL;
        break;
      case GE_SAGITTAL:
        curImage->imagePlane = itk::GE5ImageIO::SAGITTAL;
        break;
      case GE_AXIAL:
        curImage->imagePlane = itk::GE5ImageIO::AXIAL;
        break;
      default:
        curImage->imagePlane = itk::GE5ImageIO::CORONAL;
        break;
      }
    curImage->sliceLocation = hdr2Float (&hdr[IM_HDR_START + IM_LOC]);
    RGEDEBUG(fprintf (stderr, "Location %f %c %c\n", curImage->sliceLocation, hdr[IM_HDR_START + IM_LOC_RAS], hdr[IM_HDR_START + IM_LOC_RAS+1]);)

    curImage->TR = hdr2Int (&hdr[IM_HDR_START + IM_TR]) / 1000.0;
    curImage->TI = hdr2Int (&hdr[IM_HDR_START + IM_TI]) / 1000.0;
    curImage->TE = hdr2Int (&hdr[IM_HDR_START + IM_TE]) / 1000.0;
    curImage->TE2 = hdr2Int (&hdr[IM_HDR_START + IM_TE2]) / 1000.0;
    RGEDEBUG(fprintf (stderr, "TR %f, TI %f, TE %f, TE2 %f\n", curImage->TR, curImage->TI, curImage->TE, curImage->TE2);)

    curImage->numberOfEchoes = hdr2Short (&hdr[IM_HDR_START + IM_NUMECHO]);
    curImage->echoNumber = hdr2Short (&hdr[IM_HDR_START + IM_ECHONUM]);

    if (curImage->numberOfEchoes == 0)
      curImage->numberOfEchoes = 1;
    RGEDEBUG(fprintf (stderr, "Echos %d,  Number %d\n", curImage->numberOfEchoes, curImage->echoNumber);)

    curImage->NEX = (int) hdr2Float (&hdr[IM_HDR_START + IM_NEX]);
    RGEDEBUG(fprintf (stderr, "NEX %d\n", curImage->NEX);)

    curImage->flipAngle = hdr2Short (&hdr[IM_HDR_START + IM_MR_FLIP]);
    RGEDEBUG(fprintf (stderr, "Flip Angle %d\n", curImage->flipAngle);)

    strncpy (curImage->pulseSequence, &hdr[IM_HDR_START + IM_PSDNAME], 31);
    curImage->pulseSequence[31] = '\0';
    RGEDEBUG(fprintf (stderr, "Sequence %s\n", curImage->pulseSequence);)

    curImage->numberOfSlices = hdr2Short (&hdr[IM_HDR_START + IM_SLQUANT]);
    RGEDEBUG(fprintf (stderr, "Number Of Slices %d\n", curImage->numberOfSlices);)

      if (pixelHdrFlag)
      {
        curImage->offset = imageHdr.img_hdr_length;
      }
    else
      {
        curImage->offset = itk::IOCommon::FileLength(FileNameToRead) - (curImage->imageXsize * curImage->imageYsize * 2);
      }

    strncpy (curImage->filename,FileNameToRead, IOCommon::MAXPATHLEN+1);

    return (curImage);
  }

} // end namespace itk
