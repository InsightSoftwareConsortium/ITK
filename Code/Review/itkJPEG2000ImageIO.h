/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkJPEG2000ImageIO.h,v $
  Language:  C++
  Date:      $Date: 2007/03/22 14:28:51 $
  Version:   $Revision: 1.31 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkJPEG2000ImageIO_h
#define __itkJPEG2000ImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkStreamingImageIOBase.h"

#define USE_OPJ_DEPRECATED

#ifndef ITK_BUILD_SHARED_LIBS
# define OPJ_STATIC
#endif

extern "C"
{
  #include "openjpeg.h"
  #include "j2k.h"
  #include "jp2.h"
}

namespace itk
{
/** \class JPEG2000ImageIO
 *
 * \brief Supports for the JPEG2000 file format based on openjpeg
 *
 *  JPEG2000 offers a large collection of interesting features including:
 *  compression (lossless and lossy), streaming, multi-channel images.
 *
 *  \ingroup IOFilters
 */
class ITK_EXPORT JPEG2000ImageIO:public StreamingImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef JPEG2000ImageIO      Self;
  typedef StreamingImageIOBase Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JPEG2000ImageIO, StreamingImageIOBase);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer);

  /** Method for supporting streaming.  Given a requested region, determine what
   * could be the region that we can read from the file. This is called the
   * streamable region, which will be smaller than the LargestPossibleRegion and
   * greater or equal to the RequestedRegion */
  virtual ImageIORegion
  GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requested) const;

  /** Method required by the base class StreamingImageIOBase */
  virtual SizeType GetHeaderSize(void) const;

  void SetTileSize(int x, int y)
  {
    m_TileWidth = x;
    m_TileHeight = y;
  }

  /** Currently JPEG2000 does not support streamed writing
   *
   * These methods are re-overridden to not support streaming for
   * now...
   */
  virtual bool CanStreamWrite( void );
  virtual unsigned int GetActualNumberOfSplitsForWriting( unsigned int numberOfRequestedSplits,
                                                          const ImageIORegion &pasteRegion,
                                                          const ImageIORegion &largestPossibleRegion );

protected:
  JPEG2000ImageIO();
  ~JPEG2000ImageIO();
  void PrintSelf(std::ostream & os, Indent indent) const;

  opj_dparameters_t m_DecompressionParameters;  /* decompression parameters */
private:
  JPEG2000ImageIO(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented

  typedef enum {
    J2K_CFMT = 0,
    JP2_CFMT = 1,
    JPT_CFMT = 2,
    MJ2_CFMT = 3
    } DecodingFormatType;

  typedef enum {
    PXM_DFMT = 0,
    PGX_DFMT = 1,
    BMP_DFMT = 2,
    YUV_DFMT = 3
    } DFMFormatType;

  opj_codec_t *m_Dinfo;

  OPJ_UINT32 m_TileWidth;
  OPJ_UINT32 m_TileHeight;

  OPJ_UINT32 m_TileStartX;
  OPJ_UINT32 m_TileStartY;

  OPJ_UINT32 m_NumberOfTilesInX;
  OPJ_UINT32 m_NumberOfTilesInY;

  typedef ImageIORegion::SizeValueType  SizeValueType;
  typedef ImageIORegion::IndexValueType IndexValueType;

  void ComputeRegionInTileBoundaries(unsigned int dimension,
                                     SizeValueType tileSize, ImageIORegion & streamableRegion) const;
};
} // end namespace itk

#endif // __itkJPEG2000ImageIO_h
