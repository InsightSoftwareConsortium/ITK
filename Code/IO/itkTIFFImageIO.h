/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTIFFImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTIFFImageIO_h
#define __itkTIFFImageIO_h

#include "itkImageIOBase.h"
#include <fstream>

namespace itk
{

//BTX
class TIFFReaderInternal;
//ETX


/** \class TIFFImageIO
 *
 * \brief ImageIO object for reading and writing TIFF images
 *
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT TIFFImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef TIFFImageIO            Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TIFFImageIO, ImageIOBase);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char*);
  
  /** Set the spacing and diemention information for the set filename. */
  virtual void ReadImageInformation();
  
  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /** Reads 3D data from multi-pages tiff. */
  virtual void ReadVolume(void* buffer);

  /** Reads 3D data from tiled tiff*/
  virtual void ReadTiles(void* buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*);

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void* buffer);

  enum { NOFORMAT, RGB_, GRAYSCALE, PALETTE_RGB, PALETTE_GRAYSCALE, OTHER };

  //BTX
  enum { // Compression types
    NoCompression,
    PackBits,
    JPEG,
    Deflate,
    LZW
  };
  //ETX

  // Description:
  // Set compression type. Sinze LZW compression is patented outside US, the
  // additional work steps have to be taken in order to use that compression.
  void SetCompressionToNoCompression() { this->SetCompression(NoCompression); }
  void SetCompressionToPackBits()      { this->SetCompression(PackBits); }
  void SetCompressionToJPEG()          { this->SetCompression(JPEG); }
  void SetCompressionToDeflate()       { this->SetCompression(Deflate); }
  void SetCompressionToLZW()           { this->SetCompression(LZW); }

  void SetCompression(int compression) 
    {
    m_Compression = compression;
 
    // This If block isn't strictly necessary:
    // SetCompression(true); would be sufficient.  However, it reads strangely
    // for SetCompression(NoCompression) to then set SetCompression(true).
    // Doing it this way is probaly also less likely to break in the future.
    if (compression == NoCompression)
      {
      this->SetUseCompression(false); // this is for the ImageIOBase class
      }
    else
      {
      this->SetUseCompression(true);  // this is for the ImageIOBase class
      }
    }


protected:
  TIFFImageIO();
  ~TIFFImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

  void InternalWrite(const void* buffer);

  void InitializeColors();
  void ReadGenericImage( void *out, 
                         unsigned int itkNotUsed(width), 
                         unsigned int height );

  // To support Zeiss images
  void ReadTwoSamplesPerPixelImage( void *out, 
                         unsigned int itkNotUsed(width), 
                         unsigned int height );


  int EvaluateImageAt( void* out, void* in );

  unsigned int  GetFormat();

  void GetColor( int index, unsigned short *red, 
                 unsigned short *green, unsigned short *blue );
  // Check that tag t can be found
  bool  CanFindTIFFTag(unsigned int t);
  // Read and returns the raw bytes of tag t
  void* ReadRawByteFromTag(unsigned int t);

  TIFFReaderInternal * m_InternalImage;
  int m_Compression;
private:
  TIFFImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned short *ColorRed;
  unsigned short *ColorGreen;
  unsigned short *ColorBlue;
  int TotalColors;
  unsigned int ImageFormat;
};

} // end namespace itk

#endif // __itkTIFFImageIO_h

