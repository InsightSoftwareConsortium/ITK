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
#ifndef __itkTIFFImageIO_h
#define __itkTIFFImageIO_h
#include "ITKIOTIFFExport.h"

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
 * \ingroup ITKIOTIFF
 *
 * \wiki
 * \wikiexample{IO/TIFFImageIO,Write a TIFF image}
 * \endwiki
 */
class ITKIOTIFF_EXPORT TIFFImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef TIFFImageIO          Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TIFFImageIO, ImageIOBase);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /** Reads 3D data from multi-pages tiff. */
  virtual void ReadVolume(void *buffer);

  /** Reads 3D data from tiled tiff. */
  virtual void ReadTiles(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

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
    // Doing it this way is probably also less likely to break in the future.
    if ( compression == NoCompression )
      {
      this->SetUseCompression(false); // this is for the ImageIOBase class
      }
    else
      {
      this->SetUseCompression(true);  // this is for the ImageIOBase class
      }
  }

  /** Set/Get the level of quality for the output images if
    * Compression is JPEG. Settings vary from 1 to 100.
    * 100 is the highest quality. Default is 75 */
  itkSetClampMacro(JPEGQuality, int, 1, 100);
  itkGetConstMacro(JPEGQuality, int);

protected:
  TIFFImageIO();
  ~TIFFImageIO();
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void InternalWrite(const void *buffer);

  void InitializeColors();

  void ReadGenericImage(void *out,
                        unsigned int width,
                        unsigned int height);

  // To support Zeiss images
  void ReadTwoSamplesPerPixelImage(void *out,
                                   unsigned int width,
                                   unsigned int height);

  int EvaluateImageAt(void *out, void *in);

  unsigned int  GetFormat();

  void GetColor(int index, unsigned short *red,
                unsigned short *green, unsigned short *blue);

  // Check that tag t can be found
  bool  CanFindTIFFTag(unsigned int t);

  // Read and returns the raw bytes of tag t
  void * ReadRawByteFromTag(unsigned int t, unsigned int & value_count);

  TIFFReaderInternal *m_InternalImage;

  void ReadTiffInfo();

  int m_Compression;
  int m_JPEGQuality;

private:
  TIFFImageIO(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void ReadCurrentPage(void *out, size_t pixelOffset);

  template <typename TComponent>
  void ReadGenericImage(void *out,
                        unsigned int width,
                        unsigned int height);

  template <typename TComponent>
    void RGBAImageToBuffer( void *out, const uint32_t *tempImage );

  unsigned short *m_ColorRed;
  unsigned short *m_ColorGreen;
  unsigned short *m_ColorBlue;
  int             m_TotalColors;
  unsigned int    m_ImageFormat;
};
} // end namespace itk

#endif // __itkTIFFImageIO_h
