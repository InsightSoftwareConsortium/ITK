/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkTIFFImageIO_h
#define itkTIFFImageIO_h
#include "ITKIOTIFFExport.h"

#include "itkImageIOBase.h"
#include <fstream>

namespace itk
{
// BTX
class TIFFReaderInternal;
// ETX

/**
 *\class TIFFImageIO
 *
 * \brief ImageIO object for reading and writing TIFF images
 *
 * The compressors supported include "PackBits" (default), "JPEG",
 * "DEFLATE" and may also include "LZW". Only the "JPEG" compressor
 * supports the compression level for JPEG quality parameter in the
 * range 0-100.
 *
 * \ingroup IOFilters
 * \ingroup ITKIOTIFF
 *
 * \sphinx
 * \sphinxexample{IO/TIFF/WriteATIFFImage,Write A TIFF Image}
 * \endsphinx
 */
class ITKIOTIFF_EXPORT TIFFImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TIFFImageIO);

  /** Standard class type aliases. */
  using Self = TIFFImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  using RGBPixelType = RGBPixel<unsigned short>;
  using PaletteType = std::vector<RGBPixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TIFFImageIO, ImageIOBase);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanReadFile(const char *) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /** Reads 3D data from multi-pages tiff. */
  virtual void
  ReadVolume(void * buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Writes the spacing and dimensions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  void
  Write(const void * buffer) override;

  enum
  {
    NOFORMAT,
    RGB_,
    GRAYSCALE,
    PALETTE_RGB,
    PALETTE_GRAYSCALE,
    OTHER
  };

  // BTX
  enum
  { // Compression types
    NoCompression,
    PackBits,
    JPEG,
    Deflate,
    LZW
  };
  // ETX

  /** \brief Set type and automatically enable/disable compression.
   *
   * Since LZW compression is patented outside US, the additional work
   * steps have to be taken in order to use that compression.  */
  void
  SetCompressionToNoCompression()
  {
    this->UseCompressionOff();
    this->SetCompressor("NoCompression");
  }
  void
  SetCompressionToPackBits()
  {
    this->UseCompressionOn();
    this->SetCompressor("PackBits");
  }
  void
  SetCompressionToJPEG()
  {
    this->UseCompressionOn();
    this->SetCompressor("JPEG");
  }
  void
  SetCompressionToDeflate()
  {
    this->UseCompressionOn();
    this->SetCompressor("Deflate");
  }
  void
  SetCompressionToLZW()
  {
    this->UseCompressionOn();
    this->SetCompressor("LZW");
  }


  /** Set/Get the level of quality for the output images if
   * Compression is JPEG. Settings vary from 1 to 100.
   * 100 is the highest quality. Default is 75 */
  virtual void
  SetJPEGQuality(int _JPEGQuality)
  {
    this->SetCompressionLevel(_JPEGQuality);
  }
  virtual int
  GetJPEGQuality() const
  {
    return this->GetCompressionLevel();
  }


  /** Get a const ref to the palette of the image. In the case of non palette
   * image or ExpandRGBPalette set to true, a vector of size
   * 0 is returned.
   * For multipage Images, only the palette of the first page is read */
  itkGetConstReferenceMacro(ColorPalette, PaletteType);

  /** Set the palette of the image.
   * For multipage images, the same palette is going to be used for all pages */
  void
  SetColorPalette(const PaletteType _arg)
  {
    if (this->m_ColorPalette != _arg)
    {
      this->m_ColorPalette = _arg;
      this->Modified();
    }
  }

protected:
  TIFFImageIO();
  ~TIFFImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  InternalSetCompressor(const std::string & _compressor) override;

  // This method is protected because it does not keep
  // ImageIO::m_Compressor and TIFFImageIO::m_Compression in sync.
  void
  SetCompression(int compression)
  {
    m_Compression = compression;
  }

  void
  InternalWrite(const void * buffer);

  void
  InitializeColors();

  void
  ReadGenericImage(void * out, unsigned int width, unsigned int height);

  // To support Zeiss images
  void
  ReadTwoSamplesPerPixelImage(void * out, unsigned int width, unsigned int height);

  unsigned int
  GetFormat();

  void
  GetColor(uint64_t index, uint16_t * red, uint16_t * green, uint16_t * blue);

  // Check that tag t can be found
  bool
  CanFindTIFFTag(unsigned int t);

  // Read and returns the raw bytes of tag t
  void *
  ReadRawByteFromTag(unsigned int t, unsigned int & value_count);

  // Populate m_ColorPalette from the file palette
  // The palette corresponds to the one of the first page in case of multipage tiff
  void
  PopulateColorPalette();

  TIFFReaderInternal * m_InternalImage;

  void
  ReadTIFFTags();

  int m_Compression;

  PaletteType m_ColorPalette;

private:
  void
  AllocateTiffPalette(uint16_t bps);

  void
  ReadCurrentPage(void * out, size_t pixelOffset);

  template <typename TComponent>
  void
  ReadGenericImage(void * out, unsigned int width, unsigned int height);

  template <typename TComponent>
  void
  RGBAImageToBuffer(void * out, const uint32_t * tempImage);

  template <typename TType>
  void
  PutGrayscale(TType *      to,
               TType *      from,
               unsigned int xsize,
               unsigned int ysize,
               unsigned int toskew,
               unsigned int fromskew);

  template <typename TType>
  void
  PutRGB_(TType * to, TType * from, unsigned int xsize, unsigned int ysize, unsigned int toskew, unsigned int fromskew);


  template <typename TType, typename TFromType>
  void
  PutPaletteGrayscale(TType *      to,
                      TFromType *  from,
                      unsigned int xsize,
                      unsigned int ysize,
                      unsigned int toskew,
                      unsigned int fromskew);

  template <typename TType, typename TFromType>
  void
  PutPaletteRGB(TType *      to,
                TFromType *  from,
                unsigned int xsize,
                unsigned int ysize,
                unsigned int toskew,
                unsigned int fromskew);

  template <typename TType, typename TFromType>
  void
  PutPaletteScalar(TType *      to,
                   TFromType *  from,
                   unsigned int xsize,
                   unsigned int ysize,
                   unsigned int toskew,
                   unsigned int fromskew);

  uint16_t *   m_ColorRed;
  uint16_t *   m_ColorGreen;
  uint16_t *   m_ColorBlue;
  uint64_t     m_TotalColors{ 0 };
  unsigned int m_ImageFormat{ TIFFImageIO::NOFORMAT };
};
} // end namespace itk

#endif // itkTIFFImageIO_h
