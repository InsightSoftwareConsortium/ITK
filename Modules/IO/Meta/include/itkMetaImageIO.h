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
#ifndef itkMetaImageIO_h
#define itkMetaImageIO_h
#include "ITKIOMetaExport.h"


#include <fstream>
#include "itkImageIOBase.h"
#include "metaObject.h"
#include "metaImage.h"

namespace itk
{
/** \class MetaImageIO
 *
 *  \brief Read MetaImage file format.
 *
 *  For a detailed description of using this format, please see
 *  https://www.itk.org/Wiki/ITK/MetaIO/Documentation
 *
 *  \ingroup IOFilters
 * \ingroup ITKIOMeta
 */
class ITKIOMeta_EXPORT MetaImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef MetaImageIO          Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaImageIO, Superclass);

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D. This method returns
   * true/false as to whether the ImageIO can support the dimension
   * indicated. */
  virtual bool SupportsDimension(unsigned long) ITK_OVERRIDE
  {
    return true;
  }

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  MetaImage * GetMetaImagePointer();

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  virtual bool CanWriteFile(const char *) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

  /** Set the filename for the Data file. Setting this will make the
      Writer to use the non-Local mode and save header and data in
      independent files */
  virtual void SetDataFileName(const char *filename);

  /** set the precision in the MetaImage member
   */
  virtual void SetDoublePrecision(unsigned int precision)
  {
    m_MetaImage.SetDoublePrecision(precision);
  }

  /** Method for supporting streaming.  Given a requested region, calculate what
   * could be the region that we can read from the file. This is called the
   * streamable region, which will be smaller than the LargestPossibleRegion and
   * greater or equal to the RequestedRegion */
  virtual ImageIORegion
  GenerateStreamableReadRegionFromRequestedRegion(const ImageIORegion & requested) const ITK_OVERRIDE;

  virtual unsigned int
  GetActualNumberOfSplitsForWriting(unsigned int numberOfRequestedSplits,
                                    const ImageIORegion & pasteRegion,
                                    const ImageIORegion & largestPossibleRegion) ITK_OVERRIDE;

  virtual ImageIORegion
  GetSplitRegionForWriting(unsigned int ithPiece,
                           unsigned int numberOfActualSplits,
                           const ImageIORegion & pasteRegion,
                           const ImageIORegion & largestPossibleRegion) ITK_OVERRIDE;

  /** Determine if the ImageIO can stream reading from this
   *  file. Only time cannot stream read/write is if compression is used.
   *  CanRead must be called prior to this function. */
  virtual bool CanStreamRead() ITK_OVERRIDE
  {
    if ( m_MetaImage.CompressedData() )
      {
      return false;
      }
    return true;
  }

  /** Determine if the ImageIO can stream writing to this
   *  file. Only time cannot stream read/write is if compression is used.
   *  Assumes file passes a CanRead call and its pixels are of the same
   *  type as the template of the writer. Can verify by first calling
   *  CanRead and then CanStreamRead prior to calling CanStreamWrite. */
  virtual bool CanStreamWrite() ITK_OVERRIDE
  {
    if ( this->GetUseCompression() )
      {
      return false;
      }
    return true;
  }

  /** Determing the subsampling factor in case
   *  we want a coarse version of the image/
   * \warning this is only used when streaming is on. */
  itkSetMacro(SubSamplingFactor, unsigned int);
  itkGetConstMacro(SubSamplingFactor, unsigned int);

  /**
   * Set the default precision when writing out the MetaImage header.
   * MetaImage header contains values stored in memory as double,
   * use this precision when writing out the value. The precision
   * should only be defined in the range [0, 17] since a value of 17
   * will make the conversion of a double floating-point to text and
   * back exact.
   * This function is not thread safe.
   * Default value after static initialization is 17.
   */
  static void SetDefaultDoublePrecision(unsigned int precision);
  static unsigned int GetDefaultDoublePrecision();

protected:
  MetaImageIO();
  ~MetaImageIO() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  MetaImage m_MetaImage;

  ITK_DISALLOW_COPY_AND_ASSIGN(MetaImageIO);

  unsigned int m_SubSamplingFactor;

  static unsigned int m_DefaultDoublePrecision;
};
} // end namespace itk

#endif // itkMetaImageIO_h
