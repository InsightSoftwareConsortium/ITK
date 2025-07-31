/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkNrrdImageIO_h
#define itkNrrdImageIO_h
#include "ITKIONRRDExport.h"


#include "itkImageIOBase.h"
#include <fstream>

struct NrrdEncoding_t;

namespace itk
{
/** \class NrrdImageIOEnums
 * \brief Contains all enum classes used by NrrdImageIO class.
 * \ingroup ITKIONRRD
 */
class NrrdImageIOEnums
{
public:
  /** \class AxesReorder
   * \ingroup ITKIONRRD
   * Enum type that specifies how to reorder axes in NRRD file when reading it into an ITK image.  */
  enum class AxesReorder : uint8_t
  {
    UseAnyRangeAxisAsPixel = 0,
    UseNonListRangeAxisAsPixel = 1,
    UseScalarPixel = 2
  };
};
// Define how to print enumeration
extern ITKIONRRD_EXPORT std::ostream &
                        operator<<(std::ostream & out, const NrrdImageIOEnums::AxesReorder value);

using AxesReorderEnum = NrrdImageIOEnums::AxesReorder;

/**
 * \class NrrdImageIO
 *
 * \brief Read and write the "Nearly Raw Raster Data" (Nrrd) image format.
 * The Nrrd format was developed as part of the Teem package
 * (teem.sourceforge.net).
 *
 * The compressor supported may include "gzip" (default) and
 * "bzip2".  Only the "gzip" compressor support the compression level
 * in the range 0-9.
 *
 * The NRRD file format supports two types of metadata,
 * fields, "<field>:<desc>" and key/value pairs, "<key>:=<value>"
 * (https://teem.sourceforge.net/nrrd/format.html#general.2).
 * General, non NRRD specific, metadata entries added to an
 * image's metadata dictionary will be written to file using key/value
 * notation.
 * NRRD specific metadata entries must be added to the image's
 * metadata dictionary using "NRRD_<field>" as the key. For example,
 * the information for the NRRD "content" field is added using the key
 * "NRRD_content". When a field consists of multiple values, per axis
 * fields (https://teem.sourceforge.net/nrrd/format.html#per-axis),
 * a per axis key is used. For example, to set the information in the
 * image's metadata dictionary so that when written to file the
 * result is, "thicknesses: 0.7 0.8 0.9", we need to add three entries
 * to the dictionary with keys: "NRRD_thicknesses[0]",
 * "NRRD_thicknesses[1]" and "NRRD_thicknesses[2]".
 *
 *  \ingroup IOFilters
 * \ingroup ITKIONRRD
 */
class ITKIONRRD_EXPORT NrrdImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NrrdImageIO);

  /** Standard class type aliases. */
  using Self = NrrdImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(NrrdImageIO);

  /** The different types of ImageIO's can support data of varying
   * dimensionality. For example, some file formats are strictly 2D
   * while others can support 2D, 3D, or even n-D. This method returns
   * true/false as to whether the ImageIO can support the dimension
   * indicated. */
  bool
  SupportsDimension(unsigned long) override;

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

  /** Determine the file type. Returns true if this ImageIO can write the
   * file specified. */
  bool
  CanWriteFile(const char *) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void
  Write(const void * buffer) override;

  /** Set/Get the strategy for axis reordering.
   * ITK image can be an Image object that can store scalar, RGB, CovariantVector, etc. pixels in an arbitrarily high
   * dimensional array. If the number of components of the pixel has to be dynamic then the ITK image has to be a
   * VectorImage object that can store multi-component pixels in an arbitrarily high dimensional array.
   *
   * NRRD file can contain domain (space, time) axes and range (list, RGB, covariant-vector, etc.) axes in an arbitrary
   * order. Therefore, axes of the NRRD file may have to be reordered to be able to read the NRRD file into an ITK
   * image. In general, NRRD domain axes are mapped to ITK image axes then all range axes are used as additional image
   * dimensions. The only exception is that it makes sense to pick one a range axis and use that as pixel component.
   *
   * In early ITK versions, any kind of NRRD range axis was used as pixel component by default (this is the
   * UseAnyRangeAxisAsPixel strategy). However, this resulted in more complicated application code (developers need to
   * instantiate an Image or VectorImage depending on what kind of axes are in the NRRD file) and potential unnecessary
   * reordering of axes (e.g., sequence of 3D images was always converted into a single multi-component 3D image).
   *
   * Therefore additional strategies were introduced to allow more control over how NRRD axes are mapped to ITK image
   * axes and pixel component. UseNonListRangeAxisAsPixel to ensure that the image can be always read into an Image
   * object. UseScalarPixel to ensure that the image can be always read into an Image object with a scalar pixel type.
   *
   * \li UseAnyRangeAxisAsPixel Use the first a range axis (i.e., non-domain axis) of the NRRD file as pixel component.
   *     This is the default (for backward compatibility).
   *     If the NRRD file contains a non-list type (e.g., RGB, point, covariant-vector) range axis then the first one
   * will be used as pixel component; the image will be read into an Image object. If the NRRD file only contains
   * list-type range axis then it will be used as pixel component; the image will be read into a VectorImage.
   * \li UseNonListRangeAxisAsPixel Use the first non-list range axis (i.e., non-domain axis) of the NRRD file as pixel
   * component. The image is always read into an Image object, with potentially a non-scalar pixel type.
   * \li UseScalarPixel Pixel type is scalar. All range (i.e., non-domain) axes of the NRRD file are added as additional
   * domain axes. The image is always read into an Image object with a scalar pixel type.
   *
   * Examples:
   *
   *   NRRD file: list domain domain domain RGB-Color (or: domain domain domain list RGB-Color)
   *   - UseAnyRangeAxisAsPixel:     Image<RGBPixel<double>,4> with axes: domain domain domain list
   *   - UseNonListRangeAxisAsPixel: Image<RGBPixel<double>,4> with axes: domain domain domain list
   *   - UseScalarPixel:             Image<double,5>           with axes: domain domain domain list RGB-Color
   *
   *   NRRD file: list domain domain domain (or: domain domain domain list)
   *   - UseAnyRangeAxisAsPixel:     VectorImage<double,3> with axes: domain domain domain
   *   - UseNonListRangeAxisAsPixel: Image<double,4>       with axes: domain domain domain list
   *   - UseScalarPixel:             Image<double,4>       with axes: domain domain domain list
   */
  itkSetMacro(AxesReorder, AxesReorderEnum);
  itkGetConstMacro(AxesReorder, AxesReorderEnum);

  /** Use the first a range axis (i.e., non-domain axis) of the NRRD file as pixel component. */
  void
  SetAxesReorderToUseAnyRangeAxisAsPixel()
  {
    this->SetAxesReorder(AxesReorderEnum::UseAnyRangeAxisAsPixel);
  }

  /** UseNonListRangeAxisAsPixel Use the first non-list range axis (i.e., non-domain axis) of the NRRD file as pixel
   * component. */
  void
  SetAxesReorderToUseNonListRangeAxisAsPixel()
  {
    this->SetAxesReorder(AxesReorderEnum::UseNonListRangeAxisAsPixel);
  }

  /** UseScalarPixel Pixel type is scalar. All range (i.e., non-domain) axes of the NRRD file are added as additional
   * domain axes. */
  void
  SetAxesReorderToUseScalarPixel()
  {
    this->SetAxesReorder(AxesReorderEnum::UseScalarPixel);
  }

protected:
  NrrdImageIO();
  ~NrrdImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  InternalSetCompressor(const std::string & _compressor) override;

  /** Utility functions for converting between enumerated data type
      representations */
  int
  ITKToNrrdComponentType(const IOComponentEnum) const;

  IOComponentEnum
  NrrdToITKComponentType(const int) const;

  const NrrdEncoding_t * m_NrrdCompressionEncoding{ nullptr };

  AxesReorderEnum m_AxesReorder{ AxesReorderEnum::UseAnyRangeAxisAsPixel };
};
} // end namespace itk

#endif // itkNrrdImageIO_h
