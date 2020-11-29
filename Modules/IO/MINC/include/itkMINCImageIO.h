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
/**
 *         The specification for this file format is taken from the
 *         web site http://www.bic.mni.mcgill.ca/ServicesSoftware/MINC
 * \author Vladimir S. FONOV
 *         Brain Imaging Center, Montreal Neurological Institute, McGill University, Montreal Canada 2012
 * \author Leila Baghdadi
 *         Mouse Imaging Centre, Toronto, Canada 2005.
 */

#ifndef itkMINCImageIO_h
#define itkMINCImageIO_h

#include "itkImageIOBase.h"

#include "itkMatrix.h"

#include "ITKIOMINCExport.h"

namespace itk
{

// Structure for "Pointer to Implementation" or "Private
// Implementation" to hide MINC data from the ITK interface.
struct ITKIOMINC_HIDDEN MINCImageIOPImpl;

/**
 *\class MINCImageIO
 *
 * \author Leila Baghdadi
 * \brief Class that defines how to read MINC file format.
 *
 * \ingroup ITKIOMINC
 *
 * Note, like ITK, MINC is N dimensional and dimensions
 * can be submitted in any arbitrary order. Here we make sure the
 * dimensions are ordered as xspace, yspace, zspace, time and
 * vector_dimension and so on or xfrequencey, yfrequency, zfrequency,
 * tfrequency and vector_dimension and so on NOTE** This class only
 * reads the regularly sampled dimensions as I am not sure how to deal
 * with "irregularly sampled" dimensions yet!
 *
 * Compression is supported with only the default compressor. The
 * compression level option is supported in the range 0-9.
 *
 * This code was contributed in the Insight Journal paper:
 * "MINC2.0 IO Support for ITK"
 * by Baghdadi L.
 * https://www.insight-journal.org/browse/publication/88
 *
 * \ingroup IOFilters
 *
 */
class ITKIOMINC_EXPORT MINCImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MINCImageIO);

  /** Standard class type aliases. */
  using Self = MINCImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;
  using MatrixType = Matrix<float, 3, 3>;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MINCImageIO, Superclass);

  /** Right now MINC supports up to 3D with multiple components */
  bool
  SupportsDimension(unsigned long dim) override
  {
    return dim < 4;
  }

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

protected:
  MINCImageIO();
  ~MINCImageIO() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  WriteSlice(std::string & fileName, const void * buffer);

  // will assign m_NDims and allocate all internal buffers to hold the
  // information
  void
  AllocateDimensions(int nDims);

  // cleanup internal buffers
  void
  CleanupDimensions();

  // close existing volume, cleanup internal structures
  void
  CloseVolume();

private:
  MINCImageIOPImpl * m_MINCPImpl;

  MatrixType m_DirectionCosines;

  // complex type images, composed of complex numbers
  // int m_Complex;
};
} // end namespace itk

#endif // itkMINCImageIO_h
