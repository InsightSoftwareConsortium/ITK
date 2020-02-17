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
 * \file   itkPhilipsRECImageIO.h
 *         The code for this file reader was written based on
 *         examination of Philips REC/PAR image files acquired at the
 *         Center for NMR Research at the Penn State Milton S. Hershey
 *         Medical Center.
 *
 *
 * \author Don C. Bigler
 *         The Pennsylvania State University 2005
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/1381
 *
 */

#ifndef itkPhilipsRECImageIO_h
#define itkPhilipsRECImageIO_h
#include "ITKIOPhilipsRECExport.h"


#include "itkImageIOBase.h"
#include "itkVectorContainer.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{
/** \class PhilipsRECImageIO
 * \author Don C. Bigler
 * \brief Reads Philips REC/PAR image files.
 *
 *  This class supports reading only and not writing.
 * \ingroup IOFilters
 * \ingroup ITKIOPhilipsREC
 */

class ITKIOPhilipsREC_EXPORT PhilipsRECImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhilipsRECImageIO);

  /** Standard class type aliases. */
  using Self = PhilipsRECImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhilipsRECImageIO, Superclass);

  /** Special types used for Philips PAR meta data. */
  using EchoTimesContainerType = VectorContainer<unsigned int, double>;
  using TriggerTimesContainerType = VectorContainer<unsigned int, double>;
  using RepetitionTimesContainerType = VectorContainer<unsigned int, double>;
  using ScanResolutionType = vnl_vector_fixed<int, 2>;
  using FOVType = vnl_vector_fixed<float, 3>;
  using AngulationMidSliceType = vnl_vector_fixed<double, 3>;
  using OffCentreMidSliceType = vnl_vector_fixed<double, 3>;
  using PhaseEncodingVelocityType = vnl_vector_fixed<float, 3>;
  /** Image types:
   * 0 = Magnitude,
   * 1 = Real,
   * 2 = Imaginary,
   * 3 = Phase,
   * 4 = Special/Processed. */
  using ImageTypesType = vnl_vector_fixed<int, 8>;
  using ScanningSequencesType = vnl_vector_fixed<int, 8>;
  using IndexValueType = Superclass::IndexValueType;
  using SliceIndexType = std::vector<IndexValueType>;
  using ImageTypeRescaleValuesType = vnl_vector_fixed<double, 3>;

  using ImageTypeRescaleValuesContainerType = VectorContainer<unsigned int, ImageTypeRescaleValuesType>;
  using ImageTypeRescaleValuesContainerTypePtr = ImageTypeRescaleValuesContainerType::Pointer;
  using ScanningSequenceImageTypeRescaleValuesContainerType =
    VectorContainer<unsigned int, ImageTypeRescaleValuesContainerTypePtr>;
  using GradientBvalueType = double;
  using GradientBvalueContainerType = VectorContainer<unsigned int, GradientBvalueType>;
  using GradientDirectionType = vnl_vector_fixed<double, 3>;
  using GradientDirectionContainerType = VectorContainer<unsigned int, GradientDirectionType>;
  using LabelTypesASLContainerType = VectorContainer<unsigned int, int>;

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
   * \author Don C. Bigler
   * \param FileNameToRead The name of the file to test for reading.
   * \return Returns true if this ImageIO can read the file specified.
   */
  bool
  CanReadFile(const char * FileNameToRead) override;

  /** Set the spacing and dimension information for the set filename. */
  void
  ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void
  Read(void * buffer) override;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
   * FileNameToWrite The name of the file to test for writing.
   * \author Don C. Bigler
   * \post This function will always return false (Not implemented).
   * \return Returns true if this ImageIO can write the file specified.
   */
  bool
  CanWriteFile(const char * itkNotUsed(FileNameToWrite)) override
  {
    return false;
  }

  /** Set the spacing and dimension information for the set filename. */
  void
  WriteImageInformation() override
  {
    return;
  }

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void
  Write(const void * itkNotUsed(buffer)) override
  {
    return;
  }

protected:
  PhilipsRECImageIO();
  ~PhilipsRECImageIO() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  void
  SwapBytesIfNecessary(void * buffer, SizeValueType numberOfPixels);

  IndexValueType
  GetSliceIndex(IndexValueType index) const;

  SliceIndexType * m_SliceIndex;
  IOByteOrderEnum  m_MachineByteOrder;
};
} // end namespace itk

#endif // itkPhilipsRECImageIO_h
