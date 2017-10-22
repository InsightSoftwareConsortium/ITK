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

class ITKIOPhilipsREC_EXPORT PhilipsRECImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef PhilipsRECImageIO    Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhilipsRECImageIO, Superclass);

  /** Special types used for Philips PAR meta data. */
  typedef VectorContainer< unsigned int, double > EchoTimesContainerType;
  typedef VectorContainer< unsigned int, double > TriggerTimesContainerType;
  typedef VectorContainer< unsigned int, double > RepetitionTimesContainerType;
  typedef vnl_vector_fixed< int, 2 >              ScanResolutionType;
  typedef vnl_vector_fixed< float, 3 >            FOVType;
  typedef vnl_vector_fixed< double, 3 >           AngulationMidSliceType;
  typedef vnl_vector_fixed< double, 3 >           OffCentreMidSliceType;
  typedef vnl_vector_fixed< float, 3 >            PhaseEncodingVelocityType;
  /** Image types:
   * 0 = Magnitude,
   * 1 = Real,
   * 2 = Imaginary,
   * 3 = Phase,
   * 4 = Special/Processed. */
  typedef vnl_vector_fixed< int, 8 >    ImageTypesType;
  typedef vnl_vector_fixed< int, 8 >    ScanningSequencesType;
  typedef Superclass::IndexValueType    IndexValueType;
  typedef std::vector< IndexValueType > SliceIndexType;
  typedef vnl_vector_fixed< double, 3 > ImageTypeRescaleValuesType;

  typedef VectorContainer< unsigned int, ImageTypeRescaleValuesType > ImageTypeRescaleValuesContainerType;
  typedef ImageTypeRescaleValuesContainerType::Pointer
  ImageTypeRescaleValuesContainerTypePtr;
  typedef VectorContainer< unsigned int,
                           ImageTypeRescaleValuesContainerTypePtr > ScanningSequenceImageTypeRescaleValuesContainerType;
  typedef double                                                 GradientBvalueType;
  typedef VectorContainer< unsigned int, GradientBvalueType >    GradientBvalueContainerType;
  typedef vnl_vector_fixed< double, 3 >                          GradientDirectionType;
  typedef VectorContainer< unsigned int, GradientDirectionType > GradientDirectionContainerType;
  typedef VectorContainer< unsigned int, int >                   LabelTypesASLContainerType;

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
       * \author Don C. Bigler
       * \param FileNameToRead The name of the file to test for reading.
       * \return Returns true if this ImageIO can read the file specified.
       */
  virtual bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
       * FileNameToWrite The name of the file to test for writing.
       * \author Don C. Bigler
       * \post This function will always return false (Not implemented).
       * \return Returns true if this ImageIO can write the file specified.
       */
  virtual bool CanWriteFile( const char *itkNotUsed(FileNameToWrite) ) ITK_OVERRIDE
  {
    return false;
  }

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE
  {
    return;
  }

  /** Writes the data to disk from the memory buffer provided. Make sure
       * that the IORegions has been set properly. */
  virtual void Write( const void *itkNotUsed(buffer) ) ITK_OVERRIDE
  {
    return;
  }

protected:
  PhilipsRECImageIO();
  ~PhilipsRECImageIO() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(PhilipsRECImageIO);

  void SwapBytesIfNecessary(void *buffer, SizeValueType numberOfPixels);

  IndexValueType GetSliceIndex(IndexValueType index) const;

  SliceIndexType *       m_SliceIndex;
  ImageIOBase::ByteOrder m_MachineByteOrder;
};
} // end namespace itk

#endif // itkPhilipsRECImageIO_h
