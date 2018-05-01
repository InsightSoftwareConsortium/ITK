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

#ifndef itkTileMontage_h
#define itkTileMontage_h

#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include <vector>

namespace itk
{

/** \class TileMontage
 *  \brief Determines registrations for an n-Dimensional mosaic of images.
 *
 *  Determines registrations which can be used to resample a mosaic into a single image.
 *
 * \author Dženan Zukić, dzenan.zukic@kitware.com
 *
 * \ingroup Montage
 */
template <typename TImageType>
class ITK_TEMPLATE_EXPORT TileMontage: public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TileMontage);

  /** Standard class type aliases. */
  using Self = TileMontage;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ImageType = TImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhaseCorrelationImageRegistrationMethod, ProcessObject);

  /** Dimensionality of input images. */
  itkStaticConstMacro(ImageDimension, unsigned int, ImageType::ImageDimension);

  /** Montage size and tile index types. */
  using SizeType = Size<ImageDimension>;
  using IndexType = Size<ImageDimension>;

  /** Image's pixel type. */
  using PixelType = typename ImageType::PixelType;

  /** Internal PhaseCorrelationImageRegistrationMethod's type alias. */
  using PCMType = PhaseCorrelationImageRegistrationMethod<ImageType, ImageType>;

  using PCMOperatorType = itk::PhaseCorrelationOperator< typename itk::NumericTraits< PixelType >::RealType, ImageDimension >;

  using PCMOptimizerType = itk::MaxPhaseCorrelationOptimizer< PCMType >;

  /**  Type for the transform. */
  using TransformType = typename PCMType::TransformType;
  using TransformPointer = typename TransformType::Pointer;
  using TransformConstPointer = typename TransformType::ConstPointer;

  /** Type for the output transform parameters (the shift). */
  //using ParametersType = typename TransformType::ParametersType;

  /** Type for the output: Using Decorator pattern for enabling
  *  the Transform to be passed in the data pipeline */
  using TransformOutputType = DataObjectDecorator< TransformType >;
  using TransformOutputPointer = typename TransformOutputType::Pointer;
  using TransformOutputConstPointer = typename TransformOutputType::ConstPointer;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Passes ReleaseDataFlag to internal filters. */
  void SetReleaseDataFlag(bool flag) override
  {
    Superclass::SetReleaseDataFlag(flag);
    m_PCM->SetReleaseDataFlag(flag);
  }

  /** Passes ReleaseDataBeforeUpdateFlag to internal filters. */
  void SetReleaseDataBeforeUpdateFlag(const bool flag) override
  {
    Superclass::SetReleaseDataBeforeUpdateFlag(flag);
    m_PCM->SetReleaseDataBeforeUpdateFlag(flag);
  }

  /** Set/Get the PhaseCorrelationImageRegistrationMethod. */
  itkSetObjectMacro(PCM, PCMType);
  itkGetModifiableObjectMacro(PCM, PCMType);

  /** Set/Get the PhaseCorrelationImageRegistrationMethod. */
  itkSetObjectMacro(PCMOptimizer, PCMOptimizerType);
  itkGetModifiableObjectMacro(PCMOptimizer, PCMOptimizerType);

  /** Get/Set size of the image mosaic. */
  itkGetConstMacro(MontageSize, SizeType);
  void SetMontageSize(SizeType montageSize);

  void SetInputTile(IndexType position, ImageType* image)
  {
    SizeValueType linInd = this->nDIndexToLinearIndex(position);
    this->SetNthInput(linInd, image);
    m_FFTCache[linInd] = nullptr;
  }

  TransformConstPointer GetOutputTransform(IndexType position)
  {
    return static_cast<TransformOutputType *>(this->GetOutput(this->nDIndexToLinearIndex(position)))->Get();
  }

protected:
  TileMontage();
  virtual ~TileMontage() {};
  void PrintSelf(std::ostream& os, Indent indent) const override;

  DataObjectPointerArraySizeType nDIndexToLinearIndex(IndexType nDIndex) const;
  IndexType LinearIndexTonDIndex(DataObjectPointerArraySizeType linearIndex) const;

  /** Make a DataObject of the correct type to be used as the specified output. */
  DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) override
  {
    return TransformOutputType::New();
  }

  /** Register a pair of images with given indices. Handles FFTcaching. */
  TransformPointer RegisterPair(IndexType fixed, IndexType moving);

  /** Montage this dimension, and all lower dimensions. */
  void MontageDimension(int d, TransformPointer tInitial, IndexType initialTile);

  /** Accesses output, sets a transform to it, and updates progress. */
  void WriteOutTransform(IndexType index, TransformPointer transform);

  /** Method invoked by the pipeline in order to trigger the computation of the registration. */
  void GenerateData() override;

  /** Method invoked by the pipeline to determine the output information. */
  void GenerateOutputInformation() override;

  /** Image's FFT type. */
  using FFTType = typename PCMType::ComplexImageType;
  using FFTPointer = typename FFTType::Pointer;
  using FFTConstPointer = typename FFTType::ConstPointer;

private:
    SizeType      m_MontageSize;
    SizeValueType m_LinearMontageSize;
    SizeValueType m_FinishedTiles;

    std::vector<FFTConstPointer> m_FFTCache;
    typename PCMType::Pointer    m_PCM;

    typename PCMOperatorType::Pointer   m_PCMOperator;
    typename PCMOptimizerType::Pointer  m_PCMOptimizer;
}; // class TileMontage

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTileMontage.hxx"
#endif

#endif //itkTileMontage_h
