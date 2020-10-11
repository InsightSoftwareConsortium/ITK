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
#ifndef itkSpatialObjectToImageStatisticsCalculator_h
#define itkSpatialObjectToImageStatisticsCalculator_h

#include "itkObject.h"
#include "itkMatrix.h"
#include "itkNumericTraits.h"
#include "itkListSample.h"
#include "itkVector.h"

namespace itk
{
/**
 *\class SpatialObjectToImageStatisticsCalculator
 * This calculator computes the mean and the covariance matrices of a certain
 *  region of an image specified by a spatial object.
 * \ingroup Operators
 * \ingroup ITKSpatialObjects
 */
template <typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension = 1>
class ITK_TEMPLATE_EXPORT SpatialObjectToImageStatisticsCalculator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SpatialObjectToImageStatisticsCalculator);

  /** Standard class type aliases. */
  using Self = SpatialObjectToImageStatisticsCalculator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectToImageStatisticsCalculator, Object);

  /** Type definitions for the input image. */
  using ImageType = TInputImage;
  using ImagePointer = typename TInputImage::Pointer;
  using ImageConstPointer = typename TInputImage::ConstPointer;
  using PixelType = typename TInputImage::PixelType;
  using IndexType = typename TInputImage::IndexType;
  using PointType = typename TInputImage::PointType;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename RegionType::SizeType;

  using AccumulateType = typename NumericTraits<PixelType>::AccumulateType;

  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  static constexpr unsigned int SampleDimension = TSampleDimension;

  static constexpr unsigned int ObjectDimension = TInputSpatialObject::ObjectDimension;

  /** Type definitions for the input spatial object. */
  using SpatialObjectType = TInputSpatialObject;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;
  using SpatialObjectConstPointer = typename SpatialObjectType::ConstPointer;

  /** Vector and Matrix Type */
  using VectorType = Vector<double, TSampleDimension>;
  using MatrixType = Matrix<double, TSampleDimension, TSampleDimension>;

  /** Type definitions for the samples */
  using SampleType = itk::Statistics::ListSample<VectorType>;

  /** Set/Get the direction of the sample */
  itkSetMacro(SampleDirection, unsigned int);
  itkGetConstMacro(SampleDirection, unsigned int);

  /** Set the input image. */
  itkSetConstObjectMacro(Image, ImageType);

  /** Set the input spatial object. */
  itkSetObjectMacro(SpatialObject, SpatialObjectType);

  /** Get the mean */
  const VectorType &
  GetMean() const
  {
    return m_Mean;
  }

  /** Get the covariance matrix */
  const MatrixType &
  GetCovarianceMatrix() const
  {
    return m_CovarianceMatrix;
  }

  /** Get the sum of pixels */
  AccumulateType
  GetSum() const
  {
    return m_Sum;
  }

  /** Get the number of pixels inside the object */
  itkGetConstMacro(NumberOfPixels, SizeValueType);

  /** Compute of the input image. */
  void
  Update();

protected:
  SpatialObjectToImageStatisticsCalculator();
  ~SpatialObjectToImageStatisticsCalculator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  bool
  ComputeStatistics();

private:
  ImageConstPointer    m_Image;
  SpatialObjectPointer m_SpatialObject;
  VectorType           m_Mean;
  AccumulateType       m_Sum;
  SizeValueType        m_NumberOfPixels;
  MatrixType           m_CovarianceMatrix;
  unsigned int         m_SampleDirection;
  ModifiedTimeType     m_InternalImageTime;
  ModifiedTimeType     m_InternalSpatialObjectTime;
  TimeStamp            m_ModifiedTime;

  typename SampleType::Pointer m_Sample;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpatialObjectToImageStatisticsCalculator.hxx"
#endif

#endif /* itkSpatialObjectToImageStatisticsCalculator_h */
