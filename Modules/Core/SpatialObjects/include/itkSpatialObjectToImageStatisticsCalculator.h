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
#ifndef itkSpatialObjectToImageStatisticsCalculator_h
#define itkSpatialObjectToImageStatisticsCalculator_h

#include "itkObject.h"
#include "itkFloodFilledSpatialFunctionConditionalConstIterator.h"
#include "itkMatrix.h"
#include "itkNumericTraits.h"
#include "itkListSample.h"
#include "itkVector.h"

namespace itk
{
/** \class SpatialObjectToImageStatisticsCalculator
 * This calculator computes the mean and the covariance matrice of a certain
 *  region of an image specified by a spatial object.
 * \ingroup Operators
 * \ingroup ITKSpatialObjects
 */
template< typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension = 1 >
class ITK_TEMPLATE_EXPORT SpatialObjectToImageStatisticsCalculator:public Object
{
public:
  /** Standard class typedefs. */
  typedef SpatialObjectToImageStatisticsCalculator Self;
  typedef Object                                   Superclass;
  typedef SmartPointer< Self >                     Pointer;
  typedef SmartPointer< const Self >               ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectToImageStatisticsCalculator, Object);

  /** Type definitions for the input image. */
  typedef TInputImage                        ImageType;
  typedef typename TInputImage::Pointer      ImagePointer;
  typedef typename TInputImage::ConstPointer ImageConstPointer;
  typedef typename TInputImage::PixelType    PixelType;
  typedef typename TInputImage::IndexType    IndexType;

  typedef  typename NumericTraits< PixelType >::AccumulateType AccumulateType;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      ImageType::ImageDimension);

  itkStaticConstMacro(SampleDimension, unsigned int,
                      TSampleDimension);

  itkStaticConstMacro(ObjectDimension, unsigned int,
                      TInputSpatialObject::ObjectDimension);

  /** Type definitions for the input spatial object. */
  typedef TInputSpatialObject                      SpatialObjectType;
  typedef typename SpatialObjectType::Pointer      SpatialObjectPointer;
  typedef typename SpatialObjectType::ConstPointer SpatialObjectConstPointer;

  /** Type definition of the flood fill iterator */
  typedef itk::FloodFilledSpatialFunctionConditionalConstIterator< ImageType,
                                                                   SpatialObjectType > IteratorType;

  /** Vector and Matrix Type */
  typedef Vector< double, TSampleDimension >                   VectorType;
  typedef Matrix< double, TSampleDimension, TSampleDimension > MatrixType;

  /** Type definitions for the samples */
  typedef itk::Statistics::ListSample< VectorType > SampleType;

  /** Set/Get the direction of the sample */
  itkSetMacro(SampleDirection, unsigned int);
  itkGetConstMacro(SampleDirection, unsigned int);

  /** Set the input image. */
  itkSetConstObjectMacro(Image, ImageType);

  /** Set the input spatial object. */
  itkSetObjectMacro(SpatialObject, SpatialObjectType);

  /** Get the mean */
  const VectorType & GetMean() const { return m_Mean; }

  /** Get the covariance matrix */
  const MatrixType & GetCovarianceMatrix() const { return m_CovarianceMatrix; }

  /** Get the sum of pixels */
  AccumulateType GetSum() const { return m_Sum; }

  /** Get the number of pixels inside the object */
  itkGetConstMacro(NumberOfPixels, SizeValueType);

  /** Compute of the input image. */
  void Update();

protected:
  SpatialObjectToImageStatisticsCalculator();
  virtual ~SpatialObjectToImageStatisticsCalculator() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  bool ComputeStatistics();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SpatialObjectToImageStatisticsCalculator);

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
#include "itkSpatialObjectToImageStatisticsCalculator.hxx"
#endif

#endif /* itkSpatialObjectToImageStatisticsCalculator_h */
