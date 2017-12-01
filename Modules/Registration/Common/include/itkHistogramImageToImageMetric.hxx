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
#ifndef itkHistogramImageToImageMetric_hxx
#define itkHistogramImageToImageMetric_hxx

#include "itkArray.h"
#include "itkHistogramImageToImageMetric.h"
#include "itkNumericTraits.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
template< typename TFixedImage, typename TMovingImage >
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::HistogramImageToImageMetric()
{
  itkDebugMacro("Constructor");

  m_HistogramSize.Fill(256);
  m_UsePaddingValue = false;
  m_DerivativeStepLength = 0.1;
  m_DerivativeStepLengthScales.Fill(1);
  m_UpperBoundIncreaseFactor = 0.001;
  m_PaddingValue = NumericTraits< FixedImagePixelType >::ZeroValue();
  m_Histogram = HistogramType::New();
  m_Histogram->SetMeasurementVectorSize(2);
  m_LowerBoundSetByUser = false;
  m_UpperBoundSetByUser = false;
}

template< typename TFixedImage, typename TMovingImage >
void HistogramImageToImageMetric< TFixedImage, TMovingImage >
::SetUpperBound(const MeasurementVectorType & bounds)
{
  m_UpperBound = bounds;
  m_UpperBoundSetByUser = true;
  this->Modified();
}

template< typename TFixedImage, typename TMovingImage >
const typename HistogramImageToImageMetric< TFixedImage, TMovingImage >::MeasurementVectorType &
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::GetUpperBound() const
{
  return m_UpperBound;
}

template< typename TFixedImage, typename TMovingImage >
void HistogramImageToImageMetric< TFixedImage, TMovingImage >
::SetLowerBound(const MeasurementVectorType & bounds)
{
  m_LowerBound = bounds;
  m_LowerBoundSetByUser = true;
  this->Modified();
}

template< typename TFixedImage, typename TMovingImage >
const typename HistogramImageToImageMetric< TFixedImage, TMovingImage >::MeasurementVectorType &
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::GetLowerBound() const
{
  return m_LowerBound;
}

template< typename TFixedImage, typename TMovingImage >
void HistogramImageToImageMetric< TFixedImage, TMovingImage >
::Initialize()
{
  Superclass::Initialize();

  if ( !this->m_FixedImage )
    {
    itkExceptionMacro(<< "Fixed image has not been set.");
    }
  else if ( !this->m_MovingImage )
    {
    itkExceptionMacro(<< "Moving image has not been set.");
    }

  if ( !m_LowerBoundSetByUser || !m_UpperBoundSetByUser )
    {
    // Calculate min and max image values in fixed image.
    FixedImageConstPointerType                 pFixedImage = this->m_FixedImage;
    ImageRegionConstIterator< FixedImageType > fiIt( pFixedImage,
                                                     pFixedImage->
                                                     GetBufferedRegion() );
    fiIt.GoToBegin();
    FixedImagePixelType minFixed = fiIt.Value();
    FixedImagePixelType maxFixed = fiIt.Value();
    ++fiIt;
    while ( !fiIt.IsAtEnd() )
      {
      FixedImagePixelType value = fiIt.Value();

      if ( value < minFixed )
        {
        minFixed = value;
        }
      else if ( value > maxFixed )
        {
        maxFixed = value;
        }

      ++fiIt;
      }

    // Calculate min and max image values in moving image.
    MovingImageConstPointerType                 pMovingImage = this->m_MovingImage;
    ImageRegionConstIterator< MovingImageType > miIt( pMovingImage,
                                                      pMovingImage->
                                                      GetBufferedRegion() );
    miIt.GoToBegin();
    MovingImagePixelType minMoving = miIt.Value();
    MovingImagePixelType maxMoving = miIt.Value();
    ++miIt;
    while ( !miIt.IsAtEnd() )
      {
      MovingImagePixelType value = miIt.Value();

      if ( value < minMoving )
        {
        minMoving = value;
        }
      else if ( value > maxMoving )
        {
        maxMoving = value;
        }
      ++miIt;
      }

    // Initialize the upper and lower bounds of the histogram.
    if ( !m_LowerBoundSetByUser )
      {
      m_LowerBound.SetSize(2);
      m_LowerBound[0] = minFixed;
      m_LowerBound[1] = minMoving;
      }

    if ( !m_UpperBoundSetByUser )
      {
      m_UpperBound.SetSize(2);
      m_UpperBound[0] =
        maxFixed + ( maxFixed - minFixed ) * m_UpperBoundIncreaseFactor;
      m_UpperBound[1] =
        maxMoving + ( maxMoving - minMoving ) * m_UpperBoundIncreaseFactor;
      }
    }
}

template< typename TFixedImage, typename TMovingImage >
void
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::SetTransform(TransformType *transform)
{
  if ( m_DerivativeStepLengthScales.GetSize()
       != transform->GetNumberOfParameters() )
    {
    m_DerivativeStepLengthScales.SetSize( transform->GetNumberOfParameters() );
    m_DerivativeStepLengthScales.Fill(1.0);
    }
  Superclass::SetTransform(transform);
}

template< typename TFixedImage, typename TMovingImage >
typename HistogramImageToImageMetric< TFixedImage, TMovingImage >::MeasureType
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::GetValue(const TransformParametersType & parameters) const
{
  itkDebugMacro("GetValue( " << parameters << " ) ");

  this->ComputeHistogram(parameters, *m_Histogram);

  return this->EvaluateMeasure(*m_Histogram);
}

template< typename TFixedImage, typename TMovingImage >
void
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::GetDerivative(const TransformParametersType & parameters,
                DerivativeType & derivative) const
{
  itkDebugMacro("GetDerivative( " << parameters << " ) ");

  const unsigned int ParametersDimension = this->GetNumberOfParameters();

  // Make sure the scales have been set
  if ( m_DerivativeStepLengthScales.size() != ParametersDimension )
    {
    itkExceptionMacro(<< "The size of DerivativesStepLengthScales is "
                      << m_DerivativeStepLengthScales.size()
                      << ", but the Number of Parameters is "
                      << ParametersDimension
                      << ".");
    }

  // Calculate gradient.
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(NumericTraits< typename
                                 DerivativeType::ValueType >::ZeroValue());

  typename HistogramType::Pointer pHistogram = HistogramType::New();
  pHistogram->SetMeasurementVectorSize(2);
  this->ComputeHistogram(parameters, *pHistogram);

  for ( unsigned int i = 0; i < ParametersDimension; i++ )
    {
    typename HistogramType::Pointer pHistogram2 = HistogramType::New();
    pHistogram2->SetMeasurementVectorSize(2);
    this->CopyHistogram(*pHistogram2, *pHistogram);

    TransformParametersType newParameters = parameters;
    newParameters[i] -=
      m_DerivativeStepLength / m_DerivativeStepLengthScales[i];
    this->ComputeHistogram(newParameters, *pHistogram2);

    MeasureType e0 = EvaluateMeasure(*pHistogram2);

    pHistogram2 = HistogramType::New();
    pHistogram2->SetMeasurementVectorSize(2);
    this->CopyHistogram(*pHistogram2, *pHistogram);

    newParameters = parameters;
    newParameters[i] +=
      m_DerivativeStepLength / m_DerivativeStepLengthScales[i];
    this->ComputeHistogram(newParameters, *pHistogram2);

    MeasureType e1 = EvaluateMeasure(*pHistogram2);

    derivative[i] =
      ( e1 - e0 ) / ( 2 * m_DerivativeStepLength / m_DerivativeStepLengthScales[i] );
    }
}

template< typename TFixedImage, typename TMovingImage >
void
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivative(const TransformParametersType & parameters,
                        MeasureType & value,
                        DerivativeType & derivative) const
{
  value = GetValue(parameters);
  this->GetDerivative(parameters, derivative);
}

template< typename TFixedImage, typename TMovingImage >
void
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::ComputeHistogram(TransformParametersType const & parameters,
                   HistogramType & histogram) const
{
  FixedImageConstPointerType fixedImage = this->m_FixedImage;

  if ( !fixedImage )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  typedef itk::ImageRegionConstIteratorWithIndex< FixedImageType >
  FixedIteratorType;

  typename FixedImageType::IndexType index;
  typename FixedImageType::RegionType fixedRegion;
  typename HistogramType::IndexType hIndex;

  fixedRegion = this->GetFixedImageRegion();
  FixedIteratorType ti(fixedImage, fixedRegion);

  this->m_NumberOfPixelsCounted = 0;
  this->SetTransformParameters(parameters);

  histogram.Initialize(m_HistogramSize, m_LowerBound, m_UpperBound);

  ti.GoToBegin();
  while ( !ti.IsAtEnd() )
    {
    index = ti.GetIndex();

    if ( fixedRegion.IsInside(index)
         && ( !m_UsePaddingValue
              || ( m_UsePaddingValue && ti.Get() > m_PaddingValue ) ) )
      {
      InputPointType inputPoint;
      fixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

      if ( this->m_FixedImageMask
           && !this->m_FixedImageMask->IsInside(inputPoint) )
        {
        ++ti;
        continue;
        }

      OutputPointType transformedPoint =
        this->m_Transform->TransformPoint(inputPoint);

      if ( this->m_MovingImageMask
           && !this->m_MovingImageMask->IsInside(transformedPoint) )
        {
        ++ti;
        continue;
        }

      if ( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
        {
        const RealType movingValue =
          this->m_Interpolator->Evaluate(transformedPoint);
        const RealType fixedValue = ti.Get();
        this->m_NumberOfPixelsCounted++;

        typename HistogramType::MeasurementVectorType sample;
        sample.SetSize(2);
        sample[0] = fixedValue;
        sample[1] = movingValue;

        histogram.GetIndex( sample, hIndex );
        histogram.IncreaseFrequencyOfIndex( hIndex, 1 );
        }
      }

    ++ti;
    }

  itkDebugMacro("NumberOfPixelsCounted = " << this->m_NumberOfPixelsCounted);
  if ( this->m_NumberOfPixelsCounted == 0 )
    {
    itkExceptionMacro(
      << "All the points mapped to outside of the moving image");
    }
}

template< typename TFixedImage, typename TMovingImage >
void
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::CopyHistogram(HistogramType & target, HistogramType & source) const
{
  // Initialize the target.
  typename HistogramType::MeasurementVectorType min;
  typename HistogramType::MeasurementVectorType max;

  min.SetSize(2);
  max.SetSize(2);

  typename HistogramType::SizeType size = source.GetSize();

  for ( unsigned int i = 0; i < min.Size(); i++ )
    {
    min[i] = source.GetBinMin(i, 0);
    }

  for ( unsigned int i = 0; i < max.Size(); i++ )
    {
    max[i] = source.GetBinMax(i, size[i] - 1);
    }

  target.Initialize(size, min, max);

  // Copy the values.
  typename HistogramType::Iterator sourceIt = source.Begin();
  typename HistogramType::Iterator sourceEnd = source.End();
  typename HistogramType::Iterator targetIt = target.Begin();
  typename HistogramType::Iterator targetEnd = target.End();

  while ( sourceIt != sourceEnd && targetIt != targetEnd )
    {
    typename HistogramType::AbsoluteFrequencyType freq = sourceIt.GetFrequency();

    if ( freq > 0 )
      {
      targetIt.SetFrequency(freq);
      }
    ++sourceIt;
    ++targetIt;
    }
}

template< typename TFixedImage, typename TMovingImage >
void
HistogramImageToImageMetric< TFixedImage, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Padding value: "
     << static_cast< typename NumericTraits< FixedImagePixelType >::PrintType >(
    m_PaddingValue )
     << std::endl;
  os << indent << "Use padding value?: " << m_UsePaddingValue << std::endl;
  os << indent << "Derivative step length: " << m_DerivativeStepLength
     << std::endl;
  os << indent << "Derivative step length scales: ";
  os << m_DerivativeStepLengthScales << std::endl;
  os << indent << "Histogram size: ";
  os << m_HistogramSize << std::endl;
  os << indent << "Histogram upper bound increase factor: ";
  os << m_UpperBoundIncreaseFactor << std::endl;
  os << indent << "Histogram computed by GetValue(): ";
  os << m_Histogram.GetPointer() << std::endl;
}
} // end namespace itk

#endif // itkHistogramImageToImageMetric_hxx
