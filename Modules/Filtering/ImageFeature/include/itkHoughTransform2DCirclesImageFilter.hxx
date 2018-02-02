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
#ifndef itkHoughTransform2DCirclesImageFilter_hxx
#define itkHoughTransform2DCirclesImageFilter_hxx

#include "itkHoughTransform2DCirclesImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkGaussianDerivativeImageFunction.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkMath.h"
#include <cassert>

namespace itk
{
template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::Circle::Circle()
  :
  m_Center{},
  m_Radius{}
{
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::Circle::Circle(const CenterType& center, const RadiusType radius)
  :
  // Note: Use parentheses instead of curly braces to initialize m_Center, to avoid
  // AppleClang 6.0.0.6000056 compilation error, "no viable conversion from
  // 'const CenterType' (aka 'const Index<2>') to 'IndexValueType' (aka 'long').
  m_Center( center ),
  m_Radius{ radius }
{
  assert(radius >= 0.0);
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
auto
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::Circle::GetCenter() const
-> CenterType
{
  return m_Center;
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::Circle::SetCenter(const CenterType& center)
{
  m_Center = center;
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
auto
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::Circle::GetRadius() const
-> RadiusType
{
  return m_Radius;
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::Circle::SetRadius(const RadiusType radius)
{
  assert(radius >= 0.0);
  m_Radius = radius;
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::Circle::ToEllipseSpatialObject(EllipseSpatialObject<2>& ellipseSpatialObject) const
{
  auto* const transform = ellipseSpatialObject.GetObjectToParentTransform();

  if (transform == nullptr)
  {
    assert(!"ellipseSpatialObject.GetObjectToParentTransform() should not return nullptr!");
  }
  else
  {
    EllipseSpatialObject<2>::TransformType::OffsetType offset;
    offset[0] = m_Center[0];
    offset[1] = m_Center[1];

    transform->SetOffset(offset);
  }
  ellipseSpatialObject.SetRadius(m_Radius);
}


template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::HoughTransform2DCirclesImageFilter() :
  m_SweepAngle( 0.0 ),
  m_MinimumRadius( 0.0 ),
  m_MaximumRadius( 10.0 ),
  m_Threshold( 0.0 ),
  m_SigmaGradient( 1.0 ),
  m_NumberOfCircles( 1 ),
  m_DiscRadiusRatio( 1 ),
  m_Variance( 10 ),
  m_OldModifiedTime( 0 )
{
  this->SetNumberOfRequiredInputs( 1 );
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::SetRadius(double radius)
{
  this->SetMinimumRadius(radius);
  this->SetMaximumRadius(radius);
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image =
      const_cast< InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::GenerateData()
{
  // Get the input and output pointers
  const InputImageConstPointer inputImage = this->GetInput(0);
  const OutputImagePointer     outputImage = this->GetOutput(0);

  // Allocate the output
  this->AllocateOutputs();
  outputImage->FillBuffer(0);

  using DoGFunctionType = GaussianDerivativeImageFunction< InputImageType >;
  typename DoGFunctionType::Pointer DoGFunction = DoGFunctionType::New();
  DoGFunction->SetInputImage(inputImage);
  DoGFunction->SetSigma(m_SigmaGradient);

  m_RadiusImage = RadiusImageType::New();

  m_RadiusImage->SetRegions( outputImage->GetLargestPossibleRegion() );
  m_RadiusImage->SetOrigin( inputImage->GetOrigin() );
  m_RadiusImage->SetSpacing( inputImage->GetSpacing() );
  m_RadiusImage->SetDirection( inputImage->GetDirection() );
  m_RadiusImage->Allocate( true ); // initialize buffer to zero

  ImageRegionConstIteratorWithIndex< InputImageType > image_it( inputImage,
    inputImage->GetRequestedRegion() );
  image_it.GoToBegin();

  const ImageRegion< 2 > & region = outputImage->GetRequestedRegion();

  while ( !image_it.IsAtEnd() )
    {
    if ( image_it.Get() > m_Threshold )
      {
      const Index< 2 > inputIndex = image_it.GetIndex();
      const typename DoGFunctionType::VectorType grad =
        DoGFunction->EvaluateAtIndex( inputIndex );

      double Vx = grad[0];
      double Vy = grad[1];

      // if the gradient is not flat
      if ( ( std::fabs(Vx) > 1 ) || ( std::fabs(Vy) > 1 ) )
        {
        const double norm = std::sqrt(Vx * Vx + Vy * Vy);
        Vx /= norm;
        Vy /= norm;

        for ( double angle = -m_SweepAngle; angle <= m_SweepAngle; angle += 0.05 )
          {
          double i = m_MinimumRadius;
          double distance;

          do
            {
            const Index< 2 > outputIndex =
              {{
              Math::Round<IndexValueType>( inputIndex[0] - i * ( Vx * std::cos(angle) + Vy * std::sin(angle) ) ),
              Math::Round<IndexValueType>( inputIndex[1] - i * ( Vx * std::sin(angle) + Vy * std::cos(angle) ) )
              }};

            if ( region.IsInside(outputIndex) )
              {
              distance = std::sqrt(static_cast<double>((outputIndex[1] - inputIndex[1]) * (outputIndex[1] - inputIndex[1])
                                 + (outputIndex[0] - inputIndex[0]) * (outputIndex[0] - inputIndex[0])));

              ++outputImage->GetPixel(outputIndex);
              m_RadiusImage->GetPixel(outputIndex) += distance;
              }
            else
              {
              break;
              }
            ++i;
            }
          while ( distance < m_MaximumRadius );
          }
        }
      }
    ++image_it;
    }

  // Compute the average radius
  ImageRegionConstIterator< OutputImageType > output_it( outputImage,
    outputImage->GetLargestPossibleRegion() );
  ImageRegionIterator< RadiusImageType > radius_it( m_RadiusImage,
    m_RadiusImage->GetLargestPossibleRegion() );
  output_it.GoToBegin();
  radius_it.GoToBegin();
  while ( !output_it.IsAtEnd() )
    {
    if ( output_it.Get() > 1 )
      {
      radius_it.Value() /= output_it.Get();
      }
    ++output_it;
    ++radius_it;
    }
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
typename HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >::CirclesListType &
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::GetCircles()
{
  // Make sure that all the required inputs exist and have a non-null value
  this->VerifyPreconditions();

  if ( this->GetMTime() == m_OldModifiedTime )
    {
    // If the filter has not been updated
    return m_CirclesList;
    }

  if( m_RadiusImage.IsNull() )
    {
    itkExceptionMacro(<<"Update() must be called before GetCircles().");
    }

  m_CirclesList.clear();

  if ( m_NumberOfCircles > 0 )
    {
    // Blur the accumulator in order to find the maximum
    using InternalImageType = Image< float, 2 >;

    // The variable "outputImage" is only used as input to gaussianFilter.
    // It should not be modified, because GetOutput(0) should not be changed.
    const OutputImagePointer outputImage = OutputImageType::New();
    outputImage->Graft(this->GetOutput(0));

    using GaussianFilterType = DiscreteGaussianImageFilter< OutputImageType, InternalImageType >;
    const typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();

    gaussianFilter->SetInput(outputImage); // The output is the accumulator image
    gaussianFilter->SetVariance(m_Variance);
    gaussianFilter->Update();
    const InternalImageType::Pointer postProcessImage = gaussianFilter->GetOutput();

    using MinMaxCalculatorType = MinimumMaximumImageCalculator< InternalImageType >;
    typename MinMaxCalculatorType::Pointer minMaxCalculator = MinMaxCalculatorType::New();
    ImageRegionIterator< InternalImageType > it_input( postProcessImage,
      postProcessImage->GetLargestPossibleRegion() );

    Index< 2 > index;

    // Find maxima
    // Break out of "forever loop" as soon as the requested number of circles is found.
    for(;;)
      {
      minMaxCalculator->SetImage(postProcessImage);
      minMaxCalculator->ComputeMaximum();

      if ( minMaxCalculator->GetMaximum() <= 0 )
        {
        // When all pixel values in 'postProcessImage' are zero or less, no more circles
        // should be found. Note that a zero in 'postProcessImage' might correspond to a
        // zero in the accumulator image, or it might be that the pixel is within a
        // removed disc around a previously found circle center.
        break;
        }

      const InternalImageType::IndexType indexOfMaximum = minMaxCalculator->GetIndexOfMaximum();

      // Create a Circle
      const TRadiusPixelType radius = m_RadiusImage->GetPixel(indexOfMaximum);

      m_CirclesList.push_back(Circle{ indexOfMaximum, radius });

      if ( m_CirclesList.size() >= m_NumberOfCircles ) { break; }

      // Remove a black disc from the Hough space domain
      for ( double angle = 0; angle <= 2 * itk::Math::pi; angle += itk::Math::pi / 1000 )
        {
        for ( double length = 0; length < m_DiscRadiusRatio * radius; length += 1 )
          {
          index[0] = Math::Round<IndexValueType>( indexOfMaximum[0] + length * std::cos(angle) );
          index[1] = Math::Round<IndexValueType>( indexOfMaximum[1] + length * std::sin(angle) );
          if ( postProcessImage->GetLargestPossibleRegion().IsInside(index) )
            {
            postProcessImage->SetPixel(index, 0);
            }
          }
        }
      }
    }

  m_OldModifiedTime = this->GetMTime();
  return m_CirclesList;
}

template< typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType, TRadiusPixelType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "Minimum Radius:  " << m_MinimumRadius << std::endl;
  os << indent << "Maximum Radius: " << m_MaximumRadius << std::endl;
  os << indent << "Derivative Scale : " << m_SigmaGradient << std::endl;
  os << indent << "Number Of Circles: " << m_NumberOfCircles << std::endl;
  os << indent << "Disc Radius Ratio: " << m_DiscRadiusRatio << std::endl;
  os << indent << "Accumulator blur variance: " << m_Variance << std::endl;
  os << indent << "Sweep angle : " << m_SweepAngle << std::endl;

  itkPrintSelfObjectMacro( RadiusImage );

  os << indent << "CirclesList: " << std::endl;
  unsigned int i = 0;
  auto it = m_CirclesList.begin();
  while( it != m_CirclesList.end() )
    {
    os << indent << "[" << i << "]: " << *it << std::endl;
    ++it;
    ++i;
    }

  os << indent << "OldModifiedTime: "
    << NumericTraits< ModifiedTimeType >::PrintType( m_OldModifiedTime )
    << std::endl;
}
} // end namespace

#endif
