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

namespace itk
{
template< typename TInputPixelType, typename TOutputPixelType >
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType >
::HoughTransform2DCirclesImageFilter()
{
  m_Threshold = 0;
  m_MinimumRadius = 0;  // by default
  m_MaximumRadius = 10; // by default
  m_SigmaGradient = 1;  // Scale of the DoG filter
  m_DiscRadiusRatio = 1;
  m_Variance = 10;
  m_OldModifiedTime = 0;
  m_OldNumberOfCircles = 0;
  m_SweepAngle = 0.0;
  m_NumberOfCircles = 1;
}

template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType >
::SetRadius(double radius)
{
  this->SetMinimumRadius(radius);
  this->SetMaximumRadius(radius);
}

template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType >
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

template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType >
::GenerateData()
{
  // Get the input and output pointers
  InputImageConstPointer inputImage = this->GetInput(0);
  OutputImagePointer     outputImage = this->GetOutput(0);

  // Allocate the output
  this->AllocateOutputs();
  outputImage->FillBuffer(0);

  typedef GaussianDerivativeImageFunction< InputImageType > DoGFunctionType;
  typename DoGFunctionType::Pointer DoGFunction = DoGFunctionType::New();
  DoGFunction->SetInputImage(inputImage);
  DoGFunction->SetSigma(m_SigmaGradient);

  m_RadiusImage = OutputImageType::New();

  m_RadiusImage->SetRegions( outputImage->GetLargestPossibleRegion() );
  m_RadiusImage->SetOrigin( inputImage->GetOrigin() );
  m_RadiusImage->SetSpacing( inputImage->GetSpacing() );
  m_RadiusImage->SetDirection( inputImage->GetDirection() );
  m_RadiusImage->Allocate(true); // initialize
                                                        // buffer to zero

  ImageRegionConstIteratorWithIndex< InputImageType > image_it( inputImage,  inputImage->GetRequestedRegion() );
  image_it.GoToBegin();

  Index< 2 >        index;
  Point< float, 2 > point;

  while ( !image_it.IsAtEnd() )
    {
    if ( image_it.Get() > m_Threshold )
      {
      point[0] = image_it.GetIndex()[0];
      point[1] = image_it.GetIndex()[1];
      typename DoGFunctionType::VectorType grad = DoGFunction->EvaluateAtIndex( image_it.GetIndex() );

      double Vx = grad[0];
      double Vy = grad[1];

      // if the gradient is not flat
      if ( ( std::fabs(Vx) > 1 ) || ( std::fabs(Vy) > 1 ) )
        {
        double norm = std::sqrt(Vx * Vx + Vy * Vy);
        Vx /= norm;
        Vy /= norm;

        for ( double angle = -m_SweepAngle; angle <= m_SweepAngle; angle += 0.05 )
          {
          double i = m_MinimumRadius;
          double distance;

          do
            {
            index[0] = (IndexValueType)( point[0] - i * ( Vx * std::cos(angle) + Vy * std::sin(angle) ) );
            index[1] = (IndexValueType)( point[1] - i * ( Vx * std::sin(angle) + Vy * std::cos(angle) ) );

            distance = std::sqrt( ( index[1] - point[1] ) * ( index[1] - point[1] )
                                 + ( index[0] - point[0] ) * ( index[0] - point[0] ) );

            if ( outputImage->GetRequestedRegion().IsInside(index) )
              {
              outputImage->SetPixel(index, outputImage->GetPixel(index) + 1);
              m_RadiusImage->SetPixel( index, ( m_RadiusImage->GetPixel(index) + distance ) );
              }

            i = i + 1;
            }
          while ( outputImage->GetRequestedRegion().IsInside(index)
                  && ( distance < m_MaximumRadius ) );
          }
        }
      }
    ++image_it;
    }

  // Compute the average radius
  ImageRegionConstIterator< OutputImageType > output_it( outputImage, outputImage->GetLargestPossibleRegion() );
  ImageRegionIterator< OutputImageType >      radius_it( m_RadiusImage, m_RadiusImage->GetLargestPossibleRegion() );
  output_it.GoToBegin();
  radius_it.GoToBegin();
  while ( !output_it.IsAtEnd() )
    {
    if ( output_it.Get() > 0 )
      {
      radius_it.Set( radius_it.Get() / output_it.Get() );
      }
    ++output_it;
    ++radius_it;
    }
}

/** Get the list of circles. This recomputes the circles */
template< typename TInputPixelType, typename TOutputPixelType >
typename HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType >::CirclesListType &
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType >
::GetCircles(unsigned int n)
{
  if ( ( this->GetMTime() == m_OldModifiedTime ) && ( n == m_OldNumberOfCircles ) )
    {
    // if the filter has not been updated
    return m_CirclesList;
    }

  m_CirclesList.clear();

  /** Blur the accumulator in order to find the maximum */
  typedef Image< float, 2 > InternalImageType;

  OutputImagePointer outputImage = OutputImageType::New();
  outputImage->SetRegions( this->GetOutput(0)->GetLargestPossibleRegion() );
  outputImage->SetOrigin( this->GetOutput(0)->GetOrigin() );
  outputImage->SetSpacing( this->GetOutput(0)->GetSpacing() );
  outputImage->SetDirection( this->GetOutput(0)->GetDirection() );
  outputImage->Allocate(true); // initialize
                                                      // buffer to zero

  ImageRegionConstIteratorWithIndex< OutputImageType > image_it( this->GetOutput(0),  this->GetOutput(
                                                                   0)->GetRequestedRegion() );
  image_it.GoToBegin();

  ImageRegionIterator< OutputImageType > it( outputImage,  outputImage->GetRequestedRegion() );

  while ( !image_it.IsAtEnd() )
    {
    it.Set( image_it.Get() );
    ++image_it;
    ++it;
    }

  typedef DiscreteGaussianImageFilter< OutputImageType, InternalImageType > GaussianFilterType;
  typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();

  gaussianFilter->SetInput(outputImage); // the output is the accumulator image
  double variance[2];
  variance[0] = m_Variance;
  variance[1] = m_Variance;
  gaussianFilter->SetVariance(variance);
  gaussianFilter->Update();
  typename InternalImageType::Pointer postProcessImage = gaussianFilter->GetOutput();

  typedef MinimumMaximumImageCalculator< InternalImageType > MinMaxCalculatorType;
  typename MinMaxCalculatorType::Pointer minMaxCalculator = MinMaxCalculatorType::New();
  ImageRegionIterator< InternalImageType > it_input( postProcessImage, postProcessImage->GetLargestPossibleRegion() );

  Index< 2 > index;

  CirclesListSizeType circles = 0;
  bool         found;

  const double nPI = 4.0 * std::atan(1.0);

  // Find maxima
  do
    {
    minMaxCalculator->SetImage(postProcessImage);
    minMaxCalculator->ComputeMaximum();
    InternalImageType::PixelType max = minMaxCalculator->GetMaximum();

    found = false;
    for ( it_input.GoToBegin(); !it_input.IsAtEnd(); ++it_input )
      {
      if ( it_input.Get() == max )
        {
        // Create a Line Spatial Object
        CirclePointer Circle = CircleType::New();
        Circle->SetId(circles);
        Circle->SetRadius( m_RadiusImage->GetPixel( it_input.GetIndex() ) );

        CircleType::VectorType center;
        center[0] = it_input.GetIndex()[0];
        center[1] = it_input.GetIndex()[1];
        Circle->GetObjectToParentTransform()->SetOffset(center);
        Circle->ComputeBoundingBox();

        m_CirclesList.push_back(Circle);

        // Remove a black disc from the hough space domain
        for ( double angle = 0; angle <= 2 * nPI; angle += nPI / 1000 )
          {
          for ( double length = 0; length < m_DiscRadiusRatio * Circle->GetRadius()[0]; length += 1 )
            {
            index[0] = (IndexValueType)( it_input.GetIndex()[0] + length * std::cos(angle) );
            index[1] = (IndexValueType)( it_input.GetIndex()[1] + length * std::sin(angle) );
            if ( postProcessImage->GetLargestPossibleRegion().IsInside(index) )
              {
              postProcessImage->SetPixel(index, 0);
              }
            }
          }
        minMaxCalculator->SetImage(postProcessImage);
        minMaxCalculator->ComputeMaximum();
        max = minMaxCalculator->GetMaximum();

        circles++;
        found = true;
        if ( circles == m_NumberOfCircles ) { break; }
        }
      }
    }
  while ( ( circles < m_NumberOfCircles ) && ( found ) );

  m_OldModifiedTime = this->GetMTime();
  m_OldNumberOfCircles = m_CirclesList.size();
  return m_CirclesList;
}

/** Print Self information */
template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DCirclesImageFilter< TInputPixelType, TOutputPixelType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "Threshold: " << m_Threshold << std::endl;
  os << "Minimum Radius:  " << m_MinimumRadius << std::endl;
  os << "Maximum Radius: " << m_MaximumRadius << std::endl;
  os << "Derivative Scale : " << m_SigmaGradient << std::endl;
  os << "Radius Image Information : " << m_RadiusImage << std::endl;
  os << "Number Of Circles: " << m_NumberOfCircles << std::endl;
  os << "Disc Radius: " << m_DiscRadiusRatio << std::endl;
  os << "Accumulator blur variance: " << m_Variance << std::endl;
  os << "Sweep angle : " << m_SweepAngle << std::endl;
}
} // end namespace

#endif
