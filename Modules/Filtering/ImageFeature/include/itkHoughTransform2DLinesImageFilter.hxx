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
#ifndef itkHoughTransform2DLinesImageFilter_hxx
#define itkHoughTransform2DLinesImageFilter_hxx

#include "itkHoughTransform2DLinesImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkCastImageFilter.h"
#include "itkMath.h"

namespace itk
{
/** Constructor */
template< typename TInputPixelType, typename TOutputPixelType >
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >
::HoughTransform2DLinesImageFilter()
{
  m_Threshold = 0; // by default
  m_AngleResolution = 500;
  m_NumberOfLines = 1;
  m_DiscRadius = 10;
  m_Variance = 5;
  m_OldModifiedTime = 0;
  m_SimplifyAccumulator = ITK_NULLPTR;
}

template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImageConstPointer input  = this->GetInput();
  OutputImagePointer     output = this->GetOutput();

  if ( !input || !output )
    {
    return;
    }

  // Compute the size of the output image
  typename InputImageType::RegionType region;
  Size< 2 > size;

  size[0] =
    (SizeValueType)( std::sqrt(m_AngleResolution * m_AngleResolution
                                  + input->GetLargestPossibleRegion().GetSize()[0]
                                  * input->GetLargestPossibleRegion().GetSize()[0]) );
  size[1] = (SizeValueType)m_AngleResolution;
  region.SetSize(size);
  region.SetIndex( input->GetLargestPossibleRegion().GetIndex() );

  output->SetLargestPossibleRegion(region);
}

template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >
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

/** Generate the accumulator image */
template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >
::GenerateData()
{
  itkDebugMacro(<< "HoughTransform2DLinesImageFilter called");

  // Get the input and output pointers
  InputImageConstPointer inputImage  = this->GetInput(0);
  OutputImagePointer     outputImage = this->GetOutput(0);

  // Allocate the output
  this->AllocateOutputs();
  outputImage->FillBuffer(0);

  const double nPI = 4.0 * std::atan(1.0);

  ImageRegionConstIteratorWithIndex< InputImageType > image_it( inputImage,  inputImage->GetRequestedRegion() );
  image_it.GoToBegin();

  Index< 2 > index;

  while ( !image_it.IsAtEnd() )
    {
    if ( image_it.Get() > m_Threshold )
      {
      for ( double angle = -nPI; angle < nPI; angle += nPI / m_AngleResolution )
        {
        index[0] =
          // m_R
          (IndexValueType)( image_it.GetIndex()[0] * std::cos(angle) + image_it.GetIndex()[1] * std::sin(angle) );
        // m_Theta
        index[1] = (IndexValueType)( ( m_AngleResolution / 2 ) + m_AngleResolution * angle / ( 2 * nPI ) );

        if ( ( index[0] > 0 )
             && ( index[0] <= (IndexValueType)outputImage->GetBufferedRegion().GetSize()[0] ) )
        // the preceding "if" should be replacable with "if (
        // outputImage->GetBufferedRegion().IsInside(index) )" but
        // the algorithm fails if it is
          {
          outputImage->SetPixel(index, outputImage->GetPixel(index) + 1);
          }
        }
      }
    ++image_it;
    }
}

/** Simplify the accumulator
 * Do the same iteration process as the Update() method but find the maximum
 * along the curve and then remove the curve */
template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >
::Simplify(void)
{
  // Get the input and output pointers
  InputImageConstPointer inputImage = this->GetInput(0);
  OutputImagePointer     outputImage = this->GetOutput(0);

  if ( !inputImage || !outputImage )
    {
    itkExceptionMacro("Update() must be called before Simplify().");
    }

  /** Allocate the simplify accumulator */
  m_SimplifyAccumulator = OutputImageType::New();
  m_SimplifyAccumulator->SetRegions( outputImage->GetLargestPossibleRegion() );
  m_SimplifyAccumulator->SetOrigin( inputImage->GetOrigin() );
  m_SimplifyAccumulator->SetSpacing( inputImage->GetSpacing() );
  m_SimplifyAccumulator->SetDirection( inputImage->GetDirection() );
  m_SimplifyAccumulator->Allocate(true); // initialize
                                                                // buffer
                                                                // to zero

  Index< 2 > index;
  Index< 2 > maxIndex;

  typename OutputImageType::PixelType value;
  typename OutputImageType::PixelType valuemax;

  ImageRegionConstIteratorWithIndex< InputImageType > image_it( inputImage,  inputImage->GetRequestedRegion() );
  image_it.GoToBegin();

  const double nPI = 4.0 * std::atan(1.0);

  while ( !image_it.IsAtEnd() )
    {
    if ( image_it.Get() > m_Threshold )
      {
      // Look for maximum along the curve and remove the curve at the same time
      valuemax = -1;
      maxIndex[0] = 0;
      maxIndex[1] = 0;
      for ( double angle = -nPI; angle < nPI; angle += nPI / m_AngleResolution )
        {
        // m_R
        index[0] = (IndexValueType)( image_it.GetIndex()[0] * std::cos(angle) + image_it.GetIndex()[1] * std::sin(angle) );
        // m_Theta
        index[1] = (IndexValueType)( ( m_AngleResolution / 2 ) + m_AngleResolution * angle / ( 2 * nPI ) );

        if ( outputImage->GetBufferedRegion().IsInside(index) )
          {
          value = outputImage->GetPixel(index);
          if ( value > valuemax )
            {
            valuemax = value;
            maxIndex = index;
            }
          }
        }
      m_SimplifyAccumulator->SetPixel(maxIndex, m_SimplifyAccumulator->GetPixel(maxIndex) + 1);
      }
    ++image_it;
    }

  ImageRegionConstIteratorWithIndex< OutputImageType > accusimple_it( m_SimplifyAccumulator,
                                                                      m_SimplifyAccumulator->GetRequestedRegion() );
  ImageRegionIteratorWithIndex< OutputImageType > accu_it( outputImage,  outputImage->GetRequestedRegion() );

  accusimple_it.GoToBegin();
  accu_it.GoToBegin();

  while ( !accusimple_it.IsAtEnd() )
    {
    accu_it.Set( accusimple_it.Get() );
    ++accu_it;
    ++accusimple_it;
    }
}

/** Get the list of lines. This recomputes the lines */
template< typename TInputPixelType, typename TOutputPixelType >
typename HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >::LinesListType &
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >
::GetLines()
{
  // if the filter has not been updated
  if ( this->GetMTime() == m_OldModifiedTime )
    {
    return m_LinesList;
    }

  m_LinesList.clear();

  if ( m_NumberOfLines > 0 )
    {
    /** Blur the accumulator in order to find the maximum */
    typedef float                              InternalImagePixelType;
    typedef Image< InternalImagePixelType, 2 > InternalImageType;

    OutputImagePointer outputImage = this->GetOutput(0);

    if ( !outputImage )
      {
      itkExceptionMacro("Update() must be called before GetLines().");
      }

    /** xxxConvert the accumulator output image type to internal image type */
    typedef CastImageFilter< OutputImageType, InternalImageType > CastImageFilterType;

    const typename CastImageFilterType::Pointer castImageFilter = CastImageFilterType::New();
    castImageFilter->SetInput(outputImage);

    typedef DiscreteGaussianImageFilter< InternalImageType, InternalImageType > GaussianFilterType;
    const typename GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();

    // the output is the accumulator image
    gaussianFilter->SetInput( castImageFilter->GetOutput() );
    gaussianFilter->SetVariance(m_Variance);
    gaussianFilter->Update();
    const InternalImageType::Pointer postProcessImage = gaussianFilter->GetOutput();

    typedef MinimumMaximumImageCalculator< InternalImageType > MinMaxCalculatorType;
    typename MinMaxCalculatorType::Pointer minMaxCalculator = MinMaxCalculatorType::New();
    itk::ImageRegionIterator< InternalImageType >
    it_input( postProcessImage, postProcessImage->GetLargestPossibleRegion() );


    itk::Index< 2 > index;

    unsigned int lines = 0;

    // Find maxima
    do
      {
      minMaxCalculator->SetImage(postProcessImage);
      minMaxCalculator->ComputeMaximum();
      InternalImageType::PixelType max = minMaxCalculator->GetMaximum();

      if ( max <= 0 )
        {
        // When all pixel values in 'postProcessImage' are zero or less, no more lines
        // should be found. Note that a zero in 'postProcessImage' might correspond to a
        // zero in the accumulator image, or it might be that the pixel is within a
        // removed disc.
        break;
        }

      for ( it_input.GoToBegin(); !it_input.IsAtEnd(); ++it_input )
        {
        if ( Math::ExactlyEquals(it_input.Get(), max) )
          {
          // Create the line
          LineType::PointListType list; // insert two points per line

          double radius = it_input.GetIndex()[0];
          double teta   = ( ( it_input.GetIndex()[1] ) * 2 * Math::pi / this->GetAngleResolution() ) - Math::pi;
          double Vx = radius * std::cos(teta);
          double Vy = radius * std::sin(teta);
          double norm = std::sqrt(Vx * Vx + Vy * Vy);
          double VxNorm = Vx / norm;
          double VyNorm = Vy / norm;

          if ( ( teta <= 0 ) || ( teta >= Math::pi / 2 ) )
            {
            if ( teta >= Math::pi / 2 )
              {
              VyNorm = -VyNorm;
              VxNorm = -VxNorm;
              }

            LinePointType p;
            p.SetPosition(Vx, Vy);
            list.push_back(p);
            p.SetPosition(Vx - VyNorm * 5, Vy + VxNorm * 5);
            list.push_back(p);
            }
          else // if teta>0
            {
            LinePointType p;
            p.SetPosition(Vx, Vy);
            list.push_back(p);
            p.SetPosition(Vx - VyNorm * 5, Vy + VxNorm * 5);
           list.push_back(p);
            } // end if(teta>0)

          // Create a Line Spatial Object
          LinePointer Line = LineType::New();
          Line->SetId(lines);
          Line->SetPoints(list);
          Line->ComputeBoundingBox();

          m_LinesList.push_back(Line);

          // Remove a black disc from the hough space domain
          for ( double angle = 0; angle <= 2 * Math::pi; angle += Math::pi / 1000 )
            {
            for ( double length = 0; length < m_DiscRadius; length += 1 )
              {
              index[0] = (IndexValueType)( it_input.GetIndex()[0] + length * std::cos(angle) );
              index[1] = (IndexValueType)( it_input.GetIndex()[1] + length * std::sin(angle) );
              if ( postProcessImage->GetBufferedRegion().IsInside(index) )
                {
                postProcessImage->SetPixel(index, 0);
                }
              }
            }
          minMaxCalculator->SetImage(postProcessImage);
          minMaxCalculator->ComputeMaximum();
          max = minMaxCalculator->GetMaximum();

          lines++;
          if ( lines == m_NumberOfLines ) { break; }
          }
        }
      }
    while ( lines < m_NumberOfLines );
  }

  m_OldModifiedTime = this->GetMTime();
  return m_LinesList;
}

#if !defined(ITK_LEGACY_REMOVE)
template< typename TInputPixelType, typename TOutputPixelType >
typename HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >::LinesListType &
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >
::GetLines(unsigned int)
{
  return this->GetLines();
}
#endif

/** Print Self information */
template< typename TInputPixelType, typename TOutputPixelType >
void
HoughTransform2DLinesImageFilter< TInputPixelType, TOutputPixelType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "Angle Resolution: " << m_AngleResolution << std::endl;
  os << indent << "Number Of Lines: " << m_NumberOfLines << std::endl;
  os << indent << "Disc Radius: " << m_DiscRadius << std::endl;
  os << indent << "Accumulator blur variance: " << m_Variance << std::endl;
  os << indent << "Simplify Accumulator" << m_SimplifyAccumulator << std::endl;

  os << indent << "LinesList: " << std::endl;
  unsigned int i = 0;
  LinesListType::const_iterator it = m_LinesList.begin();
  while( it != m_LinesList.end() )
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
