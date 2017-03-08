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
#ifndef itkValuedRegionalExtremaImageFilter_hxx
#define itkValuedRegionalExtremaImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkValuedRegionalExtremaImageFilter.h"
#include "itkProgressReporter.h"
#include "itkConnectedComponentAlgorithm.h"


namespace itk
{

template< typename TInputImage, typename TOutputImage, typename TFunction1,
          typename TFunction2 >
ValuedRegionalExtremaImageFilter< TInputImage, TOutputImage, TFunction1,
                                  TFunction2 >
::ValuedRegionalExtremaImageFilter():
  m_MarkerValue( 0 ),
  m_FullyConnected( false ),
  // not really useful, just to always have the same value before
  //the filter has run
  m_Flat( false )
{
}


template< typename TInputImage, typename TOutputImage, typename TFunction1,
          typename TFunction2 >
void
ValuedRegionalExtremaImageFilter< TInputImage, TOutputImage, TFunction1,
                                  TFunction2 >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImageType * input = const_cast< InputImageType * >( this->GetInput() );
  if ( !input )
    {
    return;
    }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template< typename TInputImage, typename TOutputImage, typename TFunction1,
          typename TFunction2 >
void
ValuedRegionalExtremaImageFilter< TInputImage, TOutputImage, TFunction1,
                                  TFunction2 >
::EnlargeOutputRequestedRegion(DataObject *)
{
  OutputImageType * output = this->GetOutput();
  output->SetRequestedRegion( output->GetLargestPossibleRegion() );
}


template< typename TInputImage, typename TOutputImage, typename TFunction1,
          typename TFunction2 >
void
ValuedRegionalExtremaImageFilter< TInputImage, TOutputImage, TFunction1,
                                  TFunction2 >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  const InputImageType * input = this->GetInput();
  OutputImageType * output = this->GetOutput();

  // 2 phases
  ProgressReporter progress(this, 0,
                            this->GetOutput()->GetRequestedRegion().GetNumberOfPixels() * 2);

  // copy input to output - isn't there a better way?
  typedef ImageRegionConstIterator< TInputImage > InputIterator;
  typedef ImageRegionIterator< TOutputImage >     OutputIterator;

  InputIterator inIt( input,
                      output->GetRequestedRegion() );
  OutputIterator outIt( output,
                        output->GetRequestedRegion() );
  inIt.GoToBegin();
  outIt.GoToBegin();

  InputImagePixelType firstValue = inIt.Get();
  this->m_Flat = true;

  while( !outIt.IsAtEnd() )
    {
    InputImagePixelType currentValue = inIt.Get();
    outIt.Set( static_cast< OutputImagePixelType >( currentValue ) );
    if ( currentValue != firstValue )
      {
      this->m_Flat = false;
      }
    ++inIt;
    ++outIt;
    progress.CompletedPixel();
    }

  // if the image is flat, there is no need to do the work:
  // the image will be unchanged
  if( !this->m_Flat )
    {
    // Now for the real work!
    // More iterators - use shaped ones so that we can set connectivity
    // Note : all comments refer to finding regional minima, because
    // it is briefer and clearer than trying to describe both regional
    // maxima and minima processes at the same time
    ISizeType kernelRadius;
    kernelRadius.Fill( 1 );
    NOutputIterator outNIt( kernelRadius,
                            output,
                            output->GetRequestedRegion() );
    setConnectivity(&outNIt, m_FullyConnected);

    ConstInputIterator inNIt( kernelRadius,
                              input,
                              output->GetRequestedRegion() );
    setConnectivity(&inNIt, m_FullyConnected);

    ConstantBoundaryCondition< OutputImageType > iBC;
    iBC.SetConstant(m_MarkerValue);
    inNIt.OverrideBoundaryCondition(&iBC);

    ConstantBoundaryCondition< OutputImageType > oBC;
    oBC.SetConstant(m_MarkerValue);
    outNIt.OverrideBoundaryCondition(&oBC);

    TFunction1 compareIn;
    TFunction2 compareOut;

    outIt.GoToBegin();
    // set up the stack and neighbor list
    IndexStack IS;
    typename NOutputIterator::IndexListType IndexList;
    IndexList = outNIt.GetActiveIndexList();

    while( !outIt.IsAtEnd() )
      {
      OutputImagePixelType V = outIt.Get();
      // if the output pixel value = the marker value then we have
      // already visited this pixel and don't need to do so again
      if ( compareOut(V, m_MarkerValue) )
        {
        // reposition the input iterator
        inNIt += outIt.GetIndex() - inNIt.GetIndex();

        InputImagePixelType Cent = static_cast< InputImagePixelType >( V );

        // check each neighbor of the input pixel
        typename ConstInputIterator::ConstIterator sIt;
        for ( sIt = inNIt.Begin(); !sIt.IsAtEnd(); ++sIt )
          {
          InputImagePixelType Adjacent = sIt.Get();
          if ( compareIn(Adjacent, Cent) )
            {
            // The centre pixel cannot be part of a regional minima
            // because one of its neighbors is smaller.
            // Set all pixels in the output image that are connected to
            // the centre pixel and have the same value to
            // m_MarkerValue
            // This is the flood filling step. It is a simple, stack
            // based, procedure. The original value (V) of the pixel is
            // recorded and the pixel index in the output image
            // is set to the marker value. The stack is initialized
            // with the pixel index. The flooding procedure pops the
            // stack, sets that index to the marker value and places the
            // indexes of all neighbors with value V on the stack. The
            // process terminates when the stack is empty.

            outNIt += outIt.GetIndex() - outNIt.GetIndex();

            OutputImagePixelType NVal;
            OutIndexType         idx;
            // Initialize the stack
            IS.push( outNIt.GetIndex() );
            outNIt.SetCenterPixel(m_MarkerValue);

            typename NOutputIterator::IndexListType::const_iterator LIt;

            while( !IS.empty() )
              {
              // Pop the stack
              idx = IS.top();
              IS.pop();
              // position the iterator
              outNIt += idx - outNIt.GetIndex();
              // check neighbors
              for ( LIt = IndexList.begin(); LIt != IndexList.end(); ++LIt )
                {
                NVal = outNIt.GetPixel(*LIt);
                if ( NVal == V )
                  {
                  // still in a flat zone
                  IS.push( outNIt.GetIndex(*LIt) );

                  // set the output as the marker value
                  outNIt.SetPixel(*LIt, m_MarkerValue);
                  }
                }
              }
            // end flooding
            break;
            }
          }
        }
      ++outIt;
      progress.CompletedPixel();
      }
    }
}


template< typename TInputImage, typename TOutputImage, typename TFunction1,
          typename TFunction2 >
void
ValuedRegionalExtremaImageFilter< TInputImage, TOutputImage, TFunction1,
                                  TFunction2 >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "Flat: "            << m_Flat << std::endl;
  os << indent << "MarkerValue: "     << m_MarkerValue << std::endl;
}

} // end namespace itk

#endif
