/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectedHausdorffDistanceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDirectedHausdorffDistanceImageFilter_txx
#define _itkDirectedHausdorffDistanceImageFilter_txx
#include "itkDirectedHausdorffDistanceImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkProgressReporter.h"

namespace itk {


template<class TInputImage1, class TInputImage2>
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::DirectedHausdorffDistanceImageFilter(): m_MaxDistance(1)
{

  // this filter requires two input images
  this->SetNumberOfRequiredInputs( 2 );

  m_DistanceMap = NULL;
  m_DirectedHausdorffDistance = NumericTraits<RealType>::Zero;      
}


template<class TInputImage1, class TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::SetInput2( const TInputImage2 * image )
{
  this->SetNthInput(1, const_cast<TInputImage2 *>( image ) );      
}


template<class TInputImage1, class TInputImage2>
const typename DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::InputImage2Type *
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::GetInput2()
{
  return static_cast< const TInputImage2 * >
                     (this->ProcessObject::GetInput(1));
}



template<class TInputImage1, class TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // this filter requires:
  // - the largeset possible region of the first image
  // - the corresponding region of the second image
  if ( this->GetInput1() )
    {
    InputImage1Pointer image =
        const_cast< InputImage1Type * >( this->GetInput1() );
    image->SetRequestedRegionToLargestPossibleRegion();

    if ( this->GetInput2() )
      {
      InputImage2Pointer image =
          const_cast< InputImage2Type * >( this->GetInput2() );
      image->SetRequestedRegion( 
        this->GetInput1()->GetRequestedRegion() );
      }

    }
}


template<class TInputImage1, class TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}


template<class TInputImage1, class TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::AllocateOutputs()
{
  // Pass the first input through as the output
  InputImage1Pointer image =
      const_cast< TInputImage1 * >( this->GetInput1() );
  this->GraftOutput( image );
}


template<class TInputImage1, class TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::BeforeThreadedGenerateData()
{
  int numberOfThreads = this->GetNumberOfThreads();

  // Resize the thread temporaries
  m_MaxDistance.resize(numberOfThreads);
  
  // Initialize the temporaries
  m_MaxDistance.Fill(NumericTraits<RealType>::Zero);

  // Compute Danielsson distance from non-zero pixels in the second image
  typedef itk::DanielssonDistanceMapImageFilter<InputImage2Type,DistanceMapType>
    FilterType;

  typename FilterType::Pointer filter = FilterType::New();

  filter->SetInput( this->GetInput2() );
  filter->Update();

  m_DistanceMap = filter->GetOutput();

}


template<class TInputImage1, class TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::AfterThreadedGenerateData()
{
  int i;
    
  int numberOfThreads = this->GetNumberOfThreads();

  m_DirectedHausdorffDistance = NumericTraits<RealType>::Zero;

  // find max over all threads
  for( i = 0; i < numberOfThreads; i++)
    {
    if ( m_MaxDistance[i] > m_DirectedHausdorffDistance )
      {
      m_DirectedHausdorffDistance = m_MaxDistance[i];
      }
    }

  // clean up
  m_DistanceMap = NULL;

}



template<class TInputImage1, class TInputImage2>
void
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::ThreadedGenerateData(const RegionType& regionForThread,
                       int threadId) 
{

  ImageRegionConstIterator<TInputImage1> it1 (this->GetInput1(), regionForThread);
  ImageRegionConstIterator<DistanceMapType> it2 (m_DistanceMap, regionForThread);
  
  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, regionForThread.GetNumberOfPixels());
  
  // do the work
  while (!it1.IsAtEnd())
    {

    if( it1.Get() != NumericTraits<InputImage1PixelType>::Zero )
      {
      if ( it2.Get() > m_MaxDistance[threadId] )
        {
        m_MaxDistance[threadId] = it2.Get();
        }
      }

    ++it1;
    ++it2;

    progress.CompletedPixel();

    }
}


template<class TInputImage1, class TInputImage2>
void 
DirectedHausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "DirectedHausdorffDistance: "  
     << m_DirectedHausdorffDistance << std::endl;
}


}// end namespace itk
#endif
