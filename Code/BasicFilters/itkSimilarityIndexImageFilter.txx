/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarityIndexImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSimilarityIndexImageFilter_txx
#define _itkSimilarityIndexImageFilter_txx
#include "itkSimilarityIndexImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"

namespace itk {


template<class TInputImage1, class TInputImage2>
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::SimilarityIndexImageFilter(): m_CountOfImage1(1), m_CountOfImage2(1), m_CountOfIntersection(1)
{

  // this filter requires two input images
  this->SetNumberOfRequiredInputs( 2 );

  m_SimilarityIndex = NumericTraits<RealType>::Zero;      
}


template<class TInputImage1, class TInputImage2>
void
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::SetInput2( const TInputImage2 * image )
{
  this->SetNthInput(1, const_cast<TInputImage2 *>( image ) );      
}


template<class TInputImage1, class TInputImage2>
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::InputImage2ConstPointer
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::GetInput2()
{
  return static_cast< const TInputImage2 * >
                     (this->ProcessObject::GetInput(1).GetPointer());
}



template<class TInputImage1, class TInputImage2>
void
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // this filter requires all of both input images
  if ( this->GetInput1() )
    {
    InputImage1Pointer image =
        const_cast< InputImage1Type * >( this->GetInput1().GetPointer() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
  if ( this->GetInput2() )
    {
    InputImage2Pointer image =
        const_cast< InputImage2Type * >( this->GetInput2().GetPointer() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}


template<class TInputImage1, class TInputImage2>
void
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}


template<class TInputImage1, class TInputImage2>
void
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::AllocateOutputs()
{
  // Pass the first input through as the output
  InputImage1Pointer image =
      const_cast< TInputImage1 * >( this->GetInput1().GetPointer() );
  this->GraftOutput( image );
}


template<class TInputImage1, class TInputImage2>
void
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::BeforeThreadedGenerateData()
{
  int numberOfThreads = this->GetNumberOfThreads();

  // Resize the thread temporaries
  m_CountOfImage1.resize(numberOfThreads);
  m_CountOfImage2.resize(numberOfThreads);
  m_CountOfIntersection.resize(numberOfThreads);
  
  // Initialize the temporaries
  m_CountOfImage1.Fill(NumericTraits<unsigned long>::Zero);
  m_CountOfImage2.Fill(NumericTraits<unsigned long>::Zero);
  m_CountOfIntersection.Fill(NumericTraits<unsigned long>::Zero);
}


template<class TInputImage1, class TInputImage2>
void
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::AfterThreadedGenerateData()
{
  int i;
  unsigned long countImage1, countImage2, countIntersect;
    
  int numberOfThreads = this->GetNumberOfThreads();

  countImage1 = 0;
  countImage2 = 0;
  countIntersect = 0;

  // Accumulate counts over all threads
  for( i = 0; i < numberOfThreads; i++)
    {
    countImage1 += m_CountOfImage1[i];
    countImage2 += m_CountOfImage2[i];
    countIntersect += m_CountOfIntersection[i];
    }

  // compute overlap
  if ( !countImage1 && !countImage2 )
    {
    m_SimilarityIndex = NumericTraits<RealType>::Zero;
    return;
    }

  m_SimilarityIndex = 2.0 * static_cast<RealType>( countIntersect ) / 
   ( static_cast<RealType>( countImage1 ) + static_cast<RealType>( countImage2 ) );

}



template<class TInputImage1, class TInputImage2>
void
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::ThreadedGenerateData(const RegionType& regionForThread,
                       int threadId) 
{

  ImageRegionConstIterator<TInputImage1> it1 (this->GetInput1(), regionForThread);
  ImageRegionConstIterator<TInputImage2> it2 (this->GetInput2(), regionForThread);
  
  // support progress methods/callbacks
  unsigned long updateVisits = 0, i=0;
  if ( threadId == 0 )
    {
    updateVisits = regionForThread.GetNumberOfPixels()/10;
    if ( updateVisits < 1 ) updateVisits = 1;
    }

  // do the work
  while (!it1.IsAtEnd())
    {

    bool nonzero = false;
    if( it1.Get() != NumericTraits<InputImage1PixelType>::Zero )
      {
      m_CountOfImage1[threadId]++;
      nonzero = true;
      }
    if( it2.Get() != NumericTraits<InputImage2PixelType>::Zero )
      {
      m_CountOfImage2[threadId]++;
      if ( nonzero )
        {
        m_CountOfIntersection[threadId]++;
        }
      }
    ++it1;
    ++it2;

    if ( threadId == 0 && !(i % updateVisits ) )
      {
      this->UpdateProgress( static_cast<float>(i) / 
                            static_cast<float>(updateVisits * 10.0) );
      }
    ++i;
    }
}


template<class TInputImage1, class TInputImage2>
void 
SimilarityIndexImageFilter<TInputImage1, TInputImage2>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "SimilarityIndex: "  << m_SimilarityIndex << std::endl;
}


}// end namespace itk
#endif
