/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHausdorffDistanceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkHausdorffDistanceImageFilter_txx
#define _itkHausdorffDistanceImageFilter_txx
#include "itkHausdorffDistanceImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include "itkDirectedHausdorffDistanceImageFilter.h"

namespace itk {


template<class TInputImage1, class TInputImage2>
HausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::HausdorffDistanceImageFilter()
{

  // this filter requires two input images
  this->SetNumberOfRequiredInputs( 2 );

  m_HausdorffDistance = NumericTraits<RealType>::Zero;      
}


template<class TInputImage1, class TInputImage2>
void
HausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::SetInput2( const TInputImage2 * image )
{
  this->SetNthInput(1, const_cast<TInputImage2 *>( image ) );      
}


template<class TInputImage1, class TInputImage2>
const typename HausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::InputImage2Type *
HausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::GetInput2()
{
  return static_cast< const TInputImage2 * >
                     (this->ProcessObject::GetInput(1));
}



template<class TInputImage1, class TInputImage2>
void
HausdorffDistanceImageFilter<TInputImage1, TInputImage2>
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
HausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}


template<class TInputImage1, class TInputImage2>
void
HausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::AllocateOutputs()
{
  // Pass the first input through as the output
  InputImage1Pointer image =
      const_cast< TInputImage1 * >( this->GetInput1() );
  this->GraftOutput( image );
}


template<class TInputImage1, class TInputImage2>
void
HausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::GenerateData()
{

  RealType distance12, distance21;

  {
  typedef DirectedHausdorffDistanceImageFilter<InputImage1Type,InputImage2Type>
    FilterType;

  typename FilterType::Pointer filter = FilterType::New();

  filter->SetInput1( this->GetInput1() );
  filter->SetInput2( this->GetInput2() );
  filter->Update();

  distance12 = filter->GetDirectedHausdorffDistance();
  }

  {
  typedef DirectedHausdorffDistanceImageFilter<InputImage2Type,InputImage1Type>
    FilterType;

  typename FilterType::Pointer filter = FilterType::New();

  filter->SetInput1( this->GetInput2() );
  filter->SetInput2( this->GetInput1() );
  filter->Update();

  distance21 = filter->GetDirectedHausdorffDistance();
  }

  if ( distance12 > distance21 )
    {
    m_HausdorffDistance = distance12;
    }
  else
    {
    m_HausdorffDistance = distance21;
    }

}



template<class TInputImage1, class TInputImage2>
void 
HausdorffDistanceImageFilter<TInputImage1, TInputImage2>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "HausdorffDistance: "  
     << m_HausdorffDistance << std::endl;
}


}// end namespace itk
#endif
