/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkApproximateSignedDistanceMapImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkApproximateSignedDistanceMapImageFilter_txx
#define _itkApproximateSignedDistanceMapImageFilter_txx

#include "itkApproximateSignedDistanceMapImageFilter.h"

#include "itkNumericTraits.h"
#include "itkImageRegionIterator.h"
#include "itkProgressAccumulator.h"
#include "vcl_cmath.h"

namespace itk { 

  /*
   * Default constructor.
   */
  template <class TInputImage, class TOutputImage>
  ApproximateSignedDistanceMapImageFilter<TInputImage,TOutputImage>
  ::ApproximateSignedDistanceMapImageFilter() :
    m_IsoContourFilter( IsoContourType::New() ),
    m_ChamferFilter( ChamferType::New() ),
    m_InsideValue( NumericTraits<InputPixelType>::min() ),
    m_OutsideValue( NumericTraits<InputPixelType>::max() )
  {
    // I'm not setting any of the ReleaseDataFlag values for the mini-pipeline 
    // filters. Should I be?
  }

  /*
   * Generate Data.
   */
  template <class TInputImage, class TOutputImage>
  void
  ApproximateSignedDistanceMapImageFilter<TInputImage,TOutputImage>
  ::GenerateData()
    {
    // calculate the largest possible distance in the output image.
    // this maximum is the distance from one corner of the image to the other.
    OutputSizeType outputSize = this->GetOutput()->GetRequestedRegion().GetSize();
    OutputSizeValueType maximumDistance = 0;
    for (int i = 0; i < InputImageDimension; i++)
      {
      maximumDistance += outputSize[i]*outputSize[i];
      }
    // cast to float and back because there's no sqrt defined for unsigned long double,
    // which is the general SizeValueType.
    maximumDistance = 
      static_cast<OutputSizeValueType>(vcl_sqrt(static_cast<float>(maximumDistance))); 
    
    // Allocate the output
    this->AllocateOutputs();
        
    // Create a process accumulator for tracking the progress of this minipipeline
    ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
    progress->SetMiniPipelineFilter(this);
    progress->RegisterInternalFilter(m_IsoContourFilter, 0.5f);
    progress->RegisterInternalFilter(m_ChamferFilter, 0.5f);
    
    // set up the isocontour filter
    m_IsoContourFilter->SetInput(this->GetInput());
    m_IsoContourFilter->SetFarValue(maximumDistance + 1);
    InputPixelType levelSetValue = (m_InsideValue + m_OutsideValue) / 2;
    m_IsoContourFilter->SetLevelSetValue(levelSetValue);
    
    // set up the chamfer filter
    m_ChamferFilter->SetInput(m_IsoContourFilter->GetOutput());
    m_ChamferFilter->SetMaximumDistance(maximumDistance);

    // graft our output to the chamfer filter to force the proper regions
    // to be generated
    m_ChamferFilter->GraftOutput( this->GetOutput() );
    
    // create the distance map
    m_ChamferFilter->Update();
    
    // graft the output of the chamfer filter back onto this filter's
    // output. this is needed to get the appropriate regions passed
    // back.
    this->GraftOutput( m_ChamferFilter->GetOutput() );
    
    // Recall that we set the isocontour value to halfway between the inside and 
    // oustide value. The above filters assume that regions "inside" objects are
    // those with values *less* than the isocontour. This assumption is violated
    // if the "inside" intensity value is greater than the "outside" value.
    // (E.g. in the case that we're computing the distance from a mask where the
    // background is zero and the objects are colored 255.)
    // In this case, the distance will be calculated negative, so we need to 
    // flip the sign of the output image.
    if (m_InsideValue > m_OutsideValue)  
      {
      ImageRegionIterator< OutputImageType > ot( this->GetOutput(), 
        this->GetOutput()->GetRequestedRegion() );
      ot.GoToBegin();
      while( !ot.IsAtEnd() )
        {
        ot.Set(ot.Get() * -1);
        ++ot;
        }
      }
    }

  template<class TInputImage, class TOutputImage>
  void
  ApproximateSignedDistanceMapImageFilter<TInputImage, TOutputImage>
  ::PrintSelf(std::ostream &os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    
    os << indent << "Inside intensity value: " << m_InsideValue << std::endl;
    os << indent << "Outside intensity value: " << m_OutsideValue << std::endl;
    os << indent << "IsoContourDistanceImageFilter (used internally): "
      << m_IsoContourFilter << std::endl;
    os << indent << "FastChamferDistanceImageFilter (used internally): "
      << m_ChamferFilter << std::endl;

  }
  

} // end of namespace itk

#endif


