/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoxImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBoxImageFilter_txx
#define __itkBoxImageFilter_txx

#include "itkBoxImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
BoxImageFilter< TInputImage, TOutputImage >
::BoxImageFilter()
{
  m_Radius.Fill(1);     // a good arbitary starting point
}

template< class TInputImage, class TOutputImage >
void
BoxImageFilter< TInputImage, TOutputImage >
::SetRadius(const RadiusType & radius)
{
  if ( m_Radius != radius )
    {
    m_Radius = radius;
    this->Modified();
    }
}

template< class TInputImage, class TOutputImage >
void
BoxImageFilter< TInputImage, TOutputImage >
::SetRadius(const unsigned long & radius)
{
  RadiusType rad;

  rad.Fill(radius);
  this->SetRadius(rad);
}

template< class TInputImage, class TOutputImage >
void
BoxImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );

  if ( !inputPtr )
    {
    return;
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius(m_Radius);

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    std::ostringstream          msg;
    msg << static_cast< const char * >( this->GetNameOfClass() )
        << "::GenerateInputRequestedRegion()";
    e.SetLocation( msg.str().c_str() );
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< class TInputImage, class TOutputImage >
void
BoxImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Radius: " << m_Radius << std::endl;
}
}

#endif
