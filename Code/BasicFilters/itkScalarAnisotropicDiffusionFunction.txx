/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarAnisotropicDiffusionFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkScalarAnisotropicDiffusionFunction_txx_
#define __itkScalarAnisotropicDiffusionFunction_txx_

#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkDerivativeOperator.h"

namespace itk {

template <class TImage>
void
ScalarAnisotropicDiffusionFunction<TImage>
::CalculateAverageGradientMagnitudeSquared(TImage *ip)
{
  typedef ConstNeighborhoodIterator<TImage>      RNI_type;
  typedef ConstNeighborhoodIterator<TImage> SNI_type;
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TImage> BFC_type;

  unsigned int i;
  ZeroFluxNeumannBoundaryCondition<TImage>  bc;
  PixelType                                 accumulator;
  PixelType                                 val;
  PixelType                                 counter;
  BFC_type                                  bfc;
  typename BFC_type::FaceListType           faceList;
  typename RNI_type::RadiusType             radius;
  typename BFC_type::FaceListType::iterator fit;

  NeighborhoodInnerProduct<TImage> SIP;
  NeighborhoodInnerProduct<TImage>      IP;
  RNI_type                              iterator_list[ImageDimension];
  SNI_type                              face_iterator_list[ImageDimension];
  DerivativeOperator<PixelType,
                        ImageDimension> operator_list[ImageDimension];
  
  // Set up the derivative operators, one for each dimension
  for (i = 0; i < ImageDimension; ++i)
    {
    operator_list[i].SetOrder(1);
    operator_list[i].SetDirection(i);
    operator_list[i].CreateDirectional();
    radius[i] = operator_list[i].GetRadius()[i];
    }

  // Get the various region "faces" that are on the data set boundary.
  faceList = bfc(ip, ip->GetRequestedRegion(), radius);
  fit      = faceList.begin();

  // Now do the actual processing
  accumulator = NumericTraits<PixelType>::Zero;
  counter     = NumericTraits<PixelType>::Zero;

  // First process the non-boundary region

  // Instead of maintaining a single N-d neighborhood of pointers,
  // we maintain a list of 1-d neighborhoods along each axial direction.
  // This is more efficient for higher dimensions.
  for (i = 0; i < ImageDimension; ++i)
    {
    iterator_list[i]=RNI_type(operator_list[i].GetRadius(), ip, *fit); 
    iterator_list[i].GoToBegin();
    }  
  while ( !iterator_list[0].IsAtEnd() )
    {
    counter += NumericTraits<PixelType>::One;
    for (i = 0; i < ImageDimension; ++i)
      {
      val = IP(iterator_list[i], operator_list[i]);     
      accumulator += val * val;
      ++iterator_list[i];
      }
    }
  
  // Go on to the next region(s).  These are on the boundary faces.
  ++fit; 
  while ( fit != faceList.end() )
    {
    for (i = 0; i < ImageDimension; ++i)
      {
      face_iterator_list[i]=SNI_type(operator_list[i].GetRadius(), ip,
                                     *fit);
      face_iterator_list[i].OverrideBoundaryCondition(&bc);
      face_iterator_list[i].GoToBegin();
      }
        
    while ( ! face_iterator_list[0].IsAtEnd() )
      {
      counter += NumericTraits<PixelType>::One;
      for (i = 0; i < ImageDimension; ++i)
        {
        val = SIP(face_iterator_list[i], operator_list[i]);     
        accumulator += val * val;
        ++face_iterator_list[i];
        }
      }
    ++fit;
    }
  
  this->SetAverageGradientMagnitudeSquared( (double) (accumulator / counter) );

}

}// end namespace itk


#endif
