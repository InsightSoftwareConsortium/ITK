/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMahalanobisDistanceThresholdImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMahalanobisDistanceThresholdImageFunction_txx
#define _itkMahalanobisDistanceThresholdImageFunction_txx

#include "itkMahalanobisDistanceThresholdImageFunction.h"

namespace itk
{

template <class TInputImage, class TCoordRep>
MahalanobisDistanceThresholdImageFunction<TInputImage,TCoordRep>
::MahalanobisDistanceThresholdImageFunction()
{
  m_Threshold = NumericTraits<double>::Zero;
  m_MahalanobisDistanceMembershipFunction = 
          MahalanobisDistanceFunctionType::New();
}


template <class TInputImage, class TCoordRep>
void
MahalanobisDistanceThresholdImageFunction<TInputImage,TCoordRep>
::SetMean(const vnl_vector< double > & mean)
{
  m_MahalanobisDistanceMembershipFunction->SetMean( mean );
}



template <class TInputImage, class TCoordRep>
void
MahalanobisDistanceThresholdImageFunction<TInputImage,TCoordRep>
::SetCovariance(const vnl_matrix< double > & covariance)
{
  m_MahalanobisDistanceMembershipFunction->SetCovariance( covariance );
}



template <class TInputImage, class TCoordRep>
void 
MahalanobisDistanceThresholdImageFunction<TInputImage,TCoordRep>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "MahalanobisDistanceMembershipFunction: " << m_MahalanobisDistanceMembershipFunction << std::endl;
}

} // end namespace itk

#endif
