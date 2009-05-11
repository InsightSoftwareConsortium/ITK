/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarChanAndVeseLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScalarChanAndVeseLevelSetFunction_txx
#define __itkScalarChanAndVeseLevelSetFunction_txx

#include "itkScalarChanAndVeseLevelSetFunction.h"

namespace itk {

/* Calculates the numerator and denominator for c_i for each region. As part of
the optimization, it is called once at the beginning of the code, and then the
cNum and cDen are updated during the evolution without iterating through the
entire image. */
template < class TInputImage, class TFeatureImage, class TSharedData >
void
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ComputeParameters()
{
  Superclass::ComputeParameters();
}


template < class TInputImage, class TFeatureImage, class TSharedData >
typename ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ScalarValueType
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >::
computeInternalTerm( const FeaturePixelType& iValue,
  const FeatureIndexType& itkNotUsed(iIdx), const unsigned int& fId )
{
  const ScalarValueType cVals = this->m_SharedData->m_ForegroundConstantValues[fId];
  const ScalarValueType t = ( iValue - cVals );
  return t * t;
}


template < class TInputImage, class TFeatureImage, class TSharedData >
typename ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ScalarValueType
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::computeExternalTerm( const FeaturePixelType& iValue,
    const FeatureIndexType& itkNotUsed(iIdx), const unsigned int& pr )
{
  const unsigned int fId = this->m_FunctionId;
  const double cBgrnd = this->m_SharedData->m_BackgroundConstantValues[fId]; // background

  return pr * ( iValue - cBgrnd ) * ( iValue - cBgrnd );
}


/* Performs the narrow-band update of the Heaviside function for each voxel. The
characteristic function of each region is recomputed (note the shared
data which contains information from the other level sets). Using the
new H values, the previous c_i are updated. */
template < class TInputImage, class TFeatureImage, class TSharedData >
void
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::UpdatePixel( const unsigned int& idx, NeighborhoodIterator< TInputImage >
&iterator, ScalarValueType &newValue, bool &status )
{
  this->Superclass::UpdatePixel(idx,iterator,newValue,status);
}


} // end namespace itk
#endif
