/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricalDemonsRegistrationFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSymmetricalDemonsRegistrationFilter_txx
#define _itkSymmetricalDemonsRegistrationFilter_txx
#include "itkSymmetricalDemonsRegistrationFilter.h"

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
SymmetricalDemonsRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::SymmetricalDemonsRegistrationFilter()
{
 
  typename DemonsRegistrationFunctionType::Pointer drfp;
  drfp = DemonsRegistrationFunctionType::New();

  this->SetDifferenceFunction( static_cast<FiniteDifferenceFunctionType *>(
                                 drfp.GetPointer() ) );

}


/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
SymmetricalDemonsRegistrationFilter<TFixedImage,TMovingImage,TDeformationField>
::InitializeIteration()
{


  // update variables in the equation object
  DemonsRegistrationFunctionType *f = 
    dynamic_cast<DemonsRegistrationFunctionType *>
    (this->GetDifferenceFunction().GetPointer());

  if ( !f )
    {
    itkExceptionMacro(<<"FiniteDifferenceFunction not of type DemonsRegistrationFunctionType");
    }

  f->SetDeformationField( this->GetDeformationField() );


 
  // call the superclass  implementation
  Superclass::InitializeIteration();

  /*
   * Smooth the deformation field
   */
  this->SmoothDeformationField();

}


} // end namespace itk

#endif
