/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureFlowImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCurvatureFlowImageFilter_txx_
#define __itkCurvatureFlowImageFilter_txx_

#include "itkCurvatureFlowImageFilter.h"
#include "itkExceptionObject.h"

namespace itk
{

/*
 * Constructor
 */
template <class TInputImage, class TOutputImage>
CurvatureFlowImageFilter<TInputImage, TOutputImage>
::CurvatureFlowImageFilter()
{

  m_NumberOfIterations = 0;
  m_TimeStep   = 0.05f;

  typename CurvatureFlowFunctionType::Pointer cffp;
  cffp = CurvatureFlowFunctionType::New();

  this->SetDifferenceFunction( static_cast<FiniteDifferenceFunctionType *>( 
    cffp.GetPointer() ) );

}


/*
 * Standard PrintSelf method.
 */
template <class TInputImage, class TOutputImage>
void
CurvatureFlowImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number of Iterations: " << m_NumberOfIterations;
  os << std::endl;
  os << indent << "Time step: " << m_TimeStep;
  os << std::endl;
}


/*
 * Initialize the state of filter and equation before each iteration.
 */
template <class TInputImage, class TOutputImage>
void
CurvatureFlowImageFilter<TInputImage, TOutputImage>
::InitializeIteration()
{

  // update variables in the equation object
  CurvatureFlowFunctionType *f = 
    dynamic_cast<CurvatureFlowFunctionType *>
    (this->GetDifferenceFunction().GetPointer());

  if ( !f )
    {
    itkExceptionMacro(<<"DifferenceFunction not of type CurvatureFlowFunction");
    }

  f->SetTimeStep( m_TimeStep );

  // call superclass's version
  this->Superclass::InitializeIteration();           

  // progress feedback
  if ( m_NumberOfIterations != 0 )
    {
    this->UpdateProgress(((float)(this->GetElapsedIterations()))
                         /((float)(m_NumberOfIterations)));
    }
  else 
    {
    this->UpdateProgress(0);
    }
  
}


/*
 * GenerateInputRequestedRegion
 */
template <class TInputImage, class TOutputImage>
void
CurvatureFlowImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer  inputPtr  = 
    const_cast< InputImageType * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // set the input requested region to be the same as
  // the output requested region
  inputPtr->SetRequestedRegion(
    outputPtr->GetRequestedRegion() );

}


/*
 * EnlargeOutputRequestedRegion
 */
template <class TInputImage, class TOutputImage>
void
CurvatureFlowImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(
DataObject * ptr )
{

  // convert DataObject pointer to OutputImageType pointer 
  OutputImageType * outputPtr;
  outputPtr = dynamic_cast<OutputImageType*>( ptr );

  // get input image pointer
  typename Superclass::InputImagePointer  inputPtr  = 
    const_cast< InputImageType * >( this->GetInput() );
  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // Get the size of the neighborhood on which we are going to operate.  This
  // radius is supplied by the difference function we are using.
  typename FiniteDifferenceFunctionType::RadiusType radius
    = this->GetDifferenceFunction()->GetRadius();

  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    radius[j] *= m_NumberOfIterations;
    }

  /*
   * NewOutputRequestedRegion = OldOutputRequestedRegion +
   * radius * m_NumberOfIterations padding on each edge
   */
  typename OutputImageType::RegionType outputRequestedRegion =
    outputPtr->GetRequestedRegion();

  outputRequestedRegion.PadByRadius( radius );
  outputRequestedRegion.Crop( outputPtr->GetLargestPossibleRegion() );
  
  outputPtr->SetRequestedRegion( outputRequestedRegion );

  
}


} // end namespace itk

#endif
