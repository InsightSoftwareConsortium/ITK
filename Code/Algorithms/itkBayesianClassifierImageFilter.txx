/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBayesianClassifierImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBayesianClassifierImageFilter_txx
#define _itkBayesianClassifierImageFilter_txx

#include "itkBayesianClassifierImageFilter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
BayesianClassifierImageFilter<TInputImage, TOutputImage>
::BayesianClassifierImageFilter()
{

}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
BayesianClassifierImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
BayesianClassifierImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  
}


} // end namespace itk

#endif
