/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChangeLabelImageFilter.txx
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
#ifndef _itkChangeLabelImageFilter_txx
#define _itkChangeLabelImageFilter_txx

#include "itkChangeLabelImageFilter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ChangeLabelImageFilter<TInputImage, TOutputImage>
::ChangeLabelImageFilter()
{
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void
ChangeLabelImageFilter<TInputImage, TOutputImage>
::SetChange( const InputPixelType & original, const OutputPixelType & result )
{
   OutputPixelType  current = this->GetFunctor().GetChange(original); 
   if ( current != result )
     {
     this->GetFunctor().SetChange(original, result);
     this->Modified();
     }  
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void
ChangeLabelImageFilter<TInputImage, TOutputImage>
::SetChangeMap( const ChangeMapType & changeMap )
{
   //If the whole map is being set then we assume that a real change is made
   this->GetFunctor().SetChangeMap(changeMap); 
   this->Modified();
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ChangeLabelImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  //Maybe should iterate the change map and print it here
}

} // end namespace itk

#endif
