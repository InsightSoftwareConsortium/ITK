/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageWriter_txx
#define _itkImageWriter_txx

#include "itkImageWriter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage>
void 
ImageWriter<TInputImage>
::SetInput(const InputImageType *input)
{
  this->ProcessObject::SetNthInput(0, 
                                   const_cast<InputImageType *>(input) );
}


template <class TInputImage>
const typename ImageWriter<TInputImage>::InputImageType * 
ImageWriter<TInputImage>
::GetInput(void)
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage*>
    (this->ProcessObject::GetInput(0));
}


/**
 *
 */
template <class TInputImage>
void 
ImageWriter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
}

} // end namespace itk

#endif
