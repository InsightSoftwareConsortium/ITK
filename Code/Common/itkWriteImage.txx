/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriteImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkWriteImage.h"

namespace itk
{

/**
 *
 */
template <class TInputImage>
void 
WriteImage<TInputImage>
::SetInput(InputImage *input)
{
  this->ProcessObject::SetNthInput(0, input);
}

template <class TInputImage>
WriteImage<TInputImage>::InputImagePointer 
WriteImage<TInputImage>
::GetInput()
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(0).GetPointer());
}


/**
 *
 */
template <class TInputImage>
void 
WriteImage<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
  
}

} // end namespace itk
