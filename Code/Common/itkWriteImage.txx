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

ITK_NAMESPACE_BEGIN

/**
 *
 */
template <class TInputImage>
void 
WriteImage<TInputImage>
::SetInput(TInputImage *input)
{
  this->ProcessObject::SetNthInput(0, input);
}


template <class TInputImage>
TInputImage *
WriteImage<TInputImage>
::GetInput()
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage *>(this->ProcessObject::GetInput(0));
}


/**
 *
 */
template <class TInputImage>
void 
WriteImage<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Writer::PrintSelf(os,indent);
  
}

ITK_NAMESPACE_END
