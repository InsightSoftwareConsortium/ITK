/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriteImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkWriteImage.h"

//----------------------------------------------------------------------------
template <class TInputImage>
void 
itkWriteImage<TInputImage>
::SetInput(TInputImage *input)
{
  this->itkProcessObject::SetNthInput(0, input);
}

//----------------------------------------------------------------------------
template <class TInputImage>
TInputImage *
itkWriteImage<TInputImage>
::GetInput()
{
  if (this->NumberOfInputs < 1)
    {
    return 0;
    }
  
  return (TInputImage *)(this->GetInput(0));
}

//----------------------------------------------------------------------------
template <class TInputImage>
void 
itkWriteImage<TInputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkWriter::PrintSelf(os,indent);
  
}


