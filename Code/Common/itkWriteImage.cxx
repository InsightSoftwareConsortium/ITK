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
#include "itkImageWriter.h"

//----------------------------------------------------------------------------
template <class TInputImage>
void itkImageWriter<TInputImage>
::SetInput(TInputImage *input)
{
  this->itkProcessObject::SetNthInput(0, input);
}

//----------------------------------------------------------------------------
template <class TInputImage>
TInputImage *itkImageWriter<TInputImage>
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
void itkImageWriter<TInputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkWriter::PrintSelf(os,indent);
  
}


