/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRelabelLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRelabelLabelMapFilter_txx
#define __itkRelabelLabelMapFilter_txx

#include "itkRelabelLabelMapFilter.h"
#include "itkProgressReporter.h"

namespace itk
{
template< class TImage >
void
RelabelLabelMapFilter< TImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType *output = this->GetOutput();

  // get the label objects
  LabelObjectContainerType labelObjects = output->GetLabelObjectContainer();

  ProgressReporter progress( this, 0, labelObjects.size() );

  // and put back the objects in the map
  output->ClearLabels();
  typename LabelObjectContainerType::iterator it = labelObjects.begin();
  while ( it != labelObjects.end() )
    {
    output->PushLabelObject(it->second);

    // go to the next label
    progress.CompletedPixel();
    it++;
    }
}

template< class TImage >
void
RelabelLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // end namespace itk
#endif
