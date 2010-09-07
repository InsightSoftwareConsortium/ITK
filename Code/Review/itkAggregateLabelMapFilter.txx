/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAggregateLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAggregateLabelMapFilter_txx
#define __itkAggregateLabelMapFilter_txx

#include "itkAggregateLabelMapFilter.h"
#include "itkProgressReporter.h"

namespace itk
{
template< class TImage >
void
AggregateLabelMapFilter< TImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType *output = this->GetOutput();

  LabelObjectContainerType & labelObjectContainer = output->GetLabelObjectContainer();

  ProgressReporter progress( this, 0, labelObjectContainer.size() );

  typename LabelObjectContainerType::iterator it = labelObjectContainer.begin();
  if ( it != labelObjectContainer.end() )
    {
    LabelObjectType *mainLo = it->second;
    progress.CompletedPixel();
    it++;
    while ( it != labelObjectContainer.end() )
      {
      LabelObjectType *lo = it->second;
      typename LabelObjectType::LineContainerType::const_iterator lit;
      typename LabelObjectType::LineContainerType & lineContainer = lo->GetLineContainer();

      for ( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
        {
        mainLo->AddLine(*lit);
        }
      // be sure to have the lines well organized
      mainLo->Optimize();

      progress.CompletedPixel();
      it++;
      // must increment the iterator before removing the object to avoid
      // invalidating the iterator
      output->RemoveLabelObject(lo);
      }
    }
}

template< class TImage >
void
AggregateLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // end namespace itk
#endif
