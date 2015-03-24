/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkAggregateLabelMapFilter_hxx
#define itkAggregateLabelMapFilter_hxx

#include "itkAggregateLabelMapFilter.h"
#include "itkProgressReporter.h"

namespace itk
{
template< typename TImage >
void
AggregateLabelMapFilter< TImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType *output = this->GetOutput();

  ProgressReporter progress( this, 0, output->GetNumberOfLabelObjects() );

  typename TImage::Iterator it( output );
  if ( ! it.IsAtEnd() )
    {
    LabelObjectType *mainLo = it.GetLabelObject();
    progress.CompletedPixel();
    ++it;
    while ( ! it.IsAtEnd() )
      {
      LabelObjectType *lo = it.GetLabelObject();
      typename LabelObjectType::ConstLineIterator lit( lo );
      while( ! lit.IsAtEnd() )
        {
        mainLo->AddLine( lit.GetLine() );
        ++lit;
        }
      // be sure to have the lines well organized
      mainLo->Optimize();

      progress.CompletedPixel();
      ++it;
      // must increment the iterator before removing the object to avoid
      // invalidating the iterator
      output->RemoveLabelObject(lo);
      }
    }
}

template< typename TImage >
void
AggregateLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // end namespace itk
#endif
