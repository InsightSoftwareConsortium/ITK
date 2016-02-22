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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkChangeRegionLabelMapFilter_hxx
#define itkChangeRegionLabelMapFilter_hxx
#include "itkChangeRegionLabelMapFilter.h"
#include "itkProgressReporter.h"
/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Label object representation and manipulation with ITK"
 * by Lehmann G.
 * https://hdl.handle.net/1926/584
 * http://www.insight-journal.org/browse/publication/176
 *
 */

namespace itk
{
template< typename TInputImage >
void
ChangeRegionLabelMapFilter< TInputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( !input )
        { return; }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}

template< typename TInputImage >
void
ChangeRegionLabelMapFilter< TInputImage >
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();
  this->GetOutput()->SetLargestPossibleRegion(m_Region);
}

template< typename TInputImage >
void
ChangeRegionLabelMapFilter< TInputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage >
void
ChangeRegionLabelMapFilter< TInputImage >
::GenerateData()
{
  if ( m_Region.IsInside( this->GetInput()->GetLargestPossibleRegion() ) )
    {
    // only copy the image, report progress anyway
    ProgressReporter progress(this, 0, 1);
    this->AllocateOutputs();
    }
  else
    {
    // call the superclass implementation so it will take care to create the
    // threads
    Superclass::GenerateData();
    }
}

template< typename TInputImage >
void
ChangeRegionLabelMapFilter< TInputImage >
::ThreadedProcessLabelObject(LabelObjectType *labelObject)
{
  typename LabelObjectType::Pointer tmp = LabelObjectType::New();
  tmp->template CopyAllFrom<LabelObjectType>( labelObject );
  labelObject->Clear();

  const IndexType idxMin = m_Region.GetIndex();
  IndexType       idxMax;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    idxMax[i] = idxMin[i] + m_Region.GetSize()[i] - 1;
    }

  typename LabelObjectType::ConstLineIterator lit( tmp );
  while( ! lit.IsAtEnd() )
    {
    const IndexType      idx = lit.GetLine().GetIndex();
    const IndexValueType length = lit.GetLine().GetLength();

    bool outside = false;
    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      if ( idx[i] < idxMin[i] || idx[i] > idxMax[i] )
        {
        outside = true;
        }
      }
    // check the axis 0
    if ( !outside )
      {
      const IndexValueType lastIdx0 = idx[0] + length - 1;
      if ( !( ( idx[0] < idxMin[0] && lastIdx0 < idxMin[0] )
              || ( idx[0] > idxMax[0] && lastIdx0 > idxMax[0] ) ) )
        {
        IndexType      newIdx = idx;
        IndexValueType newLength = length;
        if ( idx[0] < idxMin[0] )
          {
          newLength -= idxMin[0] - idx[0];
          newIdx[0] = idxMin[0];
          }
        if ( lastIdx0 > idxMax[0] )
          {
          newLength -= lastIdx0 - idxMax[0];
          }
        labelObject->AddLine(newIdx, newLength);
        }
      }
    ++lit;
    }

  // remove the object if it is empty
  if ( labelObject->Empty() )
    {
    this->m_LabelObjectContainerLock->Lock();
    this->GetOutput()->RemoveLabelObject(labelObject);
    this->m_LabelObjectContainerLock->Unlock();
    }
}

template< typename TInputImage >
void
ChangeRegionLabelMapFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Region: " << m_Region << std::endl;
}
} // end namespace itk

#endif
