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
#ifndef itkChangeLabelLabelMapFilter_hxx
#define itkChangeLabelLabelMapFilter_hxx

#include "itkChangeLabelLabelMapFilter.h"
#include "itkProgressReporter.h"
#include <deque>

namespace itk
{
template< typename TImage >
ChangeLabelLabelMapFilter< TImage >
::ChangeLabelLabelMapFilter()
{}

template< typename TImage >
void
ChangeLabelLabelMapFilter< TImage >
::SetChangeMap(const ChangeMapType & changeMap)
{
  if ( m_MapOfLabelToBeReplaced != changeMap )
    {
    m_MapOfLabelToBeReplaced = changeMap;
    this->Modified();
    }
}

template< typename TImage >
const typename ChangeLabelLabelMapFilter< TImage >::ChangeMapType &
ChangeLabelLabelMapFilter< TImage >
::GetChangeMap() const
{
  return m_MapOfLabelToBeReplaced;
}

template< typename TImage >
void
ChangeLabelLabelMapFilter< TImage >
::SetChange(const PixelType & oldLabel, const PixelType & newLabel)
{
  if ( m_MapOfLabelToBeReplaced.find(oldLabel) == m_MapOfLabelToBeReplaced.end()
       || m_MapOfLabelToBeReplaced[oldLabel] != newLabel )
    {
    m_MapOfLabelToBeReplaced[oldLabel] = newLabel;
    this->Modified();
    }
}

template< typename TImage >
void
ChangeLabelLabelMapFilter< TImage >
::ClearChangeMap()
{
  if ( !m_MapOfLabelToBeReplaced.empty() )
    {
    m_MapOfLabelToBeReplaced.clear();
    this->Modified();
    }
}

template< typename TImage >
void
ChangeLabelLabelMapFilter< TImage >
::GenerateData()
{
  // MoveLabelsToTemporaryArray

  // Allocate the output
  this->AllocateOutputs();

  ImageType *output = this->GetOutput();

  // Report the progress
  ProgressReporter progress(this, 0, static_cast<SizeValueType>( m_MapOfLabelToBeReplaced.size() ) * 2);

  // First remove the ones to change and store them elsewhere to process later
  typedef typename LabelObjectType::Pointer LabelObjectPointer;
  typedef std::deque< LabelObjectPointer >  VectorType;

  VectorType labelObjectsToBeRelabeled;

  ChangeMapIterator pairToReplace = m_MapOfLabelToBeReplaced.begin();

  while ( pairToReplace != m_MapOfLabelToBeReplaced.end() )
    {
    const PixelType labelToBeReplaced = pairToReplace->first;

    if ( labelToBeReplaced != output->GetBackgroundValue() )
      {
      if ( output->HasLabel(labelToBeReplaced) )
        {
        labelObjectsToBeRelabeled.push_back( output->GetLabelObject(labelToBeReplaced) );
        output->RemoveLabel(labelToBeReplaced);
        }
      }

    progress.CompletedPixel();
    pairToReplace++;
    }

  // ChangeBackgroundIfNeeded

  // Check if the background is among the list of labels to relabel.
  ChangeMapIterator backgroundLabelItr = m_MapOfLabelToBeReplaced.find( output->GetBackgroundValue() );
  const bool        backgroundLabelMustBeReplaced = ( backgroundLabelItr != m_MapOfLabelToBeReplaced.end() );

  // Then change the label of the background if needed
  if ( backgroundLabelMustBeReplaced )
    {
    const PixelType newLabelForBackground = m_MapOfLabelToBeReplaced[output->GetBackgroundValue()];

    if ( newLabelForBackground !=  output->GetBackgroundValue() )
      {
      if ( output->HasLabel(newLabelForBackground) )
        {
        // we must have a background - remove that object
        output->RemoveLabel(newLabelForBackground);
        }
      output->SetBackgroundValue(newLabelForBackground);
      }
    }

  // RestoreLabelObjectsAndChangeLabels

  // Put the objects back in the map, with the updated label
  typedef typename VectorType::iterator LabelObjectIterator;
  LabelObjectIterator labelObjectItr = labelObjectsToBeRelabeled.begin();

  while ( labelObjectItr != labelObjectsToBeRelabeled.end() )
    {
    LabelObjectType *labelObjectSource = *labelObjectItr;
    PixelType        newLabel = m_MapOfLabelToBeReplaced[labelObjectSource->GetLabel()];

    // Ignore the label if it is the background
    if ( newLabel != output->GetBackgroundValue() )
      {
      // If the new label already exists in the output, then merge them.
      if ( output->HasLabel(newLabel) )
        {
        // Add the content of the label object to the one already there
        LabelObjectType *labelObjectDestination = output->GetLabelObject(newLabel);

        typename LabelObjectType::ConstLineIterator lit( labelObjectSource );
        while ( ! lit.IsAtEnd() )
          {
          labelObjectDestination->AddLine( lit.GetLine() );
          ++lit;
          }

        // be sure to have the lines well organized
        labelObjectDestination->Optimize();
        }
      else
        {
        // just put the label object in the label map with the new label
        labelObjectSource->SetLabel(newLabel);
        output->AddLabelObject(labelObjectSource);
        }
      }

    // go to the next label object
    progress.CompletedPixel();
    labelObjectItr++;
    }
}

template< typename TImage >
void
ChangeLabelLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  ChangeMapIterator pairToReplace = m_MapOfLabelToBeReplaced.begin();

  typedef typename NumericTraits< PixelType >::PrintType LabelPrintType;

  os << indent << "Labels to replace: " << std::endl;
  while ( pairToReplace != m_MapOfLabelToBeReplaced.end() )
    {
    const PixelType oldLabel = pairToReplace->first;
    const PixelType newLabel = pairToReplace->second;

    os << indent;
    os << static_cast< LabelPrintType >( oldLabel ) << " -> ";
    os << static_cast< LabelPrintType >( newLabel ) << std::endl;

    ++pairToReplace;
    }
}
} // end namespace itk
#endif
