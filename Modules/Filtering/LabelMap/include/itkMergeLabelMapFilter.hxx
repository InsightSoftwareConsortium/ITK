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
#ifndef itkMergeLabelMapFilter_hxx
#define itkMergeLabelMapFilter_hxx

#include "itkMergeLabelMapFilter.h"
#include "itkProgressReporter.h"
#include <deque>

namespace itk
{
template< typename TImage >
MergeLabelMapFilter< TImage >
::MergeLabelMapFilter()
{
  this->m_Method = KEEP;
}

template< typename TImage >
void
MergeLabelMapFilter< TImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  switch ( this->m_Method )
    {
    case KEEP:
      {
      this->MergeWithKeep();
      break;
      }
    case AGGREGATE:
      {
      this->MergeWithAggregate();
      break;
      }
    case PACK:
      {
      this->MergeWithPack();
      break;
      }
    case STRICT:
      {
      this->MergeWithStrict();
      break;
      }
    default:
      {
      itkExceptionMacro(<< "No such method: " << this->m_Method);
      }
    }
}

template< typename TImage >
void
MergeLabelMapFilter< TImage >
::MergeWithKeep()
{
  ImageType *output = this->GetOutput();

  typedef std::deque< LabelObjectPointer > VectorType;
  VectorType labelObjects;

  ProgressReporter progress(this, 0, 1);

  for ( unsigned int i = 1; i < this->GetNumberOfIndexedInputs(); i++ )
    {
    typename ImageType::ConstIterator it2( this->GetInput(i) );
    while ( ! it2.IsAtEnd() )
      {
      const LabelObjectType *lo = it2.GetLabelObject();
      LabelObjectPointer     newLo = LabelObjectType::New();
      newLo->template CopyAllFrom<LabelObjectType>(lo);

      if( ( output->GetBackgroundValue() != newLo->GetLabel() ) &&
          ( !output->HasLabel( newLo->GetLabel() ) ) )
        {
        // we can keep the label
        output->AddLabelObject(newLo);
        }
      else
        {
        // store the label object to read it later with another label
        labelObjects.push_back(newLo);
        }

      // go to the next label
      progress.CompletedPixel();
      ++it2;
      }

    // add the other label objects, with a different label
    typename VectorType::iterator it = labelObjects.begin();
    while ( it != labelObjects.end() )
      {
      output->PushLabelObject(*it);
      ++it;
      }
    }
}

template< typename TImage >
void
MergeLabelMapFilter< TImage >
::MergeWithStrict()
{
  ImageType *output = this->GetOutput();

  ProgressReporter progress(this, 0, 1);

  for ( unsigned int i = 1; i < this->GetNumberOfIndexedInputs(); i++ )
    {
    typename ImageType::ConstIterator it2( this->GetInput(i) );
    while ( ! it2.IsAtEnd() )
      {
      const LabelObjectType *lo = it2.GetLabelObject();
      LabelObjectPointer     newLo = LabelObjectType::New();
      newLo->template CopyAllFrom<LabelObjectType>(lo);

      if ( output->GetBackgroundValue() != newLo->GetLabel() )
        {
        if ( !output->HasLabel( newLo->GetLabel() ) )
          {
          // we can keep the label
          output->AddLabelObject(newLo);
          }
        else
          {
          itkExceptionMacro(<< "Label "
                            << static_cast< typename itk::NumericTraits< PixelType >::PrintType >( newLo->GetLabel() )
                            << " from input " << i
                            << " is already in use.");
          }
        }
      else
        {
        itkGenericExceptionMacro(<<"Label "
                            << static_cast< typename itk::NumericTraits< PixelType >::PrintType >( newLo->GetLabel() )
                            << " from input " << i
                            << " is output background value.");
        }

      // go to the next label
      progress.CompletedPixel();
      ++it2;
      }
    }
}

template< typename TImage >
void
MergeLabelMapFilter< TImage >
::MergeWithAggregate()
{
  ImageType *output = this->GetOutput();

  ProgressReporter progress(this, 0, 1);

  for ( unsigned int i = 1; i < this->GetNumberOfIndexedInputs(); i++ )
    {
    typename ImageType::ConstIterator it2( this->GetInput(i) );
    while ( ! it2.IsAtEnd() )
      {
      const LabelObjectType *lo = it2.GetLabelObject();

      bool hasLabel = output->HasLabel( lo->GetLabel() );
      if ( !hasLabel && ( lo->GetLabel() != output->GetBackgroundValue() ) )
        {
        // we can keep the label
        LabelObjectPointer newLo = LabelObjectType::New();
        newLo->template CopyAllFrom<LabelObjectType>(lo);
        output->AddLabelObject(newLo);
        }
      else
        {
        if ( hasLabel )
          {
          // add the lines of that object to the one already in the output
          LabelObjectType *         mainLo = output->GetLabelObject( lo->GetLabel() );
          typename LabelObjectType::ConstLineIterator lit( lo );
          while ( ! lit.IsAtEnd() )
            {
            mainLo->AddLine( lit.GetLine() );
            ++lit;
            }

          // be sure to have the lines well organized
          mainLo->Optimize();
          }
        }

      // go to the next label
      progress.CompletedPixel();
      ++it2;
      }
    }
}

template< typename TImage >
void
MergeLabelMapFilter< TImage >
::MergeWithPack()
{
  ProgressReporter progress(this, 0, 1);

  ImageType *output = this->GetOutput();

  // get the label objects of the first input
  typename ImageType::LabelObjectVectorType labelObjects = output->GetLabelObjects();

  // and put back the objects in the map
  output->ClearLabels();

  typename ImageType::LabelObjectVectorType::iterator it = labelObjects.begin();

  while ( it != labelObjects.end() )
    {
    output->PushLabelObject(*it);

    // go to the next label
    progress.CompletedPixel();
    it++;
    }

  // now, the next images
  for ( unsigned int i = 1; i < this->GetNumberOfIndexedInputs(); i++ )
    {
    typename ImageType::ConstIterator it2( this->GetInput(i) );
    while ( ! it2.IsAtEnd() )
      {
      const LabelObjectType *lo = it2.GetLabelObject();
      LabelObjectPointer     newLo = LabelObjectType::New();
      newLo->template CopyAllFrom<LabelObjectType>(lo);
      output->PushLabelObject(newLo);

      // go to the next label
      progress.CompletedPixel();
      ++it2;
      }
    }
}

template< typename TImage >
void
MergeLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Method: "  << this->m_Method << std::endl;
}
} // end namespace itk
#endif
