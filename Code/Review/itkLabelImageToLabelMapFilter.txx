/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelImageToLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelImageToLabelMapFilter_txx
#define __itkLabelImageToLabelMapFilter_txx

#include "itkLabelImageToLabelMapFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageLinearConstIteratorWithIndex.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
LabelImageToLabelMapFilter< TInputImage, TOutputImage >
::LabelImageToLabelMapFilter()
{
  m_BackgroundValue = NumericTraits< OutputImagePixelType >::NonpositiveMin();
}

template< class TInputImage, class TOutputImage >
void
LabelImageToLabelMapFilter< TInputImage, TOutputImage >
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

template< class TInputImage, class TOutputImage >
void
LabelImageToLabelMapFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< class TInputImage, class TOutputImage >
void
LabelImageToLabelMapFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  // init the temp images - one per thread
  m_TemporaryImages.resize( this->GetNumberOfThreads() );

  for ( int i = 0; i < this->GetNumberOfThreads(); i++ )
    {
    if ( i == 0 )
      {
      // the first one is the output image
      m_TemporaryImages[0] = this->GetOutput();
      }
    else
      {
      // the other must be created
      m_TemporaryImages[i] = OutputImageType::New();
      }

    // set the minimum data needed to create the objects properly
    m_TemporaryImages[i]->SetBackgroundValue(m_BackgroundValue);
    }
}

template< class TInputImage, class TOutputImage >
void
LabelImageToLabelMapFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & regionForThread, int threadId)
{
  ProgressReporter progress( this, threadId, regionForThread.GetNumberOfPixels() );

  typedef ImageLinearConstIteratorWithIndex< InputImageType > InputLineIteratorType;
  InputLineIteratorType it(this->GetInput(), regionForThread);
  it.SetDirection(0);

  for ( it.GoToBegin(); !it.IsAtEnd(); it.NextLine() )
    {
    it.GoToBeginOfLine();

    while ( !it.IsAtEndOfLine() )
      {
      const InputImagePixelType & v = it.Get();

      if ( v != m_BackgroundValue )
        {
        // We've hit the start of a run
        IndexType idx = it.GetIndex();
        long      length = 1;
        ++it;
        while ( !it.IsAtEndOfLine() && it.Get() == v )
          {
          ++length;
          ++it;
          }
        // create the run length object to go in the vector
        m_TemporaryImages[threadId]->SetLine(idx, length, v);
        }
      else
        {
        // go the the next pixel
        ++it;
        }
      }
    }
}

template< class TInputImage, class TOutputImage >
void
LabelImageToLabelMapFilter< TInputImage, TOutputImage >
::AfterThreadedGenerateData()
{
  OutputImageType *output = this->GetOutput();

  // merge the lines from the temporary images in the output image
  // don't use the first image - that's the output image
  for ( int i = 1; i < this->GetNumberOfThreads(); i++ )
    {
    typedef typename OutputImageType::LabelObjectContainerType LabelObjectContainerType;
    const LabelObjectContainerType & labelObjectContainer = m_TemporaryImages[i]->GetLabelObjectContainer();

    for ( typename LabelObjectContainerType::const_iterator it = labelObjectContainer.begin();
          it != labelObjectContainer.end();
          it++ )
      {
      LabelObjectType *labelObject = it->second;
      if ( output->HasLabel( labelObject->GetLabel() ) )
        {
        // merge the lines in the output's object
        typename LabelObjectType::LineContainerType & src = labelObject->GetLineContainer();
        typename LabelObjectType::LineContainerType & dest =
          output->GetLabelObject( labelObject->GetLabel() )->GetLineContainer();
        dest.insert( dest.end(), src.begin(), src.end() );
        }
      else
        {
        // simply take the object
        output->AddLabelObject(labelObject);
        }
      }
    }

  // release the data in the temp images
  m_TemporaryImages.clear();
}

template< class TInputImage, class TOutputImage >
void
LabelImageToLabelMapFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_BackgroundValue ) << std::endl;
}
} // end namespace itk
#endif
