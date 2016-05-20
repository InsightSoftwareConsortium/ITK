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
#ifndef itkHardConnectedComponentImageFilter_hxx
#define itkHardConnectedComponentImageFilter_hxx
#include "itkHardConnectedComponentImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
void
HardConnectedComponentImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  unsigned int i;
  int          p;
  int          q;
  int          m;

  typedef unsigned short LabelType;

  LabelType * equivalenceTable = new LabelType[NumericTraits < LabelType > ::max()];
  LabelType label = 0;
  LabelType maxLabel = 0;
  IndexType index;
  SizeType  size;

  typename ListType::iterator iter;
  RegionType region;

  TOutputImage * output = this->GetOutput();
  const TInputImage * input = this->GetInput();

  size = input->GetLargestPossibleRegion().GetSize();
  index.Fill(0);
  region.SetSize(size);
  region.SetIndex(index);
  output->SetLargestPossibleRegion(region);
  output->SetBufferedRegion(region);
  output->SetRequestedRegion(region);
  output->Allocate();

  ImageRegionConstIterator< TInputImage > it( input, input->GetRequestedRegion() );
  ImageRegionIterator< TOutputImage >     ot( output, output->GetRequestedRegion() );

  ProgressReporter progress( this, 0, output->GetRequestedRegion().GetNumberOfPixels() );
  it.GoToBegin();
  ot.GoToBegin();
  for (; !it.IsAtEnd(); ++it, ++ot )
    {
    if ( Math::NotExactlyEquals(it.Get(), NumericTraits< typename ImageRegionConstIterator< TInputImage >::PixelType >::ZeroValue()) )
      {
      ot.Set( NumericTraits< typename TOutputImage::PixelType >::max() );
      }
    else
      {
      ot.Set( NumericTraits< typename TOutputImage::PixelType >::ZeroValue() );
      }
    }
  equivalenceTable[0] = 0;
  ot.GoToBegin();
  for (; !ot.IsAtEnd(); ++ot )
    {
    if ( ot.Get() )
      {
      for ( i = 0; i < ImageDimension; i++ )
        {
        IndexType currentIndex = ot.GetIndex();
        currentIndex[i] = currentIndex[i] - 1;
        if ( currentIndex[i] < 0 )
          {
          label = 0;
          }
        else
          {
          label = static_cast< LabelType >( output->GetPixel(currentIndex) );
          }
        if ( label )
          {
          if ( ot.Get() == NumericTraits< OutputPixelType >::max() )
            {
            ot.Set(label);
            }
          else if ( ( ot.Get() != label )
                    && ( equivalenceTable[static_cast< LabelType >( ot.Get() )]
                         != equivalenceTable[label] ) )
            {
            if ( equivalenceTable[static_cast< LabelType >( ot.Get() )] > equivalenceTable[label] )
              {
              q = equivalenceTable[static_cast< LabelType >( ot.Get() )];
              for ( p = q; p <= maxLabel; p++ )
                {
                if ( equivalenceTable[p] == q )
                  {
                  equivalenceTable[p] = equivalenceTable[label];
                  }
                }
              }
            else
              {
              q = equivalenceTable[label];
              for ( p = q; p <= maxLabel; p++ )
                {
                if ( equivalenceTable[p] == q )
                  {
                  equivalenceTable[p] = equivalenceTable[static_cast< LabelType >( ot.Get() )];
                  }
                }
              }
            }
          }
        }
      if ( ot.Get() == NumericTraits< OutputPixelType >::max() )
        {
        ++maxLabel;
        equivalenceTable[maxLabel] = maxLabel;
        ot.Set(maxLabel);
        if ( maxLabel == NumericTraits< LabelType >::max() )
          {
          return;
          }
        }
      }
    progress.CompletedPixel();
    }

  for ( p = 1; p <= maxLabel; p++ )
    {
    for ( m = p; ( m <= maxLabel ) && ( equivalenceTable[m] != p ); m++ )
                                                                   {}
    if ( m > maxLabel )
      {
      for ( m = p; ( m <= maxLabel ) && ( equivalenceTable[m] < p ); m++ )
                                                                    {}
      if ( m <= maxLabel )
        {
        for ( i = m; i <= maxLabel; i++ )
          {
          if ( equivalenceTable[i] == m )
            {
            equivalenceTable[i] = p;
            }
          }
        }
      }
    }

  unsigned char * flags = new unsigned char[NumericTraits < LabelType > ::max()];
  memset(flags, 0, maxLabel + 1);
  for ( iter = m_Seeds.begin(); iter != m_Seeds.end(); iter++ )
    {
    const IndexType currentIndex = *iter;
    m = equivalenceTable[static_cast< LabelType >( output->GetPixel(currentIndex) )];
    for ( i = m; i <= maxLabel; i++ )
      {
      if ( equivalenceTable[i] == m )
        {
        flags[i] = 1;
        }
      }
    }

  ot.GoToBegin();
  if ( m_Seeds.size() == 0 )
    {
    for (; !ot.IsAtEnd(); ++ot )
      {
      ot.Set(equivalenceTable[static_cast< LabelType >( ot.Get() )]);
      }
    }
  else
    {
    for (; !ot.IsAtEnd(); ++ot )
      {
      ot.Set(flags[static_cast< LabelType >( ot.Get() )]);
      }
    }
  delete[] equivalenceTable;
  delete[] flags;
}

} // end namespace itk

#endif
