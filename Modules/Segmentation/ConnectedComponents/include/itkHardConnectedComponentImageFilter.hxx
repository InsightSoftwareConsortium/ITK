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
  int          p, q, m;

  unsigned short *eq_tab = new unsigned short[NumericTraits < unsigned short > ::max()];
  unsigned char * flags = new unsigned char[NumericTraits < unsigned short > ::max()];
  unsigned short  label, max_label = 0;
  IndexType       index, current;
  SizeType        size;

  typename ListType::iterator iter;
  RegionType region;

  typename TOutputImage::Pointer output = this->GetOutput();
  typename TInputImage::ConstPointer input = this->GetInput();

  size = input->GetLargestPossibleRegion().GetSize();
  index.Fill(0);
  region.SetSize(size);
  region.SetIndex(index);
  output->SetLargestPossibleRegion(region);
  output->SetBufferedRegion(region);
  output->SetRequestedRegion(region);
  output->Allocate();

  itk::ImageRegionConstIterator< TInputImage > it( input, input->GetRequestedRegion() );
  itk::ImageRegionIterator< TOutputImage >     ot( output, output->GetRequestedRegion() );

  ProgressReporter progress( this, 0, output->GetRequestedRegion().GetNumberOfPixels() );
  it.GoToBegin();
  ot.GoToBegin();
  for (; !it.IsAtEnd(); ++it, ++ot )
    {
    if ( Math::NotExactlyEquals(it.Get(), itk::NumericTraits< typename itk::ImageRegionConstIterator< TInputImage >::PixelType >::ZeroValue()) )
      {
      ot.Set( NumericTraits< unsigned short >::max() );
      }
    else
      {
      ot.Set(0);
      }
    }
  eq_tab[0] = 0;
  ot.GoToBegin();
  for (; !ot.IsAtEnd(); ++ot )
    {
    if ( ot.Get() )
      {
      for ( i = 0; i < ImageDimension; i++ )
        {
        current = ot.GetIndex();
        current[i] = current[i] - 1;
        if ( current[i] < 0 )
          {
          label = 0;
          }
        else
          {
          label = static_cast< unsigned short >( output->GetPixel(current) );
          }
        if ( label )
          {
          if ( ot.Get() == NumericTraits< unsigned short >::max() )
            {
            ot.Set(label);
            }
          else if ( ( ot.Get() != label )
                    && ( eq_tab[static_cast< unsigned short >( ot.Get() )]
                         != eq_tab[label] ) )
            {
            if ( eq_tab[static_cast< unsigned short >( ot.Get() )] > eq_tab[label] )
              {
              q = eq_tab[static_cast< unsigned short >( ot.Get() )];
              for ( p = q; p <= max_label; p++ )
                {
                if ( eq_tab[p] == q )
                  {
                  eq_tab[p] = eq_tab[label];
                  }
                }
              }
            else
              {
              q = eq_tab[label];
              for ( p = q; p <= max_label; p++ )
                {
                if ( eq_tab[p] == q )
                  {
                  eq_tab[p] = eq_tab[static_cast< unsigned short >( ot.Get() )];
                  }
                }
              }
            }
          }
        }
      if ( ot.Get() == NumericTraits< unsigned short >::max() )
        {
        ++max_label;
        eq_tab[max_label] = max_label;
        ot.Set(max_label);
        if ( max_label == NumericTraits< unsigned short >::max() )
          {
          return;
          }
        }
      }
    progress.CompletedPixel();
    }

  for ( p = 1; p <= max_label; p++ )
    {
    for ( m = p; ( m <= max_label ) && ( eq_tab[m] != p ); m++ )
                                                                   {}
    if ( m > max_label )
      {
      for ( m = p; ( m <= max_label ) && ( eq_tab[m] < p ); m++ )
                                                                    {}
      if ( m <= max_label )
        {
        for ( i = m; i <= max_label; i++ )
          {
          if ( eq_tab[i] == m )
            {
            eq_tab[i] = p;
            }
          }
        }
      }
    }

  memset(flags, 0, max_label + 1);
  for ( iter = m_Seeds.begin(); iter != m_Seeds.end(); iter++ )
    {
    current = *iter;
    m = eq_tab[static_cast< unsigned short >( output->GetPixel(current) )];
    for ( i = m; i <= max_label; i++ )
      {
      if ( eq_tab[i] == m )
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
      ot.Set(eq_tab[static_cast< unsigned short >( ot.Get() )]);
      }
    }
  else
    {
    for (; !ot.IsAtEnd(); ++ot )
      {
      ot.Set(flags[static_cast< unsigned short >( ot.Get() )]);
      }
    }
  delete[] eq_tab;
  delete[] flags;
}
} // end namespace itk

#endif
