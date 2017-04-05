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
#ifndef itkGridForwardWarpImageFilter_hxx
#define itkGridForwardWarpImageFilter_hxx

#include "itkGridForwardWarpImageFilter.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include "itkLineIterator.h"

namespace itk
{

template< typename TDisplacementField, typename TOutputImage >
GridForwardWarpImageFilter< TDisplacementField, TOutputImage >
::GridForwardWarpImageFilter() :
  m_BackgroundValue( NumericTraits< PixelType >::ZeroValue() ),
  m_ForegroundValue( NumericTraits< PixelType >::OneValue() ),
  m_GridPixSpacing( 5 )
{
}

template< typename TDisplacementField, typename TOutputImage >
void
GridForwardWarpImageFilter< TDisplacementField, TOutputImage >
::GenerateData()
{
  OutputImagePointer           outputPtr = this->GetOutput();
  DisplacementFieldConstPointer fieldPtr = this->GetInput();

  SpacingType spacing = fieldPtr->GetSpacing();

  outputPtr->SetRegions( fieldPtr->GetRequestedRegion() );
  outputPtr->SetOrigin( fieldPtr->GetOrigin() );
  outputPtr->SetSpacing(spacing);
  outputPtr->Allocate();
  outputPtr->FillBuffer(m_BackgroundValue);

  IndexType FirstIndex = fieldPtr->GetRequestedRegion().GetIndex();
  IndexType LastIndex = fieldPtr->GetRequestedRegion().GetIndex()
                        + fieldPtr->GetRequestedRegion().GetSize();

  // Iterator for the output image
  typedef ImageRegionIteratorWithIndex< OutputImageType > OutputImageIteratorWithIndex;
  OutputImageIteratorWithIndex iter( outputPtr, outputPtr->GetRequestedRegion() );

  // Iterator for the deformation field
  typedef ImageRegionConstIterator< DisplacementFieldType > DisplacementFieldIterator;
  DisplacementFieldIterator fieldIt( fieldPtr, outputPtr->GetRequestedRegion() );

  // Bresenham line iterator
  typedef LineIterator< OutputImageType > LineIteratorType;

  IndexType                                index, refIndex, targetIndex;
  ContinuousIndex< float, ImageDimension > contindex;
  DisplacementType                         displacement;
  bool                                     inside, targetIn;

  for ( iter.GoToBegin(), fieldIt.GoToBegin(); !iter.IsAtEnd(); ++iter, ++fieldIt )
    {
    index = iter.GetIndex();

    unsigned int numGridIntersect = 0;
    for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
      {
      numGridIntersect += ( ( index[dim] % m_GridPixSpacing ) == 0 );
      }

    if ( numGridIntersect == ImageDimension )
      {
      // We are on a grid point => transform it

      // Get the required displacement
      displacement = fieldIt.Get();

      // Compute the mapped point
      inside = true;
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        contindex[j] = index[j] + displacement[j] / spacing[j];
        if ( contindex[j] < FirstIndex[j] || contindex[j] > ( LastIndex[j] - 1 ) )
          {
          inside = false;
          break;
          }
        refIndex[j] = Math::Round< IndexValueType >(contindex[j]);
        }

      if ( inside )
        {
        // We know the current grid point is inside.
        // We will check if the grid points that are above are also inside
        // In such a case we draw a Bresenham line
        for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
          {
          targetIndex = index;
          targetIndex[dim] += m_GridPixSpacing;
          if ( targetIndex[dim] < LastIndex[dim] )
            {
            // Get the required displacement
            displacement = fieldPtr->GetPixel(targetIndex);

            // Compute the mapped point
            targetIn = true;
            for ( unsigned int j = 0; j < ImageDimension; j++ )
              {
              contindex[j] = targetIndex[j] + displacement[j] / spacing[j];
              if ( contindex[j] < FirstIndex[j] || contindex[j] > ( LastIndex[j] - 1 ) )
                {
                targetIn = false;
                break;
                }
              targetIndex[j] = Math::Round< IndexValueType >(contindex[j]);
              }

            if ( targetIn )
              {
              for ( LineIteratorType lineIter(outputPtr, refIndex, targetIndex);
                    !lineIter.IsAtEnd(); ++lineIter )
                {
                lineIter.Set(m_ForegroundValue);
                }
              }
            }
          }
        }
      }
    }

}

template< typename TDisplacementField, typename TOutputImage >
void
GridForwardWarpImageFilter< TDisplacementField, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_BackgroundValue )
     << std::endl;
  os << indent << "ForegroundValue: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_ForegroundValue )
     << std::endl;
  os << indent << "m_GridPixSpacing: " << m_GridPixSpacing << std::endl;
}
} // end namespace itk

#endif
