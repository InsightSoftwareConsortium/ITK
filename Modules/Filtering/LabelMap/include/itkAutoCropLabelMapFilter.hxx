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
#ifndef itkAutoCropLabelMapFilter_hxx
#define itkAutoCropLabelMapFilter_hxx
#include "itkAutoCropLabelMapFilter.h"

namespace itk
{
template< typename TInputImage >
AutoCropLabelMapFilter< TInputImage >
::AutoCropLabelMapFilter()
{
  m_CropBorder.Fill(0);
}

template< typename TInputImage >
void
AutoCropLabelMapFilter< TInputImage >
::GenerateOutputInformation()
{
  const InputImageType *input = this->GetInput();

  // update the input if needed
  if ( input->GetSource() )
    {
    ProcessObject *upstream = input->GetSource();
    if ( upstream )
      {
      upstream->Update();
      }
    }

  // find the bounding box of the objects
  IndexType minIdx;
  minIdx.Fill( NumericTraits< typename TInputImage::IndexValueType >::max() );
  IndexType maxIdx;
  maxIdx.Fill( NumericTraits< typename TInputImage::IndexValueType >::NonpositiveMin() );

  const InputImageType *inputImage = this->GetInput();

  // iterate over all the lines
  typename InputImageType::ConstIterator loit( inputImage );

  while ( ! loit.IsAtEnd() )
    {
    const LabelObjectType *labelObject = loit.GetLabelObject();
    typename LabelObjectType::ConstLineIterator lit( labelObject );
    while ( ! lit.IsAtEnd() )
      {
      const IndexType & idx = lit.GetLine().GetIndex();
      const typename TInputImage::IndexValueType length = lit.GetLine().GetLength();

      // update the mins and maxs
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        if ( idx[i] < minIdx[i] )
          {
          minIdx[i] = idx[i];
          }
        if ( idx[i] > maxIdx[i] )
          {
          maxIdx[i] = idx[i];
          }
        }
      // must fix the max for the axis 0
      if ( idx[0] + length > maxIdx[0] )
        {
        maxIdx[0] = idx[0] + length - 1;
        }
      ++lit;
      }
    ++loit;
    }


  // final computation
  SizeType regionSize;
  InputImageRegionType cropRegion;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    regionSize[i] = maxIdx[i] - minIdx[i] + 1;
    }
  cropRegion.SetIndex(minIdx);
  cropRegion.SetSize(regionSize);

  // pad the crop border while ensuring border is not larger than the largest
  // possible region of the input image
  cropRegion.PadByRadius(m_CropBorder);
  cropRegion.Crop( input->GetLargestPossibleRegion() );

  // finally set that region as the largest output region
  this->SetRegion(cropRegion);

  Superclass::GenerateOutputInformation();
}

template< typename TImage >
void
AutoCropLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Crop Border: "  << m_CropBorder << std::endl;
}
} // end namespace itk

#endif
