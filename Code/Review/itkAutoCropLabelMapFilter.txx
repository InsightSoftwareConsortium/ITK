/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutoCropLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAutoCropLabelMapFilter_txx
#define __itkAutoCropLabelMapFilter_txx
#include "itkAutoCropLabelMapFilter.h"

namespace itk
{
template< class TInputImage >
AutoCropLabelMapFilter< TInputImage >
::AutoCropLabelMapFilter()
{
  m_CropBorder.Fill(0);
}

template< class TInputImage >
void
AutoCropLabelMapFilter< TInputImage >
::GenerateOutputInformation()
{
  const InputImageType *input = this->GetInput();

  //
  // FIXME: This way of implementing a GenerateOutputInformation() method is
  // suspicious.
  //        Needs to be revisited.
  //
  if ( !( input->GetMTime() > m_CropTimeStamp ) && !( this->GetMTime() > m_CropTimeStamp ) )
    {
    // early exit, crop sizes already computed
    return;
    }

  // update the input if needed
  if ( input->GetSource() )
    {
    ProcessObject *upstream = input->GetSource();
    if ( upstream )
      {
      upstream->Update();
      }
    }

  this->FindBoundingBox();
  this->SetAndPadCropRegion();
}

template< class TInputImage >
void
AutoCropLabelMapFilter< TInputImage >
::FindBoundingBox()
{
  // find the bounding box of the objects
  this->m_MinIndex.Fill( NumericTraits< typename TInputImage::IndexValueType >::max() );
  this->m_MaxIndex.Fill( NumericTraits< typename TInputImage::IndexValueType >::NonpositiveMin() );

  const InputImageType *inputImage = this->GetInput();

  // iterate over all the lines
  typename InputImageType::LabelObjectContainerType container = inputImage->GetLabelObjectContainer();
  typename InputImageType::LabelObjectContainerType::const_iterator loit = container.begin();

  while ( loit != container.end() )
    {
    const LabelObjectType *labelObject = loit->second;
    typename LabelObjectType::LineContainerType::const_iterator lit;
    const typename LabelObjectType::LineContainerType & lineContainer = labelObject->GetLineContainer();

    lit = lineContainer.begin();

    while ( lit != lineContainer.end() )
      {
      const IndexType & idx = lit->GetIndex();
      const typename TInputImage::IndexValueType length = lit->GetLength();

      // update the mins and maxs
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        if ( idx[i] < this->m_MinIndex[i] )
          {
          this->m_MinIndex[i] = idx[i];
          }
        if ( idx[i] > this->m_MaxIndex[i] )
          {
          this->m_MaxIndex[i] = idx[i];
          }
        }
      // must fix the max for the axis 0
      if ( idx[0] + length > this->m_MaxIndex[0] )
        {
        this->m_MaxIndex[0] = idx[0] + length - 1;
        }
      lit++;
      }
    loit++;
    }
}

template< class TInputImage >
void
AutoCropLabelMapFilter< TInputImage >
::SetAndPadCropRegion()
{
  const InputImageType *input = this->GetInput();

  // prefetch image region and size
  InputImageRegionType cropRegion = input->GetLargestPossibleRegion();

  // final computation
  SizeType regionSize;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    regionSize[i] = this->m_MaxIndex[i] - this->m_MinIndex[i] + 1;
    }
  cropRegion.SetIndex(this->m_MinIndex);
  cropRegion.SetSize(regionSize);

  // pad the crop border while ensuring border is not larger than the largest
  // possible region of the input image
  cropRegion.PadByRadius(m_CropBorder);
  cropRegion.Crop( input->GetLargestPossibleRegion() );

  // finally set that region as the largest output region
  this->SetRegion(cropRegion);
  m_CropTimeStamp.Modified();

  Superclass::GenerateOutputInformation();
}

template< class TImage >
void
AutoCropLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Crop Border: "  << m_CropBorder << std::endl;
  os << indent << "Crop Time Stamp: "  << m_CropTimeStamp << std::endl;
  os << indent << "Min Indexes : "  << m_MinIndex << std::endl;
  os << indent << "Max Indexes : "  << m_MaxIndex << std::endl;
}
} // end namespace itk

#endif
