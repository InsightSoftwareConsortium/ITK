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

template <class TInputImage>
AutoCropLabelMapFilter<TInputImage>
::AutoCropLabelMapFilter()
{
  m_CropBorder.Fill( 0 );
}

template <class TInputImage>
AutoCropLabelMapFilter<TInputImage>
::FindBoundingBox()
{

  // find the bounding box of the objects
  IndexType mins;
  mins.Fill( NumericTraits< long >::max() );
  IndexType maxs;
  maxs.Fill( NumericTraits< long >::NonpositiveMin() );

  const InputImageType * inputImage = this->GetInput();

  // iterate over all the lines
  typename InputImageType::LabelObjectContainerType container = inputImage->GetLabelObjectContainer();
  typename InputImageType::LabelObjectContainerType::const_iterator loit = container.begin();

  while( loit != container.end() )
    {
    const LabelObjectType * labelObject = loit->second;
    typename LabelObjectType::LineContainerType::const_iterator lit;
    const typename LabelObjectType::LineContainerType & lineContainer = labelObject->GetLineContainer();
    lit = lineContainer.begin()
    while(lit != lineContainer.end())
      {
      const IndexType & idx = lit->GetIndex();
      unsigned long length = lit->GetLength();
  
      // update the mins and maxs
      for( int i=0; i<ImageDimension; i++)
        {
        if( idx[i] < mins[i] )
          {
          mins[i] = idx[i];
          }
        if( idx[i] > maxs[i] )
          {
          maxs[i] = idx[i];
          }
        }
      // must fix the max for the axis 0
      if( idx[0] + (long)length > maxs[0] )
        {
        maxs[0] = idx[0] + length - 1;
        }
      lit++;
      }
    loit++;
    }

}

template <class TInputImage>
AutoCropLabelMapFilter<TInputImage>
::SetAndPadCropRegion()
{
  // prefetch image region and size
  InputImageRegionType cropRegion = input->GetLargestPossibleRegion();

  // final computation
  SizeType regionSize;
  for( int i=0; i<ImageDimension; i++ )
    {
    regionSize[i] = maxs[i] - mins[i] + 1;
    }
  cropRegion.SetIndex( mins );
  cropRegion.SetSize( regionSize );
  
  // pad the crop border while ensuring border is not larger than the largest
  // possible region of the input image
  cropRegion.PadByRadius( m_CropBorder );
  cropRegion.Crop( input->GetLargestPossibleRegion() );

  // finally set that region as the largest output region
  this->SetRegion(cropRegion);
  m_CropTimeStamp.Modified();
  
  Superclass::GenerateOutputInformation();

  const InputImageType * input = this->GetInput();

  if( !(input->GetMTime() > m_CropTimeStamp) && !(this->GetMTime() > m_CropTimeStamp) )
    {
    // early exit, crop sizes already computed
    return;
    }
    
  // update the input if needed
  if( input->GetSource() )
    {
    ProcessObject * upstream = input->GetSource();
    if (upstream)
      {
      upstream->Update();
      }
    }

}

template <class TInputImage>
void 
AutoCropLabelMapFilter<TInputImage>
::GenerateOutputInformation()
{
  FindBoundingBox();
  SetAndPadCropRegion();
}

template <class TImage>
void
ShiftScaleLabelMapFilter<TImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Crop Border: "  << m_CropBorder << std::endl;
  os << indent << "Crop Time Stamp: "  << m_CropTimeStamp << std::endl;
}

} // end namespace itk

#endif
