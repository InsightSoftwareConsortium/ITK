/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToParametricSpaceFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToParametricSpaceFilter_txx
#define _itkImageToParametricSpaceFilter_txx


#include "itkImageToParametricSpaceFilter.h"
#include "itkNumericTraits.h"
#include "itkImageIterator.h"
#include "itkProgressReporter.h"


namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputMesh>
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::ImageToParametricSpaceFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(PointDimension);
  m_ComputeIndices = true;
}

/**
 *
 */
template <class TInputImage, class TOutputMesh>
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::~ImageToParametricSpaceFilter()
{
}
  


/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "ComputeIndices: " << m_ComputeIndices << std::endl;
}

/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::GenerateOutputInformation()
{

  OutputMeshPointer          mesh       = this->GetOutput();
  PointsContainerPointer     points     = mesh->GetPoints();
  PointDataContainerPointer  pointData  = PointDataContainer::New();
  InputImageConstPointer     image      = this->GetInput(0);

  const unsigned long numberOfPixels =
    image->GetRequestedRegion().GetNumberOfPixels(); 

  points->Reserve( numberOfPixels );

  pointData->Reserve( numberOfPixels );

  mesh->SetPointData( pointData.GetPointer() );

}




/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::GenerateData(void)
{
  OutputMeshPointer           mesh      = this->GetOutput();
  PointsContainerPointer      points    = mesh->GetPoints();
  PointDataContainerPointer   pointData = PointDataContainer::New();
  InputImageConstPointer      image     = this->GetInput(0);
  InputImageRegionType        region    = image->GetRequestedRegion();

  unsigned long numberOfPixels    = region.GetNumberOfPixels();

  points->Reserve( numberOfPixels );
  pointData->Reserve( numberOfPixels );

  mesh->SetPointData( pointData.GetPointer() );
  mesh->SetBufferedRegion( mesh->GetRequestedRegion() );

  // support progress methods/callbacks
  ProgressReporter progress(this, 0, numberOfPixels);
   
  for( unsigned int component=0; component<PointDimension; component++)
    {

    image = this->GetInput( component );
    InputImageIterator it( image, image->GetRequestedRegion() );
    
    PointsContainerIterator     point  = points->Begin();

    it.GoToBegin();
    while( !it.IsAtEnd() ) 
      {
      (point.Value())[ component ] = it.Get();
      it.GetIndex();
      ++it;
      ++point;
      progress.CompletedPixel();
      }
    }

  if( m_ComputeIndices )
    {
    PointDataContainerIterator  data   = pointData->Begin();
    image = this->GetInput( 0 );
    InputImageIterator it( image, image->GetRequestedRegion() );
    it.GoToBegin();
    while( !it.IsAtEnd() )
      {
      //  The data at each point is the index 
      //  of the corresponding pixel on the image.
      typedef typename OutputMeshType::PixelType  MeshPixelType; 
      MeshPixelType point;
      image->TransformIndexToPhysicalPoint( it.GetIndex(), point );
      (data.Value()) = point;
      ++it;
      ++data;
      }
    }
  
}



} // end namespace itk

#endif
