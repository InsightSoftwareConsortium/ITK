/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMaskToNarrowBandPointSetFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryMaskToNarrowBandPointSetFilter_txx
#define _itkBinaryMaskToNarrowBandPointSetFilter_txx


#include "itkBinaryMaskToNarrowBandPointSetFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"


namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputMesh>
BinaryMaskToNarrowBandPointSetFilter<TInputImage,TOutputMesh>
::BinaryMaskToNarrowBandPointSetFilter()
{

  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

  m_BandWidth = 5;

  m_DistanceFilter = DistanceFilterType::New();
  m_RescaleFilter  = RescaleFilterType::New();

  m_RescaleFilter->SetOutputMinimum( -0.5 );
  m_RescaleFilter->SetOutputMaximum(  0.5 );

  m_DistanceFilter->SetInput( m_RescaleFilter->GetOutput() );
  m_DistanceFilter->NarrowBandingOn();
  m_DistanceFilter->SetNarrowBandwidth( m_BandWidth );

  PointDataContainerPointer  pointData  = PointDataContainer::New();

  OutputMeshPointer mesh = this->GetOutput();

  mesh->SetPointData( pointData.GetPointer() );


}



/**
 *
 */
template <class TInputImage, class TOutputMesh>
BinaryMaskToNarrowBandPointSetFilter<TInputImage,TOutputMesh>
::~BinaryMaskToNarrowBandPointSetFilter()
{
}
  


/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
BinaryMaskToNarrowBandPointSetFilter<TInputImage,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "BandWidth: " << m_BandWidth << std::endl;
}


/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
BinaryMaskToNarrowBandPointSetFilter<TInputImage,TOutputMesh>
::GenerateOutputInformation()
{
}




/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
BinaryMaskToNarrowBandPointSetFilter<TInputImage,TOutputMesh>
::SetInput( const InputImageType * inputImage )
{

  // This const_cast is needed due to the lack of 
  // const-correctness in the ProcessObject.
  this->SetNthInput( 0, 
            const_cast< InputImageType * >( inputImage ) );

}




/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
BinaryMaskToNarrowBandPointSetFilter<TInputImage,TOutputMesh>
::GenerateData(void)
{

  m_DistanceFilter->SetNarrowBandwidth( m_BandWidth );

  m_RescaleFilter->SetInput( this->GetInput(0) );

  m_DistanceFilter->Update();

  OutputMeshPointer           mesh      = this->GetOutput();
  InputImageConstPointer      image     = this->GetInput(0);

  PointsContainerPointer      points    = PointsContainer::New();
  PointDataContainerPointer   pointData = PointDataContainer::New();

  NodeContainerPointer nodes =  m_DistanceFilter->GetOutputNarrowBand();

  unsigned long numberOfPixels    = nodes->Size();
  ProgressReporter progress(this, 0, numberOfPixels);

  typename NodeContainer::ConstIterator  nodeItr   = nodes->Begin();
  typename NodeContainer::ConstIterator  lastNode  = nodes->End();

  PointType point;

  while( nodeItr != lastNode ) 
    {
    const NodeType & node = nodeItr.Value();
    const float distance = node.GetValue();
    if( fabs( distance ) < m_BandWidth )
      {
      image->TransformIndexToPhysicalPoint( node.GetIndex(), point );
      points->push_back( point );
      pointData->push_back( distance );
      }
    ++nodeItr;
    progress.CompletedPixel();
    }

  mesh->SetPoints( points );
  mesh->SetPointData( pointData );

  // This indicates that the current BufferedRegion is equal to the 
  // requested region. This action prevents useless rexecutions of
  // the pipeline.
  mesh->SetBufferedRegion( mesh->GetRequestedRegion() );
  
}


} // end namespace itk

#endif
