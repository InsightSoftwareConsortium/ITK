/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkParametricSpaceToImageSpaceMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkParametricSpaceToImageSpaceMeshFilter_txx
#define _itkParametricSpaceToImageSpaceMeshFilter_txx

#include "itkParametricSpaceToImageSpaceMeshFilter.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/**
 *
 */
template <class TInputMesh, class TOutputMesh>
ParametricSpaceToImageSpaceMeshFilter<TInputMesh,TOutputMesh>
::ParametricSpaceToImageSpaceMeshFilter()
{
  this->SetNumberOfRequiredInputs( 1 );
}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void 
ParametricSpaceToImageSpaceMeshFilter<TInputMesh,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


/**
 * This method causes the filter to generate its output.
 */
template <class TInputMesh, class TOutputMesh>
void 
ParametricSpaceToImageSpaceMeshFilter<TInputMesh,TOutputMesh>
::GenerateData(void) 
{
  
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;
  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

  typedef typename TInputMesh::PointsContainerPointer  InputPointsContainerPointer;
  typedef typename TOutputMesh::PointsContainerPointer OutputPointsContainerPointer;

  typedef typename TInputMesh::PointDataContainer  InputPointDataContainer;
  typedef typename TOutputMesh::PointDataContainer OutputPointDataContainer;

  typedef typename TInputMesh::PointDataContainerPointer  InputPointDataContainerPointer;
  typedef typename TOutputMesh::PointDataContainerPointer OutputPointDataContainerPointer;

  InputMeshPointer    inputMesh      =  this->GetInput();
  OutputMeshPointer   outputMesh     =  this->GetOutput();
  
  if( !inputMesh )
    {
    itkExceptionMacro( <<"Missing Input Mesh" );
    }

  if( !outputMesh )
    {
    itkExceptionMacro( <<"Missing Output Mesh" );
    }

  InputPointsContainerPointer  inPoints  = inputMesh->GetPoints();
  OutputPointsContainerPointer outPoints = OutputPointsContainer::New();

  outPoints->Reserve( inputMesh->GetNumberOfPoints() );
                         
  InputPointDataContainerPointer  inData  = inputMesh->GetPointData();
  OutputPointDataContainerPointer outData = OutputPointDataContainer::New();

  outData->Reserve( inputMesh->GetNumberOfPoints() );

  outputMesh->SetPoints( outPoints.GetPointer()  );
  outputMesh->SetPointData( outData.GetPointer() );

                         
  if( !inData )
    {
    return;
    }

  if( !inPoints )
    {
    return;
    }

  typename InputPointsContainer::ConstIterator    inputPoint   = inPoints->Begin();
  typename InputPointDataContainer::ConstIterator inputData    = inData->Begin();

  typename OutputPointsContainer::Iterator        outputPoint  = outPoints->Begin();
  typename OutputPointDataContainer::Iterator      outputData  = outData->Begin();

  // support progress methods/callbacks
  ProgressReporter progress(this, 0, inPoints->Size());
    
  const unsigned long OutputDimension = TOutputMesh::PointDimension;
    
  typename TOutputMesh::PointType point;

  while( inputPoint != inPoints->End() ) 
    {
    for(unsigned int i=0; i<OutputDimension; i++)
      {
      // Conver Index coordinates to MeshSpace
      point[i] = inputData.Value()[i]; 
      }

    outputPoint.Value() = point;
    outputData.Value()  = inputPoint.Value();
    
    ++inputData;
    ++inputPoint;
    ++outputPoint;
    ++outputData;
    progress.CompletedPixel();
    }

}



/**
 * copy information from first input to all outputs
 */
template <class TInputMesh, class TOutputMesh>
void 
ParametricSpaceToImageSpaceMeshFilter<TInputMesh,TOutputMesh>
::GenerateOutputInformation()
{
  // No additional information needs to be copied
}


} // end namespace itk

#endif
