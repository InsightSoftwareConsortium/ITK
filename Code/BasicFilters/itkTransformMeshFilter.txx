/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkTransformMeshFilter_txx
#define _itkTransformMeshFilter_txx

#include "itkTransformMeshFilter.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/**
 *
 */
template <class TInputMesh, class TOutputMesh>
TransformMeshFilter<TInputMesh,TOutputMesh>
::TransformMeshFilter()
{
}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void 
TransformMeshFilter<TInputMesh,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


/**
 * This method causes the filter to generate its output.
 */
template <class TInputMesh, class TOutputMesh>
void 
TransformMeshFilter<TInputMesh,TOutputMesh>
::GenerateData(void) 
{
  
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;
  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

  typedef typename TInputMesh::PointsContainerPointer  InputPointsContainerPointer;
  typedef typename TOutputMesh::PointsContainerPointer OutputPointsContainerPointer;

  InputMeshPointer    inputMesh      =  this->GetInput();
  OutputMeshPointer   outputMesh     =  this->GetOutput();
  
  if( !inputMesh )
    {
    throw ExceptionObject();
    }

  if( !outputMesh )
    {
    throw ExceptionObject();
    }

  InputPointsContainerPointer  inPoints  = inputMesh->GetPoints();
  OutputPointsContainerPointer outPoints = outputMesh->GetPoints();

  outPoints->Reserve( inputMesh->GetNumberOfPoints() );
  outPoints->Squeeze();  // in case the previous mesh had 
                         // allocated a larger memory

  typename InputPointsContainer::ConstIterator  inputPoint  = inPoints->Begin();
  typename OutputPointsContainer::Iterator      outputPoint = outPoints->Begin();

  while( inputPoint != inPoints->End() ) 
    {
    outputPoint.Value() = 
	           m_Transform.TransformPoint( inputPoint.Value() );

    ++inputPoint;
    ++outputPoint;
    }


  // Create duplicate references to the rest of data on the mesh

  outputMesh->SetPointData(  inputMesh->GetPointData() );
  
  outputMesh->SetCellLinks(  inputMesh->GetCellLinks() );
  
  outputMesh->SetCells(  inputMesh->GetCells() );
  outputMesh->SetCellData(  inputMesh->GetCellData() );
  
  
  unsigned int maxDimension = TInputMesh::MaxTopologicalDimension;

  for( unsigned int dim = 0; dim < maxDimension; dim++ )
    {
    outputMesh->SetBoundaries(    dim, inputMesh->GetBoundaries(dim)   );
    outputMesh->SetBoundaryData(  dim, inputMesh->GetBoundaryData(dim) );
    outputMesh->SetBoundaryAssignments(  dim,
                                         inputMesh->GetBoundaryAssignments(dim) );
    }
}

} // end namespace itk

#endif
