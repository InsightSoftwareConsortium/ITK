/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryPointToCoreAtomImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryPointToCoreAtomImageFilter_txx
#define __itkBloxBoundaryPointToCoreAtomImageFilter_txx

#include "itkBloxBoundaryPointToCoreAtomImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkConicShellInteriorExteriorSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalConstIterator.h"

namespace itk
{

template< unsigned int dim >
BloxBoundaryPointToCoreAtomImageFilter< dim >
::BloxBoundaryPointToCoreAtomImageFilter()
{
  itkDebugMacro(<< "BloxBoundaryPointToCoreAtomImageFilter::BloxBoundaryPointToCoreAtomImageFilter() called");

  m_DistanceMin = 8;
  m_DistanceMax = 12;
  m_Epsilon = 0;
  m_Polarity = 0;
  m_InverseNumberOfBoundaryPoints = 0;
  m_BoundaryPointsPerUpdate = 0;
  m_BoundaryPointsBeforeUpdate = 0;
}

template< unsigned int dim >
void
BloxBoundaryPointToCoreAtomImageFilter< dim >
::GenerateInputRequestedRegion()
{
  itkDebugMacro(<< "BloxBoundaryPointToCoreAtomImageFilter::GenerateInputRequestedRegion() called");
  
  Superclass::GenerateInputRequestedRegion();
}


template< unsigned int dim >
void
BloxBoundaryPointToCoreAtomImageFilter< dim >
::GenerateData()
{
  itkDebugMacro(<< "BloxBoundaryPointToCoreAtomImageFilter::GenerateData() called");

  // Get the input and output pointers
  m_InputPtr  = this->GetInput(0);
  m_OutputPtr = this->GetOutput(0);

  // Allocate the output
  m_OutputPtr->SetBufferedRegion( m_OutputPtr->GetRequestedRegion() );
  m_OutputPtr->Allocate();
  
  // To avoid appending data, empty the output image
  m_OutputPtr->EmptyImage();

  //---- Set up the progress update ----
 
  float numBoundaryPoints = m_InputPtr->GetNumBoundaryPoints();
  float numUpdates = 100.0;
    
  // Make sure we have at least one pixel.
  if(numBoundaryPoints < 1)
    {
    numBoundaryPoints = 1;
    }
    
  // We cannot update more times than there are BoundaryPoints.
  if(numUpdates > numBoundaryPoints)
    {
    numUpdates = numBoundaryPoints;
    }
    
  // Calculate the interval for updates.
  m_BoundaryPointsPerUpdate = static_cast<unsigned long>(numBoundaryPoints/numUpdates);
  m_InverseNumberOfBoundaryPoints = 1.0 / numBoundaryPoints;
    
  // Set the progress to 0.  The filter is just starting.
  this->UpdateProgress(0);

  m_BoundaryPointsBeforeUpdate = m_BoundaryPointsPerUpdate;

  m_CurrentBoundaryPoint = 0;

  this->FindCoreAtoms();
}

template< unsigned int dim >
void
BloxBoundaryPointToCoreAtomImageFilter< dim >
::FindCoreAtoms()
{
  // Create an iterator to walk the source image
  typedef ImageRegionConstIterator<TInputImage> ImageIteratorType;

  ImageIteratorType imageIt ( m_InputPtr,
                              m_InputPtr->GetRequestedRegion() );

  // Iterate through the entire image (all pixels) and look for core atoms
  for ( imageIt.GoToBegin(); !imageIt.IsAtEnd(); ++imageIt)
    {
    // The iterator for accessing linked list info
    typename itk::BloxBoundaryPointPixel<dim>::const_iterator bpiterator;

    // Walk through all of the elements at the pixel
    for (bpiterator = imageIt.Value().begin(); bpiterator != imageIt.Value().end(); ++bpiterator)
      {
      this->FindCoreAtomsAtBoundaryPoint( *bpiterator );

      if(--m_BoundaryPointsBeforeUpdate == 0)
        {
        m_BoundaryPointsBeforeUpdate = m_BoundaryPointsPerUpdate;
        m_CurrentBoundaryPoint += m_BoundaryPointsPerUpdate;
        this->UpdateProgress(m_CurrentBoundaryPoint * m_InverseNumberOfBoundaryPoints);
        }
      }
    }

  // Compute mean core atom diameter
  itk::ImageRegionIterator<TOutputImage> bloxIt = 
    itk::ImageRegionIterator<TOutputImage>(m_OutputPtr, m_OutputPtr->GetRequestedRegion() );

  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    ( &bloxIt.Value() )->CalcMeanCoreAtomDiameter();
    }  
}

template< unsigned int dim >
void
BloxBoundaryPointToCoreAtomImageFilter< dim >
::FindCoreAtomsAtBoundaryPoint(BloxBoundaryPointItem<dim>* pBPOne)
{

  typedef BloxBoundaryPointItem<NDimensions> BPItemType;

  // When looking for core atoms at a boundary point, we want to examine
  // all of the boundary points within blox that are part of a conical
  // region extending out in the direction of the gradient of the boundary
  // point.

  //---------Create and initialize a conic shell spatial function-----------
  typedef itk::ConicShellInteriorExteriorSpatialFunction<NDimensions> FunctionType;
  typedef typename FunctionType::GradientType FunctionGradientType;

  typename FunctionType::Pointer spatialFunc = FunctionType::New();

  // Set the properties of the conic shell
  spatialFunc->SetDistanceMin(m_DistanceMin);
  spatialFunc->SetDistanceMax(m_DistanceMax);
  spatialFunc->SetEpsilon(m_Epsilon);
  spatialFunc->SetPolarity(m_Polarity);

  // Set the origin of the conic shell to the current boundary point location
  PositionType spatialFunctionOrigin = pBPOne->GetPhysicalPosition();
  spatialFunc->SetOrigin(spatialFunctionOrigin);

  // Covert the origin position to a vector
  //VectorType spatialFunctionOriginVector = spatialFunctionOrigin.GetVectorFromOrigin();

  VectorType spatialFunctionOriginVector;
  spatialFunctionOriginVector.Set_vnl_vector( spatialFunctionOrigin.Get_vnl_vector() );

  // Set the gradient of the conic shell to the current boundary point gradient
  FunctionGradientType spatialFunctionGradient = pBPOne->GetGradient();
  spatialFunc->SetOriginGradient(spatialFunctionGradient);

  // Create a seed position for the spatial function iterator we'll use shortly
  typename TInputImage::IndexType seedIndex;

  // Normalize the origin gradient
  VectorType seedVector;
  seedVector.Set_vnl_vector(spatialFunctionGradient.Get_vnl_vector());
  seedVector = seedVector / seedVector.GetNorm();
  
  // If the polarity is 1, the seed position is in the direction
  // opposite the gradient
  if(m_Polarity == 1)
    seedVector = seedVector * -1;
  
  // A "safe" seed position is the closest point in the conical region
  // along the axis of the cone
  PositionType seedPos = spatialFunctionOrigin + (seedVector * m_DistanceMin);

  // If the seed position is inside the image, go ahead and process it
  if( m_InputPtr->TransformPhysicalPointToIndex(seedPos, seedIndex) )
    {
    // Create and initialize a spatial function iterator
    typedef itk::FloodFilledSpatialFunctionConditionalConstIterator<TInputImage, FunctionType> ConicItType;
    ConicItType sfi = ConicItType(m_InputPtr, spatialFunc, seedIndex);
    sfi.SetIntersectInclusionStrategy();

    // Walk the spatial function
    for( sfi.GoToBegin(); !( sfi.IsAtEnd() ); ++sfi)
      {
      // The iterator for accessing linked list info
      typename itk::BloxBoundaryPointPixel<NDimensions>::const_iterator bpiterator;

      // Walk through all of the elements at the pixel
      for (bpiterator = sfi.Get().begin(); bpiterator != sfi.Get().end(); ++bpiterator)
        {
        // Get the pointer of the blox
        BPItemType* pBPTwo = *bpiterator;

        // Get the physical positions of the two boundary points
        PositionType P1 = pBPOne->GetPhysicalPosition();
        PositionType P2 = pBPTwo->GetPhysicalPosition();

        // Form the two vectors between them
        VectorType C12 = P2 - P1;

        // If we don't meet distance criteria, move on
        if(!( (C12.GetNorm() > m_DistanceMin) && (C12.GetNorm() < m_DistanceMax) ) )
          continue;

        VectorType C21 = P1 - P2;

        C12 = C12 / C12.GetNorm();
        C21 = C21 / C21.GetNorm();

        // Get the gradients of the two boundary points
        GradientType G1 = pBPOne->GetGradient();
        GradientType G2 = pBPTwo->GetGradient();

        G1 = G1 / G1.GetNorm();
        G2 = G2 / G2.GetNorm();

        // Calculate face-to-faceness
        double faceToFaceness = dot_product(G1.Get_vnl_vector(), C12.Get_vnl_vector() ) *
          dot_product(G2.Get_vnl_vector(), C21.Get_vnl_vector() );

        // If face-to-faceness meets threshold criteria
        if( faceToFaceness > (1.0 - m_Epsilon) )
          {
          // Figure out the center of the core atom
          PositionType coreAtomCenter = P1 + (P2 - P1) / 2;

          // Figure out the diameter of the core atom
          double coreAtomDiameter = (P2-P1).GetNorm();

          // Create a new core atom
          BloxCoreAtomItem<NDimensions>* pCoreAtom = new BloxCoreAtomItem<NDimensions>;
          
          // Set its boundary points, center, and diameter
          pCoreAtom->SetBoundaryPointA(pBPOne);
          pCoreAtom->SetBoundaryPointB(pBPTwo);
          pCoreAtom->SetCenterPosition(coreAtomCenter);
          pCoreAtom->SetDiameter(coreAtomDiameter);

          // Figure out the data space coordinates of the center
          IndexType coreAtomPos;
          
          m_OutputPtr->TransformPhysicalPointToIndex(coreAtomCenter, coreAtomPos);
         
          // Store the new core atom in the correct spot
          m_OutputPtr->GetPixel(coreAtomPos).push_back(pCoreAtom);

          } // end if face-to-faceness meets criteria
        } // end iterate through boundary points in pixel
      } // end iterate through the conic shell
   } // end if the seed position for the conic shell is in the image
}

template< unsigned int dim >
void
BloxBoundaryPointToCoreAtomImageFilter< dim >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Minimum core atom search distance: " << m_DistanceMin << std::endl;
  os << indent << "Maximum core atom search distance: " << m_DistanceMax << std::endl;
  os << indent << "Core atom search epsilon: " << m_Epsilon << std::endl;
  os << indent << "Core atom search polarity: " << m_Polarity << std::endl;

  os << indent << "Inverse number of boundary points: " << m_InverseNumberOfBoundaryPoints << std::endl;
  os << indent << "Current boundary point: " << m_CurrentBoundaryPoint << std::endl;
  os << indent << "Boundary points per update: " << m_BoundaryPointsPerUpdate << std::endl;
  os << indent << "Boundary points before update: " << m_BoundaryPointsBeforeUpdate << std::endl;
}

} // end namespace

#endif
