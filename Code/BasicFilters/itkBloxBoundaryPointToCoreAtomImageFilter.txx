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

  this->FindCoreAtoms();
}

template< unsigned int dim >
void
BloxBoundaryPointToCoreAtomImageFilter< dim >
::FindCoreAtoms()
{
  
  // Create an iterator to walk the source image
  typedef ImageRegionConstIterator<TInputImage> TImageIteratorType;

  TImageIteratorType imageIt ( m_InputPtr,
                               m_InputPtr->GetRequestedRegion() );

  // Iterate through the entire image (all pixels) and look for core atoms
  for ( imageIt.GoToBegin(); !imageIt.IsAtEnd(); ++imageIt)
    {
    // The iterator for accessing linked list info
    itk::BloxBoundaryPointPixel<dim>::const_iterator bpiterator;

    // Walk through all of the elements at the pixel
    for (bpiterator = imageIt.Value().begin(); bpiterator != imageIt.Value().end(); ++bpiterator)
      {
      this->FindCoreAtomsAtBoundaryPoint( *bpiterator );
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

  typedef BloxBoundaryPointItem<NDimensions> TBPItemType;

  // When looking for core atoms at a boundary point, we want to examine
  // all of the boundary points within blox that are part of a conical
  // region extending out in the direction of the gradient of the boundary
  // point.

  //---------Create and initialize a conic shell spatial function-----------
  typedef itk::ConicShellInteriorExteriorSpatialFunction<NDimensions> TFunctionType;
  typedef TFunctionType::TGradientType TFunctionGradientType;

  TFunctionType::Pointer spatialFunc = TFunctionType::New();

  // Set the properties of the conic shell
  spatialFunc->SetDistanceMin(m_DistanceMin);
  spatialFunc->SetDistanceMax(m_DistanceMax);
  spatialFunc->SetEpsilon(m_Epsilon);
  spatialFunc->SetPolarity(m_Polarity);

  // Set the origin of the conic shell to the current boundary point location
  TPositionType spatialFunctionOrigin = pBPOne->GetPhysicalPosition();
  spatialFunc->SetOrigin(spatialFunctionOrigin);

  // Covert the origin position to a vector
  //TVectorType spatialFunctionOriginVector = spatialFunctionOrigin.GetVectorFromOrigin();

  TVectorType spatialFunctionOriginVector;
  spatialFunctionOriginVector.Set_vnl_vector( spatialFunctionOrigin.Get_vnl_vector() );

  // Set the gradient of the conic shell to the current boundary point gradient
  TFunctionGradientType spatialFunctionGradient = pBPOne->GetGradient();
  spatialFunc->SetOriginGradient(spatialFunctionGradient);

  // Create a seed position for the spatial function iterator we'll use shortly
  typename TInputImage::IndexType seedIndex;

  // Normalize the origin gradient
  TVectorType seedVector;
  seedVector.Set_vnl_vector(spatialFunctionGradient.Get_vnl_vector());
  seedVector = seedVector / seedVector.GetNorm();
  
  // If the polarity is 1, the seed position is in the direction
  // opposite the gradient
  if(m_Polarity == 1)
    seedVector = seedVector * -1;
  
  // A "safe" seed position is the closest point in the conical region
  // along the axis of the cone
  TPositionType seedPos = spatialFunctionOrigin + (seedVector * m_DistanceMin);

  // If the seed position is inside the image, go ahead and process it
  if( m_InputPtr->TransformPhysicalPointToIndex(seedPos, seedIndex) )
    {
    // Create and initialize a spatial function iterator
    typedef itk::FloodFilledSpatialFunctionConditionalConstIterator<TInputImage, TFunctionType> TSphereItType;
    TSphereItType sfi = TSphereItType(m_InputPtr, spatialFunc, seedIndex);

    // Walk the spatial function
    for( sfi.GoToBegin(); !( sfi.IsAtEnd() ); ++sfi)
      {
      // The iterator for accessing linked list info
      itk::BloxBoundaryPointPixel<NDimensions>::const_iterator bpiterator;

      // Walk through all of the elements at the pixel
      for (bpiterator = sfi.Get().begin(); bpiterator != sfi.Get().end(); ++bpiterator)
        {
        // Get the pointer of the blox
        TBPItemType* pBPTwo = *bpiterator;

        // Get the physical positions of the two boundary points
        TPositionType P1 = pBPOne->GetPhysicalPosition();
        TPositionType P2 = pBPTwo->GetPhysicalPosition();

        // Form the two vectors between them
        TVectorType C12 = P2 - P1;

        // If we don't meet distance criteria, move on
        if(!( (C12.GetNorm() > m_DistanceMin) && (C12.GetNorm() < m_DistanceMax) ) )
          continue;

        TVectorType C21 = P1 - P2;

        C12 = C12 / C12.GetNorm();
        C21 = C21 / C21.GetNorm();

        // Get the gradients of the two boundary points
        TGradientType G1 = pBPOne->GetGradient();
        TGradientType G2 = pBPTwo->GetGradient();

        G1 = G1 / G1.GetNorm();
        G2 = G2 / G2.GetNorm();

        // Calculate face-to-faceness
        double faceToFaceness = dot_product(G1.Get_vnl_vector(), C12.Get_vnl_vector() ) *
          dot_product(G2.Get_vnl_vector(), C21.Get_vnl_vector() );

        // If face-to-faceness meets threshold criteria
        if( faceToFaceness > (1.0 - m_Epsilon) )
          {
          // Figure out the center of the core atom
          TPositionType coreAtomCenter = P1 + (P2 - P1) / 2;

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
}

} // end namespace

#endif
