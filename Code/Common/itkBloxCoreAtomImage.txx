/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxCoreAtomImage_txx
#define __itkBloxCoreAtomImage_txx

#include <iostream>
#include "itkImageRegionIterator.h"
#include "itkConicShellInteriorExteriorSpatialFunction.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

namespace itk
{

template<class TBoundaryPointImage, class TImageTraits>
BloxCoreAtomImage<TBoundaryPointImage, TImageTraits>
::BloxCoreAtomImage()
{
  m_BoundaryPointImage = NULL;

  m_NumCoreAtoms = 0;
  m_DistanceMin = 8;
  m_DistanceMax = 12;
  m_Epsilon = 0;
  m_Polarity = 0;
}

template<class TBoundaryPointImage, class TImageTraits>
BloxCoreAtomImage<TBoundaryPointImage, TImageTraits>
::~BloxCoreAtomImage()
{

}

template<class TBoundaryPointImage, class TImageTraits>
void
BloxCoreAtomImage<TBoundaryPointImage, TImageTraits>
::FindCoreAtoms()
{
  itkDebugMacro(<< "BloxCoreAtomImage::FindCoreAtoms() called");

  // Make sure we're getting everything
  m_BoundaryPointImage->SetRequestedRegionToLargestPossibleRegion();

  // Create an iterator to walk the source image
  typedef ImageRegionIterator<TBoundaryPointImage> TImageIteratorType;

  TImageIteratorType imageIt = TImageIteratorType(m_BoundaryPointImage,
                                                  m_BoundaryPointImage->GetRequestedRegion() );

  // Iterate through the entire image (all pixels) and look for core atoms
  for ( imageIt.GoToBegin(); !imageIt.IsAtEnd(); ++imageIt)
    {
    // The iterator for accessing linked list info
    itk::BloxBoundaryPointPixel<NDimensions>::iterator bpiterator;

    // Walk through all of the elements at the pixel
    for (bpiterator = imageIt.Value().begin(); bpiterator != imageIt.Value().end(); ++bpiterator)
      {
      this->FindCoreAtomsAtBoundaryPoint( *bpiterator );
      }
    }

  // Compute mean core atom diameter
  itk::ImageRegionIterator<Self> bloxIt = 
    itk::ImageRegionIterator<Self>(this, this->GetLargestPossibleRegion() );

  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    ( &bloxIt.Value() )->CalcMeanCoreAtomDiameter();
    }

  itkDebugMacro(<< "Finished looking for core atoms\n"
                << "I found " << m_NumCoreAtoms << " core atoms\n");
}

template<class TBoundaryPointImage, class TImageTraits>
void
BloxCoreAtomImage<TBoundaryPointImage, TImageTraits>
::FindCoreAtomsAtBoundaryPoint(BloxBoundaryPointItem<NDimensions>* pBPOne)
{
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
  typename TBoundaryPointImage::IndexType seedIndex;

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
  if( this->TransformPhysicalPointToIndex(seedPos, seedIndex) )
    {
    // Create and initialize a spatial function iterator
    typedef itk::FloodFilledSpatialFunctionConditionalIterator<TBoundaryPointImage, TFunctionType> TSphereItType;
    TSphereItType sfi = TSphereItType(m_BoundaryPointImage, spatialFunc, seedIndex);

    // Walk the spatial function
    for( ; !( sfi.IsAtEnd() ); ++sfi)
      {
      // The iterator for accessing linked list info
      itk::BloxBoundaryPointPixel<NDimensions>::iterator bpiterator;

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
          
          this->TransformPhysicalPointToIndex(coreAtomCenter, coreAtomPos);
         
          // Store the new core atom in the correct spot
          this->GetPixel(coreAtomPos).push_back(pCoreAtom);

          m_NumCoreAtoms++;

          } // end if face-to-faceness meets criteria
        } // end iterate through boundary points in pixel
      } // end iterate through the conic shell
   } // end if the seed position for the conic shell is in the image
}

template<class TBoundaryPointImage, class TImageTraits>
void
BloxCoreAtomImage<TBoundaryPointImage, TImageTraits>::
DoEigenanalysis()
{
  itk::ImageRegionIterator<Self> bloxIt = 
    itk::ImageRegionIterator<Self>(this, this->GetLargestPossibleRegion() );

  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
      ( &bloxIt.Value() )->DoCoreAtomEigenanalysis();
    }
}

template<class TBoundaryPointImage, class TImageTraits>
void
BloxCoreAtomImage<TBoundaryPointImage, TImageTraits>::
DoCoreAtomVoting()
{
  // Iterator to access all pixels in the image
  ImageRegionIterator<Self> bloxIt = 
    ImageRegionIterator<Self>(this, this->GetLargestPossibleRegion() );

  // Pointer for accessing pixels
  BloxCoreAtomPixel<NDimensions>* pPixel = 0;

  // Results of eigenanalysis from each pixel
  BloxCoreAtomPixel<NDimensions>::TEigenvalueType eigenvalues;
  BloxCoreAtomPixel<NDimensions>::TEigenvectorType eigenvectors;

  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    // Get a pointer to the pixel
    pPixel = &bloxIt.Value();

    // If there are no core atoms in this pixel, it doesn't get to vote
    if( pPixel->empty() )
      continue;

    // Get eigenanalysis results
    eigenvalues = pPixel->GetEigenvalues();
    eigenvectors = pPixel->GetEigenvectors();

    // Ellipsoid axis length array
    Point<double, NDimensions> axisLengthArray;

    // Compute first length
    axisLengthArray[0] = 0.5 * pPixel->GetMeanCoreAtomDiameter();

    printf("Mean core atom diameter is %f\n", pPixel->GetMeanCoreAtomDiameter() );

    // Precompute alphaOne
    double alphaOne = 1 - eigenvalues[0];

    // Watch out for /0 problems
    if(alphaOne==0)
      alphaOne = 0.001;

    // Now compute the rest of the lengths
    for(int i = 1; i < NDimensions; i++)
      {
      axisLengthArray[i] = ( (1 - eigenvalues[i]) / alphaOne) * axisLengthArray[0];
      }

    // Dump the axis length vector
    for(int i = 0; i < NDimensions; i++)
      {
      printf("Axis length %i is %f\n", i, axisLengthArray[i]);
      }

    // Build the ellipsoid voting region
    typedef EllipsoidInteriorExteriorSpatialFunction<double, NDimensions> TVoteFunctionType;
    TVoteFunctionType::Pointer ellipsoid = TVoteFunctionType::New();

    ellipsoid->SetOrientations(eigenvectors);
    ellipsoid->SetAxes(axisLengthArray);

    // Create an iterator to traverse the ellipsoid region
    typedef FloodFilledSpatialFunctionConditionalIterator
      <Self, TVoteFunctionType> TItType;

    // The seed position for the ellipsoid is the current pixel's index in data space
    // since this is always at the center of the voting ellipsoid
    Self::IndexType seedPos = bloxIt.GetIndex();
    
    // Instantiate the iterator
    TItType sfi = TItType(this, ellipsoid, seedPos);

    // Iterate through the ellipsoid and cast votes
    for( ; !( sfi.IsAtEnd() ); ++sfi)
      {

      }
    }
}

template<class TBoundaryPointImage, class TImageTraits>
void
BloxCoreAtomImage<TBoundaryPointImage, TImageTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Number of core atoms: " << m_NumCoreAtoms << std::endl;

  unsigned int i;
  os << indent << "Boundary point image origin: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_BPImageOrigin[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Boundary point image spacing: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_BPImageSpacing[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Minimum core atom search distance: " << m_DistanceMin << std::endl;
  os << indent << "Maximum core atom search distance: " << m_DistanceMax << std::endl;
  os << indent << "Core atom search epsilon: " << m_Epsilon << std::endl;
  os << indent << "Core atom search polarity: " << m_Polarity << std::endl;
}

} // end namespace itk

#endif
