/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter_txx
#define __itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter_txx

#include "itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkConicShellInteriorExteriorSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalConstIterator.h"

namespace itk
{

template< typename TSourceImage, unsigned int dim >
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TSourceImage, dim >
::BloxBoundaryProfileImageToBloxCoreAtomImageFilter()
{
  itkDebugMacro(<< "BloxBoundaryProfileImageToBloxCoreAtomImageFilter::BloxBoundaryProfileImageToBloxCoreAtomImageFilter() called");
  m_DistanceMin = 8;
  m_DistanceMax = 12;
  m_Epsilon = 0;
  m_Polarity = 0;
  m_IntensityThreshold = 10;
  m_CoreAtomsCreated = 0;
}


template< typename TSourceImage, unsigned int dim >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TSourceImage, dim >
::SetInput1(const SourceImageType * image1 ) 
{
  // Process object is not const-correct so the const casting is required.
  SetNthInput(1,  const_cast<SourceImageType *>( image1 ) );
}

template< typename TSourceImage, unsigned int dim >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TSourceImage, dim >
::SetInput2(const BoundaryProfileImageType * image2 ) 
{
  // Process object is not const-correct so the const casting is required.
  SetNthInput(0, const_cast<BoundaryProfileImageType *>( image2 ) );
}


template< typename TSourceImage, unsigned int dim >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TSourceImage, dim >
::GenerateInputRequestedRegion()
{
  itkDebugMacro(<< "BloxBoundaryProfileImageToBloxCoreAtomImageFilter::GenerateInputRequestedRegion() called");
  
  Superclass::GenerateInputRequestedRegion();
}


template< typename TSourceImage, unsigned int dim >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TSourceImage, dim >
::GenerateData()
{
  std::cerr << "BloxBoundaryProfileImageToBloxCoreAtomImageFilter: GenerateData()" << std::endl;
  itkDebugMacro(<< "BloxBoundaryProfileImageToBloxCoreAtomImageFilter::GenerateData() called");

  // Get the input and output pointers
  m_BoundaryProfileImagePtr  = this->GetInput(0);
  m_SourceImagePtr = dynamic_cast<SourceImageType*>(ProcessObject::GetInput(1));
  m_OutputPtr = this->GetOutput(0);

  // Allocate the output
  m_OutputPtr->SetBufferedRegion( m_OutputPtr->GetRequestedRegion() );
  m_OutputPtr->Allocate();
  
  // To avoid appending data, empty the output image
  m_OutputPtr->EmptyImage();

  std::cerr << "BloxBoundaryProfileImageToBloxCoreAtomImageFilter: GenerateData() : Finding Core Atoms" << std::endl;

  this->FindCoreAtoms();
}

template< typename TSourceImage, unsigned int dim >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TSourceImage, dim >
::FindCoreAtoms()
{
  // Create an iterator to walk the source image
  typedef ImageRegionConstIterator<BoundaryProfileImageType> ImageIteratorType;

  ImageIteratorType imageIt ( m_BoundaryProfileImagePtr,
                              m_BoundaryProfileImagePtr->GetRequestedRegion() );

  // Iterate through the entire image (all pixels) and look for core atoms
  for ( imageIt.GoToBegin(); !imageIt.IsAtEnd(); ++imageIt)
    {
    // The iterator for accessing linked list info
    typename itk::BloxBoundaryProfilePixel<dim>::const_iterator bpiterator;

    // Walk through all of the elements at the pixel
    for (bpiterator = imageIt.Value().begin(); bpiterator != imageIt.Value().end(); ++bpiterator)
      {
      this->FindCoreAtomsAtBoundaryPoint( *bpiterator );
      }
    }

  std::cout << "Core Atoms Created: " << m_CoreAtomsCreated << std::endl;

  // Compute mean core atom diameter
  itk::ImageRegionIterator<TOutputImage> bloxIt = 
    itk::ImageRegionIterator<TOutputImage>(m_OutputPtr, m_OutputPtr->GetRequestedRegion() );

  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    ( &bloxIt.Value() )->CalcMeanCoreAtomDiameter();
    }  
}

template< typename TSourceImage, unsigned int dim >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TSourceImage, dim >
::FindCoreAtomsAtBoundaryPoint(BloxBoundaryProfileItem<dim>* pBPOne)
{
  //typedef BloxBoundaryPointItemBase<NDimensions> BPItemType;

  typedef BloxBoundaryPointItem<NDimensions> BPointItemType;

  typedef BloxBoundaryProfileItem<NDimensions> BProfileItemType;

  // When looking for core atoms at a boundary point, we want to examine
  // all of the boundary points within blox that are part of a conical
  // region extending out in the direction of the gradient of the boundary
  // point.

  //---------Create and initialize a conic shell spatial function-----------
  typedef typename itk::ConicShellInteriorExteriorSpatialFunction<NDimensions> FunctionType;
  typedef typename FunctionType::GradientType FunctionGradientType;

  typename  FunctionType::Pointer spatialFunc = FunctionType::New();

  for(int i=0;i<2;++i)
  {
  // Set the properties of the conic shell
  spatialFunc->SetDistanceMin(m_DistanceMin);
  spatialFunc->SetDistanceMax(m_DistanceMax);
  spatialFunc->SetEpsilon(m_Epsilon);
  spatialFunc->SetPolarity(m_Polarity);
  if(i==1 && m_Polarity == true)m_Polarity = false;
  else if(i==1 && m_Polarity == false)m_Polarity = true;
  spatialFunc->SetPolarity(m_Polarity);

  // Set the origin of the conic shell to the current boundary point location
  PositionType spatialFunctionOrigin = pBPOne->GetOptimalBoundaryLocation();
  spatialFunc->SetOrigin(spatialFunctionOrigin);

  // Covert the origin position to a vector
  //VectorType spatialFunctionOriginVector = spatialFunctionOrigin.GetVectorFromOrigin();

  VectorType spatialFunctionOriginVector;
  spatialFunctionOriginVector.Set_vnl_vector( spatialFunctionOrigin.Get_vnl_vector() );

  // Set the gradient of the conic shell to the current boundary point gradient
  FunctionGradientType spatialFunctionGradient = pBPOne->GetGradient();
  spatialFunc->SetOriginGradient(spatialFunctionGradient);

  // Create a seed position for the spatial function iterator we'll use shortly
  typename BoundaryProfileImageType::IndexType seedIndex;

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
  if( m_BoundaryProfileImagePtr->TransformPhysicalPointToIndex(seedPos, seedIndex) )
    {
    // Create and initialize a spatial function iterator
    typedef FloodFilledSpatialFunctionConditionalConstIterator<BoundaryProfileImageType, FunctionType> SphereItType;
    SphereItType sfi = SphereItType(m_BoundaryProfileImagePtr, spatialFunc, seedIndex);

    // Walk the spatial function
    for( sfi.GoToBegin(); !( sfi.IsAtEnd() ); ++sfi)
      {
      // The iterator for accessing linked list info
      typename BloxBoundaryProfilePixel<NDimensions>::const_iterator bpiterator;

      // Walk through all of the elements at the pixel
      for (bpiterator = sfi.Get().begin(); bpiterator != sfi.Get().end(); ++bpiterator)
        {
        // Get the pointer of the blox
        BProfileItemType* pBPTwo = *bpiterator;

        // Get the physical positions of the two boundary profiles
        PositionType P1 = ( (BProfileItemType*)pBPOne )->GetOptimalBoundaryLocation();
        PositionType P2 = ( (BProfileItemType*)pBPTwo )->GetOptimalBoundaryLocation();

        // Form the two vectors between them
        VectorType C12 = P2 - P1;

        // If we don't meet distance criteria, move on
        if(!( (C12.GetNorm() > m_DistanceMin) && (C12.GetNorm() < m_DistanceMax) ) )
          continue;

        VectorType C21 = P1 - P2;

        C12 = C12 / C12.GetNorm();
        C21 = C21 / C21.GetNorm();

        // Get the gradients of the two boundary points
        GradientType G1 = ( (BProfileItemType*)pBPOne )->GetGradient();
        GradientType G2 = ( (BProfileItemType*)pBPTwo )->GetGradient();

        G1 = G1 / G1.GetNorm();
        G2 = G2 / G2.GetNorm();

        // Calculate face-to-faceness
        double faceness1 = dot_product(G1.Get_vnl_vector(), C12.Get_vnl_vector() );
        double faceness2 = dot_product(G2.Get_vnl_vector(), C21.Get_vnl_vector() );
        double faceToFaceness = faceness1 * faceness2;

        //std::cout << "faceToFaceness = " << faceToFaceness << std::endl;

        // If face-to-faceness meets threshold criteria
        if( faceToFaceness > (1.0 - m_Epsilon) )
        {//std::cout << "0" << std::endl;
          // Figure out the center of the core atom
          PositionType coreAtomCenter = P1 + (P2 - P1) / 2;
          SourceImageIndexType centerIndex;
          SourceImageIndexType centerIndexTemp;
          m_SourceImagePtr->TransformPhysicalPointToIndex(coreAtomCenter, centerIndex);
          m_SourceImagePtr->TransformPhysicalPointToIndex(coreAtomCenter, centerIndexTemp);

          // Get the average pixel intensity in the pixels surrounding the center of the core atom
          float coreCenterIntensityAvg = (float)m_SourceImagePtr->GetPixel(centerIndexTemp);

          centerIndexTemp[0] = centerIndex[0]+1;
          coreCenterIntensityAvg += (float)m_SourceImagePtr->GetPixel(centerIndexTemp);
          centerIndexTemp[0] = centerIndex[0]-1;
          coreCenterIntensityAvg += (float)m_SourceImagePtr->GetPixel(centerIndexTemp);
          centerIndexTemp[0] = centerIndex[0];
          centerIndexTemp[1] = centerIndex[1]+1;
          coreCenterIntensityAvg += (float)m_SourceImagePtr->GetPixel(centerIndexTemp);
          centerIndexTemp[1] = centerIndex[1]-1;
          coreCenterIntensityAvg += (float)m_SourceImagePtr->GetPixel(centerIndexTemp);
          centerIndexTemp[1] = centerIndex[1];
          centerIndexTemp[2] = centerIndex[2]+1;
          coreCenterIntensityAvg += (float)m_SourceImagePtr->GetPixel(centerIndexTemp);
          centerIndexTemp[2] = centerIndex[2]-1;
          coreCenterIntensityAvg += (float)m_SourceImagePtr->GetPixel(centerIndexTemp);

          coreCenterIntensityAvg /= 7;

         // std::cout << "coreCenterIntensityAvg = " << coreCenterIntensityAvg << std::endl;

          //homogeneous core profiles

          if(faceness1 <= 0 && faceness2 <= 0)
          {  //std::cout << "1";
            float temp = ( pBPOne->GetLowerIntensity() + pBPTwo->GetLowerIntensity() ) * 0.5;
            if( (temp >= coreCenterIntensityAvg - m_IntensityThreshold) && 
                (temp <= coreCenterIntensityAvg + m_IntensityThreshold) )
                m_IntensityFlag = 1;
            else m_IntensityFlag = 0;

            if( fabs( (pBPOne->GetLowerIntensity() - pBPTwo->GetLowerIntensity()) ) <= m_IntensityThreshold
                && m_IntensityFlag == true )
              m_CreateCoreAtom = true;
            else
              m_CreateCoreAtom = false;
          }
          else if(faceness1 <= 0 && faceness2 > 0)
          {//std::cout << "2";
            float temp = ( pBPOne->GetLowerIntensity() + pBPTwo->GetUpperIntensity() ) * 0.5;
            if( (temp >= coreCenterIntensityAvg - m_IntensityThreshold) && 
                (temp <= coreCenterIntensityAvg + m_IntensityThreshold) )
                m_IntensityFlag = 1;
            else m_IntensityFlag = 0;

            if( fabs( (pBPOne->GetLowerIntensity() - pBPTwo->GetUpperIntensity()) ) <= m_IntensityThreshold
                && m_IntensityFlag == true )
              m_CreateCoreAtom = true;
            else
              m_CreateCoreAtom = false;
          }
          else if(faceness1 > 0 && faceness2 <= 0)
          {//std::cout << "3";
            float temp = ( pBPOne->GetUpperIntensity() + pBPTwo->GetLowerIntensity() ) * 0.5;
            if( (temp >= coreCenterIntensityAvg - m_IntensityThreshold) && 
                (temp <= coreCenterIntensityAvg + m_IntensityThreshold) )
                m_IntensityFlag = 1;
            else m_IntensityFlag = 0;

            if( fabs( (pBPOne->GetUpperIntensity() - pBPTwo->GetLowerIntensity()) ) <= m_IntensityThreshold
                && m_IntensityFlag == true )
              m_CreateCoreAtom = true;
            else
              m_CreateCoreAtom = false;
          }
          else if(faceness1 > 0 && faceness2 > 0)
          {//std::cout << "4";
            float temp = ( pBPOne->GetUpperIntensity() + pBPTwo->GetUpperIntensity() ) * 0.5;
            if( (temp >= coreCenterIntensityAvg - m_IntensityThreshold) && 
                (temp <= coreCenterIntensityAvg + m_IntensityThreshold) )
                m_IntensityFlag = 1;
            else m_IntensityFlag = 0;

            if( fabs( (pBPOne->GetUpperIntensity() - pBPTwo->GetUpperIntensity()) ) <= m_IntensityThreshold
                && m_IntensityFlag == true )
              m_CreateCoreAtom = true;
            else
              m_CreateCoreAtom = false;
          }
          else
          {
            std::cerr << "BloxBoundaryProfileImageToBloxCoreAtomImageFilter::FindCoreAtomsAtBoundaryPoint: Invalid Homogeneous Condition" << std::endl;
            m_CreateCoreAtom = false;
          }

          //===== CREATE CORE ATOM =====

          if(m_CreateCoreAtom)
          {
            m_CoreAtomsCreated++;

            // Figure out the diameter of the core atom
            double coreAtomDiameter = (P2-P1).GetNorm();

            // Create a new core atom
            BloxCoreAtomItem<NDimensions>* pCoreAtom = new BloxCoreAtomItem<NDimensions>;
          
            // Set its boundary points, center, and diameter
            pCoreAtom->SetBoundaryPointA( (BPointItemType*)pBPOne );
            pCoreAtom->SetBoundaryPointB( (BPointItemType*)pBPTwo );
            pCoreAtom->SetCenterPosition(coreAtomCenter);
            pCoreAtom->SetDiameter(coreAtomDiameter);

            // Figure out the data space coordinates of the center
            IndexType coreAtomPos;
          
            m_OutputPtr->TransformPhysicalPointToIndex(coreAtomCenter, coreAtomPos);
         
            // Store the new core atom in the correct spot
            m_OutputPtr->GetPixel(coreAtomPos).push_back(pCoreAtom);
          }

          } // end if face-to-faceness meets criteria
        } // end iterate through boundary points in pixel
      } // end iterate through the conic shell
    } // end if the seed position for the conic shell is in the image
  }
}

template< typename TSourceImage, unsigned int dim >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TSourceImage, dim >
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
