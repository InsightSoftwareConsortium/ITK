/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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

template< typename TInputImage, typename TOutputImage, typename TSourceImage >
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TInputImage, TOutputImage, TSourceImage >
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


template< typename TInputImage, typename TOutputImage, typename TSourceImage >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TInputImage, TOutputImage, TSourceImage >
::SetInput1(const SourceImageType * image1 ) 
{
  // Process object is not const-correct so the const casting is required.
  SetNthInput(1,  const_cast<SourceImageType *>( image1 ) );
}

template< typename TInputImage, typename TOutputImage, typename TSourceImage >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TInputImage, TOutputImage, TSourceImage >
::SetInput2(const InputImageType * image2 ) 
{
  // Process object is not const-correct so the const casting is required.
  SetNthInput(0, const_cast<InputImageType *>( image2 ) );
}


template< typename TInputImage, typename TOutputImage, typename TSourceImage >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TInputImage, TOutputImage, TSourceImage >
::GenerateInputRequestedRegion()
{
  itkDebugMacro(<< "BloxBoundaryProfileImageToBloxCoreAtomImageFilter::GenerateInputRequestedRegion() called");
  
  Superclass::GenerateInputRequestedRegion();
}


template< typename TInputImage, typename TOutputImage, typename TSourceImage >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TInputImage, TOutputImage, TSourceImage >
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

template< typename TInputImage, typename TOutputImage, typename TSourceImage >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TInputImage, TOutputImage, TSourceImage >
::FindCoreAtoms()
{
  // Create an iterator to walk the source image
  typedef ImageRegionConstIterator<InputImageType> ImageIteratorType;

  ImageIteratorType imageIt ( m_BoundaryProfileImagePtr,
                              m_BoundaryProfileImagePtr->GetRequestedRegion() );

  // Iterate through the entire image (all pixels) and look for core atoms
  for ( imageIt.GoToBegin(); !imageIt.IsAtEnd(); ++imageIt)
    {
    // The iterator for accessing linked list info
    typename BloxBoundaryProfilePixel<NDimensions>::const_iterator bpiterator;

    // Walk through all of the elements at the pixel
    for (bpiterator = imageIt.Value().begin(); bpiterator != imageIt.Value().end(); ++bpiterator)
      {
      this->FindCoreAtomsAtBoundaryPoint( *bpiterator );
      }
    }

  // Compute mean core atom diameter
  ImageRegionIterator<TOutputImage> bloxIt = 
    ImageRegionIterator<TOutputImage>(m_OutputPtr, m_OutputPtr->GetRequestedRegion() );

  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    ( &bloxIt.Value() )->CalcMeanCoreAtomDiameter();
    }  
}

template< typename TInputImage, typename TOutputImage, typename TSourceImage >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TInputImage, TOutputImage, TSourceImage >
::FindCoreAtomsAtBoundaryPoint(BloxProfileItemType* pBPOne)
{
  //typedef BloxBoundaryPointItemBase<NDimensions> BPItemType;

  typedef BloxBoundaryPointItem<NDimensions> BPointItemType;

  typedef BloxBoundaryProfileItem<NDimensions> BProfileItemType;

  // When looking for core atoms at a boundary point, we want to examine
  // all of the boundary points within blox that are part of a conical
  // region extending out in the direction of the gradient of the boundary
  // point.

  //---------Create and initialize a conic shell spatial function-----------
  typedef ConicShellInteriorExteriorSpatialFunction<NDimensions> FunctionType;
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
    spatialFunctionOriginVector.SetVnlVector( spatialFunctionOrigin.GetVnlVector() );

    // Set the gradient of the conic shell to the current boundary point gradient
    FunctionGradientType spatialFunctionGradient = pBPOne->GetGradient2();
    spatialFunc->SetOriginGradient(spatialFunctionGradient);

    // Create a seed position for the spatial function iterator we'll use shortly
    SourceImageIndexType seedIndex;

    // Normalize the origin gradient
    VectorType seedVector;
    seedVector.SetVnlVector(spatialFunctionGradient.GetVnlVector());
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
      typedef FloodFilledSpatialFunctionConditionalConstIterator<InputImageType, FunctionType> SphereItType;
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
          GradientType G1 = ( (BProfileItemType*)pBPOne )->GetGradient2();
          GradientType G2 = ( (BProfileItemType*)pBPTwo )->GetGradient2();

          if(G1.GetNorm()>0)
            {
            G1 = G1 / G1.GetNorm();
            }
          
        
          if(G2.GetNorm()>0)
            {
            G2 = G2 / G2.GetNorm();
            }

          // Calculate face-to-faceness
          double faceness1 = dot_product(G1.GetVnlVector(), C12.GetVnlVector() );
          double faceness2 = dot_product(G2.GetVnlVector(), C21.GetVnlVector() );
          double faceToFaceness = faceness1 * faceness2;

          // If face-to-faceness meets threshold criteria
          if( faceToFaceness > (1.0 - m_Epsilon) )
            {
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

            //homogeneous core profiles

            if(faceness1 <= 0 && faceness2 <= 0)
              {
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
              {
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
              {
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
              {
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
              OutputImageIndexType coreAtomPos;
          
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

template< typename TInputImage, typename TOutputImage, typename TSourceImage >
void
BloxBoundaryProfileImageToBloxCoreAtomImageFilter< TInputImage, TOutputImage, TSourceImage >
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
