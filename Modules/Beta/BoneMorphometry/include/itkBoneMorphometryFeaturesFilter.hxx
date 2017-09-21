/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkBoneMorphometryFeaturesFilter_hxx
#define itkBoneMorphometryFeaturesFilter_hxx

#include "itkBoneMorphometryFeaturesFilter.h"

#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{
template< typename TInputImage, typename TMaskImage >
BoneMorphometryFeaturesFilter< TInputImage, TMaskImage >
::BoneMorphometryFeaturesFilter():
    m_Threshold(1),
    m_Pp(0),
    m_Pl(0),
    m_PlX(0),
    m_PlY(0),
    m_PlZ(0),
    m_NumVoxelsInsideMask(0),
    m_NumBoneVoxels(0)
{
  this->SetNumberOfRequiredInputs( 1 );
}

template< typename TInputImage, typename TMaskImage >
void
BoneMorphometryFeaturesFilter< TInputImage, TMaskImage >
::AllocateOutputs()
{
  // Pass the input through as the output
  InputImagePointer image =
    const_cast< TInputImage * >( this->GetInput() );

  this->GraftOutput(image);

  // Nothing that needs to be allocated for the remaining outputs
}

template< typename TInputImage, typename TMaskImage >
void
BoneMorphometryFeaturesFilter< TInputImage, TMaskImage >
::BeforeThreadedGenerateData()
{
    ThreadIdType numberOfThreads = this->GetNumberOfThreads();

    m_Pp = 0;
    m_Pl = 0;
    m_PlX = 0;
    m_PlY = 0;
    m_PlZ = 0;

    // Resize the thread temporaries
    m_NumVoxelsInsideMask.SetSize(numberOfThreads);
    m_NumBoneVoxels.SetSize(numberOfThreads);
    m_NumX.SetSize(numberOfThreads);
    m_NumY.SetSize(numberOfThreads);
    m_NumZ.SetSize(numberOfThreads);
    m_NumXO.SetSize(numberOfThreads);
    m_NumYO.SetSize(numberOfThreads);
    m_NumZO.SetSize(numberOfThreads);

    // Resize the thread temporaries
    m_NumVoxelsInsideMask.Fill(0);
    m_NumBoneVoxels.Fill(0);
    m_NumX.Fill(0);
    m_NumY.Fill(0);
    m_NumZ.Fill(0);
    m_NumXO.Fill(0);
    m_NumYO.Fill(0);
    m_NumZO.Fill(0);
}

template< typename TInputImage, typename TMaskImage >
void
BoneMorphometryFeaturesFilter< TInputImage, TMaskImage >
::AfterThreadedGenerateData()
{
    ThreadIdType numberOfThreads = this->GetNumberOfThreads();

    SizeValueType numVoxels = 0;
    SizeValueType numBoneVoxels = 0;
    SizeValueType numX = 0;
    SizeValueType numY = 0;
    SizeValueType numZ = 0;
    SizeValueType numXO = 0;
    SizeValueType numYO = 0;
    SizeValueType numZO = 0;

    for (unsigned int i = 0; i < numberOfThreads; ++i )
    {
      numVoxels += m_NumVoxelsInsideMask[i];
      numBoneVoxels += m_NumBoneVoxels[i];
      numX += m_NumX[i];
      numY += m_NumY[i];
      numZ += m_NumZ[i];
      numXO += m_NumXO[i];
      numYO += m_NumYO[i];
      numZO += m_NumZO[i];
    }

    typename TInputImage::SpacingType inSpacing = this->GetInput()->GetSpacing();
    m_Pp = (RealType)numBoneVoxels / (RealType)numVoxels;
    m_PlX = (RealType) ((numX+numXO)/2.0) / (RealType) (numVoxels * inSpacing[0]) * 2;
    m_PlY = (RealType) ((numY+numYO)/2.0) / (RealType) (numVoxels * inSpacing[1]) * 2;
    m_PlZ = (RealType) ((numZ+numZO)/2.0) / (RealType) (numVoxels * inSpacing[2]) * 2;
    m_Pl =(m_PlX + m_PlY + m_PlZ) / 3.0;
}

template< typename TInputImage, typename TMaskImage >
void
BoneMorphometryFeaturesFilter< TInputImage, TMaskImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
    NeighborhoodRadiusType radius;
    radius.Fill(1);
    NeighborhoodOffsetType offsetX = {{0,0,1}};
    NeighborhoodOffsetType offsetXO = {{0,0,-1}};
    NeighborhoodOffsetType offsetY = {{0,1,0}};
    NeighborhoodOffsetType offsetYO = {{0,-1,0}};
    NeighborhoodOffsetType offsetZ = {{1,0,0}};
    NeighborhoodOffsetType offsetZO = {{-1,0,0}};

    SizeValueType numVoxels = 0;
    SizeValueType numBoneVoxels = 0;
    SizeValueType numX = 0;
    SizeValueType numY = 0;
    SizeValueType numZ = 0;
    SizeValueType numXO = 0;
    SizeValueType numYO = 0;
    SizeValueType numZO = 0;

    MaskImagePointer maskPointer = TMaskImage::New();
    maskPointer = const_cast<TMaskImage*>(this->GetMaskImage());

    NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage > boundaryFacesCalculator;
    typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::FaceListType
    faceList = boundaryFacesCalculator( this->GetInput(), outputRegionForThread, radius );
    typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::FaceListType::iterator fit = faceList.begin();

    for (; fit != faceList.end(); ++fit )
    {
      NeighborhoodIteratorType inputNIt(radius, this->GetInput(), *fit );
      BoundaryConditionType  BoundaryCondition;
      inputNIt.SetBoundaryCondition(BoundaryCondition);
      inputNIt.GoToBegin();

      while( !inputNIt.IsAtEnd() )
      {
        if( maskPointer && maskPointer->GetPixel( inputNIt.GetIndex() ) == 0 )
        {
          ++inputNIt;
          continue;
        }

        ++numVoxels;

        if( inputNIt.GetCenterPixel() >= m_Threshold )
        {

          ++numBoneVoxels;

          if( inputNIt.GetPixel(offsetX) < m_Threshold )
          {
            ++numXO;
          }
          if( inputNIt.GetPixel(offsetXO) < m_Threshold )
          {
            ++numX;
          }
          if( inputNIt.GetPixel(offsetY) < m_Threshold )
          {
            ++numYO;
          }
          if( inputNIt.GetPixel(offsetYO) < m_Threshold )
          {
            ++numY;
          }
          if( inputNIt.GetPixel(offsetZ) < m_Threshold )
          {
            ++numZO;
          }
          if( inputNIt.GetPixel(offsetZO) < m_Threshold )
          {
            ++numZ;
          }
        }

        ++inputNIt;
      }
    }

    m_NumVoxelsInsideMask[threadId] = numVoxels;
    m_NumBoneVoxels[threadId] = numBoneVoxels;
    m_NumX[threadId] = numX;
    m_NumY[threadId] = numY;
    m_NumZ[threadId] = numZ;
    m_NumXO[threadId] = numXO;
    m_NumYO[threadId] = numYO;
    m_NumZO[threadId] = numZO;
}

template< typename TInputImage, typename TMaskImage >
void
BoneMorphometryFeaturesFilter< TInputImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  SizeValueType numVoxels = 0;
  SizeValueType numBoneVoxels = 0;
  SizeValueType numX = 0;
  SizeValueType numY = 0;
  SizeValueType numZ = 0;
  SizeValueType numXO = 0;
  SizeValueType numYO = 0;
  SizeValueType numZO = 0;

  for (unsigned int i = 0; i < m_NumVoxelsInsideMask.GetSize(); ++i )
  {
    numVoxels += m_NumVoxelsInsideMask[i];
    numBoneVoxels += m_NumBoneVoxels[i];
    numX += m_NumX[i];
    numY += m_NumY[i];
    numZ += m_NumZ[i];
    numXO += m_NumXO[i];
    numYO += m_NumYO[i];
    numZO += m_NumZO[i];
  }
  os << indent << "m_Threshold: " << m_Threshold << std::endl;

  Superclass::PrintSelf(os, indent);
  os << indent << "m_Threshold: " << m_Threshold << std::endl;
  os << indent << "m_Pp: " << m_Pp << std::endl;
  os << indent << "m_Pl: " << m_Pl << std::endl;
  os << indent << "m_PlX: " << m_PlX << std::endl;
  os << indent << "m_PlY: " << m_PlY << std::endl;
  os << indent << "m_PlZ: " << m_PlZ << std::endl;
  os << indent << "m_NumVoxelsInsideMask: " << m_NumVoxelsInsideMask << std::endl;
  os << indent << "NumVoxelsInsideMask total: " << numVoxels << std::endl;
  os << indent << "m_NumBoneVoxels: " << m_NumBoneVoxels << std::endl;
  os << indent << "NumBoneVoxels: " << numBoneVoxels << std::endl;
  os << indent << "m_NumX: " << m_NumX << std::endl;
  os << indent << "NumX total: " << numX << std::endl;
  os << indent << "m_NumY: " << m_NumY << std::endl;
  os << indent << "NumY total: " << numY << std::endl;
  os << indent << "m_NumZ: " << m_NumZ << std::endl;
  os << indent << "NumZ total: " << numZ << std::endl;
  os << indent << "m_NumXO: " << m_NumXO << std::endl;
  os << indent << "NumXO total: " << numXO << std::endl;
  os << indent << "m_NumYO: " << m_NumYO << std::endl;
  os << indent << "NumYO total: " << numYO << std::endl;
  os << indent << "m_NumZO: " << m_NumZO << std::endl;
  os << indent << "NumZO total: " << numZO << std::endl;

}
} // end namespace itk

#endif // itkBoneMorphometryFeaturesFilter_hxx
