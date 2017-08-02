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
#ifndef itkBoneMorphometryImageFilter_hxx
#define itkBoneMorphometryImageFilter_hxx

#include "itkBoneMorphometryImageFilter.h"

#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{
template< typename TInputImage >
BoneMorphometryImageFilter< TInputImage >
::BoneMorphometryImageFilter():
    m_Threshold(1),
    m_Pp(0),
    m_Pl(0),
    m_PlX(0),
    m_PlY(0),
    m_PlZ(0),
    m_NumVoxels(0),
    m_NumBoneVoxels(0)
{
  this->SetNumberOfRequiredInputs( 1 );
}


template< typename TInputImage >
void
BoneMorphometryImageFilter< TInputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image =
      const_cast< typename Superclass::InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage >
void
BoneMorphometryImageFilter< TInputImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage >
void
BoneMorphometryImageFilter< TInputImage >
::AllocateOutputs()
{
  // Pass the input through as the output
  InputImagePointer image =
    const_cast< TInputImage * >( this->GetInput() );

  this->GraftOutput(image);

  // Nothing that needs to be allocated for the remaining outputs
}

template< typename TInputImage >
void
BoneMorphometryImageFilter< TInputImage >
::BeforeThreadedGenerateData()
{
    ThreadIdType numberOfThreads = this->GetNumberOfThreads();

    m_Pp = 0;
    m_Pl = 0;
    m_PlX = 0;
    m_PlY = 0;
    m_PlZ = 0;

    // Resize the thread temporaries
    m_NumVoxels.SetSize(numberOfThreads);
    m_NumBoneVoxels.SetSize(numberOfThreads);
    m_NumX.SetSize(numberOfThreads);
    m_NumY.SetSize(numberOfThreads);
    m_NumZ.SetSize(numberOfThreads);
    m_NumXO.SetSize(numberOfThreads);
    m_NumYO.SetSize(numberOfThreads);
    m_NumZO.SetSize(numberOfThreads);

    // Resize the thread temporaries
    m_NumVoxels.Fill(0);
    m_NumBoneVoxels.Fill(0);
    m_NumX.Fill(0);
    m_NumY.Fill(0);
    m_NumZ.Fill(0);
    m_NumXO.Fill(0);
    m_NumYO.Fill(0);
    m_NumZO.Fill(0);
}

template< typename TInputImage >
void
BoneMorphometryImageFilter< TInputImage >
::AfterThreadedGenerateData()
{
    ThreadIdType numberOfThreads = this->GetNumberOfThreads();

    long numVoxels = 0;
    long numBoneVoxels = 0;
    long numX = 0;
    long numY = 0;
    long numZ = 0;
    long numXO = 0;
    long numYO = 0;
    long numZO = 0;

    for (unsigned int i = 0; i < numberOfThreads; ++i )
    {
      numVoxels += m_NumVoxels[i];
      numBoneVoxels += m_NumBoneVoxels[i];
      numX += m_NumX[i];
      numY += m_NumY[i];
      numZ += m_NumZ[i];
      numXO += m_NumXO[i];
      numYO += m_NumYO[i];
      numZO += m_NumZO[i];
    }

    typename TInputImage::SpacingType inSpacing = this->GetInput()->GetSpacing();
    m_Pp = (double)numBoneVoxels / (double)numVoxels;
    m_PlX = (double) ((numX+numXO)/2.0) / (double) (numVoxels * inSpacing[0]) * 2;
    m_PlY = (double) ((numY+numYO)/2.0) / (double) (numVoxels * inSpacing[1]) * 2;
    m_PlZ = (double) ((numZ+numZO)/2.0) / (double) (numVoxels * inSpacing[2]) * 2;
    m_Pl =(m_PlX + m_PlY + m_PlZ) / 3.0;
}

template< typename TInputImage >
void
BoneMorphometryImageFilter< TInputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
    NeighborhoodRadiusType radius;
    radius.Fill(1);
    NeighborhoodOffsetType offsetX = {{1,0,0}};
    NeighborhoodOffsetType offsetXO = {{-1,0,0}};
    NeighborhoodOffsetType offsetY = {{0,1,0}};
    NeighborhoodOffsetType offsetYO = {{0,-1,0}};
    NeighborhoodOffsetType offsetZ = {{0,0,1}};
    NeighborhoodOffsetType offsetZO = {{0,0,-1}};

    long numVoxels = 0;
    long numBoneVoxels = 0;
    long numX = 0;
    long numY = 0;
    long numZ = 0;
    long numXO = 0;
    long numYO = 0;
    long numZO = 0;

    typename TInputImage::Pointer maskPointer = TInputImage::New();
    maskPointer = const_cast<TInputImage *>(this->GetMaskImage());

    NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage > boundaryFacesCalculator;
    typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::FaceListType
    faceList = boundaryFacesCalculator( this->GetInput(), outputRegionForThread, radius );
    typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::FaceListType::iterator fit = faceList.begin();

    for (; fit != faceList.end(); ++fit )
    {
      NeighborhoodIteratorType inputNIt(radius, this->GetInput(), *fit );
      while( !inputNIt.IsAtEnd() )
      {
        if( maskPointer && maskPointer->GetPixel( inputNIt.GetIndex() ) != 0 )
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
            ++numX;
          }
          if( inputNIt.GetPixel(offsetXO) < m_Threshold )
          {
            ++numXO;
          }
          if( inputNIt.GetPixel(offsetY) < m_Threshold )
          {
            ++numY;
          }
          if( inputNIt.GetPixel(offsetYO) < m_Threshold )
          {
            ++numYO;
          }
          if( inputNIt.GetPixel(offsetZ) < m_Threshold )
          {
            ++numZ;
          }
          if( inputNIt.GetPixel(offsetZO) < m_Threshold )
          {
            ++numZO;
          }
        }

        ++inputNIt;
      }
    }

    m_NumVoxels[threadId] = numVoxels;
    m_NumBoneVoxels[threadId] = numBoneVoxels;
    m_NumX[threadId] = numX;
    m_NumY[threadId] = numY;
    m_NumZ[threadId] = numZ;
    m_NumXO[threadId] = numXO;
    m_NumYO[threadId] = numYO;
    m_NumZO[threadId] = numZO;
}

template< typename TImage >
void
BoneMorphometryImageFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Threshold: " << m_Threshold << std::endl;
  os << indent << "m_Pp: " << m_Pp << std::endl;
  os << indent << "m_Pl: " << m_Pl << std::endl;
  os << indent << "m_PlX: " << m_PlX << std::endl;
  os << indent << "m_PlY: " << m_PlY << std::endl;
  os << indent << "m_PlZ: " << m_PlZ << std::endl;
  os << indent << "m_NumVoxels: " << m_NumVoxels << std::endl;
  os << indent << "m_NumBoneVoxels: " << m_NumBoneVoxels << std::endl;
  os << indent << "m_NumX: " << m_NumX << std::endl;
  os << indent << "m_NumY: " << m_NumY << std::endl;
  os << indent << "m_NumZ: " << m_NumZ << std::endl;
  os << indent << "m_NumXO: " << m_NumXO << std::endl;
  os << indent << "m_NumYO: " << m_NumYO << std::endl;
  os << indent << "m_NumZO: " << m_NumZO << std::endl;

}
} // end namespace itk

#endif // itkBoneMorphometryImageFilter_hxx
