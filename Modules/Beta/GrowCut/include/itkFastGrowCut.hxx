/*=========================================================================
 *
 *  Copyright NumFOCUS
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

// Adapted from: https://github.com/ljzhu/FastGrowCut

#ifndef itkFastGrowCut_hxx
#define itkFastGrowCut_hxx

#include <iostream>

#include "itkFastGrowCut.h"
#include "FastGrowCut.h"

#include "itkObjectFactory.h"
#include "itkSmartPointer.h"
#include "itkImage.h"
#include "itkTimeProbe.h"
#include "itkPrintHelper.h"

namespace itk
{
//if using smart pointer, don't need this destructor
template <typename TInputImage, typename TOutputImage>
FastGrowCut<TInputImage, TOutputImage>::~FastGrowCut()
{
  if ( this->m_fastGC != nullptr )
  {
    delete m_fastGC;
  }
}

template <typename TInputImage, typename TOutputImage>
void
FastGrowCut<TInputImage, TOutputImage>::GenerateData()
{
  using OutputImageType = TOutputImage;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  using InputImageType = TInputImage;
  using InputImageRegionType = typename InputImageType::RegionType;

  auto inputImage = this->GetInput();
  auto seedImage = this->GetSeedImage();
  auto outputImage = this->GetOutput();

  // Copy seedImage into the output
  InputImageRegionType region = inputImage->GetRequestedRegion();
  outputImage->SetLargestPossibleRegion(region);
  outputImage->SetBufferedRegion(region);
  outputImage->Allocate();
  ImageAlgorithm::Copy(seedImage,outputImage,region,region);

  itk::TimeProbe timer;

  timer.Start();

  std::cerr << "InitializationFlag: " << m_InitializationFlag << std::endl;
  // Find ROI
  if ( !m_InitializationFlag )
  {
    FGC::FindITKImageROI( outputImage, m_imROI );
    std::cerr << "image ROI = [" << m_imROI[0] << "," << m_imROI[1] << "," << m_imROI[2] << ";"  \
              << m_imROI[3] << "," << m_imROI[4] << "," << m_imROI[5] << "]" << std::endl;
    // SB: Find the ROI from the seed volume in the source volume and store it in m_imSrcVec
    FGC::ExtractITKImageROI<InputImagePixelType>( inputImage, m_imROI, m_imSrcVec );
  }
  // SB: Store the ROI from the seed volume in m_imSeedVec
  FGC::ExtractITKImageROI<LabelPixelType>( outputImage, m_imROI, m_imSeedVec );

  // Initialize FastGrowCut
  std::vector<long> imSize( 3 );
  for ( int i = 0; i < 3; i++ )
  {
    imSize[i] = m_imROI[i + 3] - m_imROI[i];
  }
  m_fastGC->SetSourceImage( m_imSrcVec );
  m_fastGC->SetSeedlImage( m_imSeedVec );
  m_fastGC->SetImageSize( imSize );
  m_fastGC->SetWorkMode( m_InitializationFlag );

  // Do Segmentation
  m_fastGC->DoSegmentation();
  m_fastGC->GetForegroundmage( m_imLabVec );

  // Update result. SB: Seed volume is replaced with grow cut result
  FGC::UpdateITKImageROI<LabelPixelType>( m_imLabVec, m_imROI, outputImage );

  timer.Stop();

  if ( !m_InitializationFlag )
  {
    std::cout << "Initial fast GrowCut segmentation time: " << timer.GetMean() << " seconds\n";
  }
  else
  {
    std::cout << "Adaptive fast GrowCut segmentation time: " << timer.GetMean() << " seconds\n";
  }
}

template <typename TInputImage, typename TOutputImage>
void
FastGrowCut<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage, typename TOutputImage>
void
FastGrowCut<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if (this->GetInput())
  {
    InputImagePointer input = const_cast<TInputImage *>(this->GetInput());
    input->SetRequestedRegionToLargestPossibleRegion();
  }
}

template <typename TInputImage, typename TOutputImage>
void
FastGrowCut<TInputImage, TOutputImage>::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  using namespace itk::print_helper;
  os << indent << "InitializationFlag: " << m_InitializationFlag << std::endl;
  os << indent << "imSeedVec: " << m_imSeedVec << std::endl;
  os << indent << "imLabVec: " << m_imLabVec << std::endl;
  os << indent << "imSrcVec: " << m_imSrcVec << std::endl;
  os << indent << "imROI: " << m_imROI << std::endl;
  os << indent << "FastGC: " << m_fastGC << std::endl;
}

}
#endif // ifndef itkFastGrowCut_hxx
