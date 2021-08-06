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

#ifndef itkFastGrowCut_hxx
#define itkFastGrowCut_hxx

#include "itkFastGrowCut.h"

#include "itkPrintHelper.h"
#include "itkImageMaskSpatialObject.h"

namespace
{
template <typename PixelType>
void
ExtractITKImageROI(const itk::Image<PixelType, 3> * im,
                   const std::vector<long> &        imROI,
                   std::vector<PixelType> &         imROIVec)
{

  // Copy itk image ROI to vector
  typedef itk::Image<PixelType, 3> ImageType;
  typename ImageType::IndexType    index;
  long                             i, j, k, kk, DIMXYZ;

  DIMXYZ = (imROI[3] - imROI[0] + 1) * (imROI[4] - imROI[1] + 1) * (imROI[5] - imROI[2] + 1);
  imROIVec.clear();
  imROIVec.resize(DIMXYZ);
  kk = 0;
  for (k = imROI[2]; k <= imROI[5]; k++)
    for (j = imROI[1]; j <= imROI[4]; j++)
      for (i = imROI[0]; i <= imROI[3]; i++)
      {
        index[0] = i;
        index[1] = j;
        index[2] = k;
        imROIVec[kk++] = im->GetPixel(index);
      }
}
} // namespace

namespace itk
{
template <typename TInputImage, typename TLabelImage>
void
FastGrowCut<TInputImage, TLabelImage>::GenerateData()
{
  using RegionType = typename TInputImage::RegionType;

  auto inputImage = this->GetInput();
  auto seedImage = this->GetSeedImage();
  auto outputImage = this->GetOutput();

  // Copy seedImage into the output
  RegionType region = inputImage->GetRequestedRegion();
  outputImage->SetLargestPossibleRegion(region);
  outputImage->SetBufferedRegion(region);
  outputImage->Allocate();
  ImageAlgorithm::Copy(seedImage, outputImage, region, region);

  std::cerr << "InitializationFlag: " << m_InitializationFlag << std::endl;
  // Find ROI
  if (!m_InitializationFlag)
  {
    using ImageMask = ImageMaskSpatialObject<3, LabelPixelType>;
    typename ImageMask::Pointer maskSO = ImageMask::New();
    maskSO->SetImage(outputImage);
    RegionType bbRegion = maskSO->ComputeMyBoundingBoxInIndexSpace();
    bbRegion.PadByRadius(17); // expand it by some padding radius
    bbRegion.Crop(seedImage->GetLargestPossibleRegion()); // clip it to the original image size

    // copy ROI to vector
    m_imROI.resize(6);
    for (unsigned d = 0; d < 3; d++)
    {
      m_imROI[d] = bbRegion.GetIndex(d);
      m_imROI[d + 3] = bbRegion.GetIndex(d) + bbRegion.GetSize(d) - 1;
    }
    std::cerr << "image ROI = [" << m_imROI[0] << "," << m_imROI[1] << "," << m_imROI[2] << ";" << m_imROI[3] << ","
              << m_imROI[4] << "," << m_imROI[5] << "]" << std::endl;
    // SB: Find the ROI from the seed volume in the source volume and store it in m_imSrcVec
    ExtractITKImageROI<InputImagePixelType>(inputImage, m_imROI, m_imSrcVec);
  }
  // SB: Store the ROI from the seed volume in m_imSeedVec
  ExtractITKImageROI<LabelPixelType>(outputImage, m_imROI, m_imSeedVec);

  // Initialize FastGrowCut
  std::vector<long> imSize(3);
  for (int i = 0; i < 3; i++)
  {
    imSize[i] = m_imROI[i + 3] - m_imROI[i] + 1;
  }
  m_fastGC->SetSourceImage(m_imSrcVec);
  m_fastGC->SetSeedlImage(m_imSeedVec);
  m_fastGC->SetImageSize(imSize);
  m_fastGC->SetWorkMode(m_InitializationFlag);

  // Do Segmentation
  m_fastGC->DoSegmentation();
  m_fastGC->GetLabelImage(m_imLabVec);

  // Update result. SB: Seed volume is replaced with grow cut result
  {
    IndexType index;
    long      i, j, k, kk;

    // Set non-ROI as zeros
    outputImage->FillBuffer(0);
    kk = 0;
    // TODO: replace these loops by a region iterator
    for (k = m_imROI[2]; k <= m_imROI[5]; k++)
      for (j = m_imROI[1]; j <= m_imROI[4]; j++)
        for (i = m_imROI[0]; i <= m_imROI[3]; i++)
        {
          index[0] = i;
          index[1] = j;
          index[2] = k;
          outputImage->SetPixel(index, m_imLabVec[kk++]);
        }
  }
}

template <typename TInputImage, typename TLabelImage>
void
FastGrowCut<TInputImage, TLabelImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage, typename TLabelImage>
void
FastGrowCut<TInputImage, TLabelImage>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if (this->GetInput())
  {
    InputImagePointer input = const_cast<TInputImage *>(this->GetInput());
    input->SetRequestedRegionToLargestPossibleRegion();
  }
}

template <typename TInputImage, typename TLabelImage>
void
FastGrowCut<TInputImage, TLabelImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  using namespace itk::print_helper;
  os << indent << "InitializationFlag: " << m_InitializationFlag << std::endl;
  os << indent << "imSeedVec: " << m_imSeedVec << std::endl;
  os << indent << "imLabVec: " << m_imLabVec << std::endl;
  os << indent << "imSrcVec: " << m_imSrcVec << std::endl;
  os << indent << "imROI: " << m_imROI << std::endl;
  os << indent << "FastGC: " << m_fastGC.get() << std::endl;
}

} // namespace itk
#endif // ifndef itkFastGrowCut_hxx
