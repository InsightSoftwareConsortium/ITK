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

#include "itkFastGrowCut.h"

#include "itkPrintHelper.h"

namespace
{
template <typename ITKImageType>
void
FindITKImageROI(ITKImageType * im, std::vector<long> & imROI)
{

  typename ITKImageType::IndexType roiStart;
  typename ITKImageType::IndexType roiEnd;
  typename ITKImageType::IndexType start;
  typename ITKImageType::SizeType  size;

  size = im->GetLargestPossibleRegion().GetSize();
  start = im->GetLargestPossibleRegion().GetIndex();


  roiStart[0] = 0;
  roiStart[1] = 0;
  roiStart[2] = 0;
  roiEnd[0] = 0;
  roiEnd[1] = 0;
  roiEnd[2] = 0;

  unsigned int ndims = im->GetImageDimension();

  bool                                            foundLabel = false;
  itk::ImageRegionIteratorWithIndex<ITKImageType> label(im, im->GetBufferedRegion());
  for (label.GoToBegin(); !label.IsAtEnd(); ++label)
  {
    if (label.Get() != 0)
    {
      typename ITKImageType::IndexType idx = label.GetIndex();
      for (unsigned i = 0; i < ndims; i++)
      {
        if (!foundLabel)
        {
          roiStart[i] = idx[i];
          roiEnd[i] = idx[i];
        }
        else
        {
          if (idx[i] <= roiStart[i])
          {
            roiStart[i] = idx[i];
          }
          if (idx[i] >= roiEnd[i])
          {
            roiEnd[i] = idx[i];
          }
        }
      }
      foundLabel = true;
    }
  }

  int radius = 17;
  for (unsigned i = 0; i < ndims; i++)
  {
    int diff = static_cast<int>(roiStart[i] - radius);
    if (diff >= start[i])
    {
      roiStart[i] -= radius;
    }
    else
    {
      roiStart[i] = start[i];
    }
    roiEnd[i] = (static_cast<unsigned int>(roiEnd[i] + radius) < size[i]) ? (roiEnd[i] + radius) : size[i] - 1;
  }

  // copy ROI to vector
  imROI.resize(6);
  for (unsigned i = 0; i < 3; i++)
  {
    imROI[i] = roiStart[i];
    imROI[i + 3] = roiEnd[i];
  }
}

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

  DIMXYZ = (imROI[3] - imROI[0]) * (imROI[4] - imROI[1]) * (imROI[5] - imROI[2]);
  imROIVec.clear();
  imROIVec.resize(DIMXYZ);
  kk = 0;
  for (k = imROI[2]; k < imROI[5]; k++)
    for (j = imROI[1]; j < imROI[4]; j++)
      for (i = imROI[0]; i < imROI[3]; i++)
      {
        index[0] = i;
        index[1] = j;
        index[2] = k;
        imROIVec[kk++] = im->GetPixel(index);
      }
}
}

namespace itk
{
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
  ImageAlgorithm::Copy(seedImage, outputImage, region, region);

  std::cerr << "InitializationFlag: " << m_InitializationFlag << std::endl;
  // Find ROI
  if (!m_InitializationFlag)
  {
    // TODO: replace this by ImageMaskSpatialObject::ComputeMyBoundingBoxInIndexSpace()
    FindITKImageROI(outputImage, m_imROI);
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
    imSize[i] = m_imROI[i + 3] - m_imROI[i];
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
    for (k = m_imROI[2]; k < m_imROI[5]; k++)
      for (j = m_imROI[1]; j < m_imROI[4]; j++)
        for (i = m_imROI[0]; i < m_imROI[3]; i++)
        {
          index[0] = i;
          index[1] = j;
          index[2] = k;
          outputImage->SetPixel(index, m_imLabVec[kk++]);
        }
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
FastGrowCut<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
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
