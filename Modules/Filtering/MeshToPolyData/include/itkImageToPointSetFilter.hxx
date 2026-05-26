/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageToPointSetFilter_hxx
#define itkImageToPointSetFilter_hxx

#include "itkImageToPointSetFilter.h"

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{

template <typename TInputImage, typename TOutputMesh>
void
ImageToPointSetFilter<TInputImage, TOutputMesh>::GenerateData()
{
  // " from itkBinaryMask3DMeshSource:
  // This indicates that the current BufferedRegion is equal to the
  // requested region. This action prevents useless rexecutions of
  // the pipeline.
  this->GetOutput()->SetBufferedRegion(this->GetOutput()->GetRequestedRegion());

  OutputMeshPointer      mesh = this->GetOutput();
  PointsContainerPointer points = mesh->GetPoints();
  InputImageConstPointer image = this->GetInput(0);

  PointDataContainerPointer pointData;
  if (mesh->GetPointData())
  {
    pointData = mesh->GetPointData();
  }
  else
  { // Create
    pointData = PointDataContainer::New();
  }

  const unsigned long numberOfPixels = image->GetRequestedRegion().GetNumberOfPixels();
  ProgressReporter    progress(this, 0, numberOfPixels);
  points->Reserve(numberOfPixels);
  pointData->Reserve(numberOfPixels);
  mesh->SetPointData(pointData.GetPointer());

  typename itk::ImageRegionConstIteratorWithIndex<InputImageType> imageIt(image, image->GetRequestedRegion());
  imageIt.GoToBegin();
  PointsContainerIterator    pointsIt = points->Begin();
  PointDataContainerIterator pointDataIt = pointData->Begin();
  while (!imageIt.IsAtEnd())
  {
    image->TransformIndexToPhysicalPoint(imageIt.GetIndex(), pointsIt.Value());
    (pointDataIt.Value()) = imageIt.Get();
    ++pointsIt;
    ++pointDataIt;
    ++imageIt;
    progress.CompletedPixel();
  }
}

} // end namespace itk

#endif
