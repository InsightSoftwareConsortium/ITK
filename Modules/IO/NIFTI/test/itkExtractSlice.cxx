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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkExtractImageFilter.h"

typedef float                                         PixelType;
typedef itk::Image<PixelType, 3>                      ImageType;
typedef itk::Image<PixelType, 2>                      SliceType;
typedef itk::ImageFileReader<ImageType>               ReaderType;
typedef itk::ExtractImageFilter<ImageType, SliceType> ExtractType;
typedef itk::ImageFileWriter<SliceType>               WriterType;

int itkExtractSlice(int argc, char *argv[])
{
  if (argc<3)
    {
    std::cout << "Usage:\n" << argv[0] << " in.nii out.nrrd";
    return EXIT_FAILURE;
    }

  try
    {
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(argv[1]);
    reader->UpdateOutputInformation();

    ImageType::RegionType inRegion = reader->GetOutput()->GetLargestPossibleRegion();

    ImageType::SizeType outSize = inRegion.GetSize();
    outSize[1] = 0;
    ImageType::IndexType outIndex = inRegion.GetIndex();
    outIndex[1] = inRegion.GetSize()[1] / 2;
    ImageType::RegionType outRegion;
    outRegion.SetSize(outSize);
    outRegion.SetIndex(outIndex);

    ExtractType::Pointer extractFilter = ExtractType::New();
    extractFilter->SetDirectionCollapseToSubmatrix();
    extractFilter->InPlaceOn();
    extractFilter->SetInput(reader->GetOutput());
    extractFilter->SetExtractionRegion(outRegion);

    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(extractFilter->GetOutput());
    writer->SetFileName(argv[2]);
    writer->Update();
    }
  catch (itk::ExceptionObject& exc)
    {
    std::cerr << exc;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
