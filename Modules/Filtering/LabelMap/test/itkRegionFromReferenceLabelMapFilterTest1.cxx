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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelImageToLabelMapFilter.h"
#include "itkRegionFromReferenceLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int
itkRegionFromReferenceLabelMapFilterTest1(int argc, char * argv[])
{

  if (argc != 4)
  {
    std::cerr << "usage: " << argv[0] << " input reference output" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 3;

  using ImageType = itk::Image<unsigned char, dim>;

  using LabelObjectType = itk::LabelObject<unsigned char, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using I2LType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using ChangeType = itk::RegionFromReferenceLabelMapFilter<LabelMapType>;
  ChangeType::Pointer change = ChangeType::New();
  change->SetInput(i2l->GetOutput());
  change->SetReferenceImage(reader2->GetOutput());
  itk::SimpleFilterWatcher watcher6(change, "filter");
  change->UpdateLargestPossibleRegion();

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput(change->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
