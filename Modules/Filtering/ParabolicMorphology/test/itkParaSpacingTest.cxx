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
#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkParabolicOpenImageFilter.h"
#include "itkTimeProbe.h"
#include "itkMultiThreaderBase.h"

// sanity check of the image spacing option

int
itkParaSpacingTest(int, char * argv[])
{
  itk::MultiThreaderBase::SetGlobalMaximumNumberOfThreads(1);
  constexpr int dim = 2;

  using PType = unsigned char;
  using IType = itk::Image<PType, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  using FilterType = itk::ParabolicOpenImageFilter<IType, IType>;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(reader->GetOutput());
  filter->SetSafeBorder(true);
  FilterType::RadiusType scale;
  scale[0] = 1;
  scale[1] = 0.5;

  filter->SetScale(scale);
  filter->SetUseImageSpacing(false);
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  using WriterType = itk::ImageFileWriter<IType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // now we'll change the image spacing and see if we can reproduce
  // the result
  using ChangeType = itk::ChangeInformationImageFilter<IType>;
  ChangeType::Pointer changer = ChangeType::New();
  changer->SetInput(reader->GetOutput());
  ChangeType::SpacingType oldspacing, newspacing;

  oldspacing = filter->GetOutput()->GetSpacing();
  newspacing[0] = 1 / sqrt((float)1);
  newspacing[1] = 1 / sqrt((float)0.5);

  changer->SetOutputSpacing(newspacing);
  changer->ChangeSpacingOn();
  // set scales to deliver the same result
  scale[0] = 1;
  scale[1] = 1;
  filter->SetInput(changer->GetOutput());
  filter->SetScale(scale);
  filter->SetUseImageSpacing(true);
  // change the spacing back to original to allow comparison
  ChangeType::Pointer changerback = ChangeType::New();
  changerback->SetInput(filter->GetOutput());
  changerback->SetOutputSpacing(oldspacing);
  changerback->ChangeSpacingOn();

  try
  {
    changerback->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
  writer->SetInput(changerback->GetOutput());
  writer->SetFileName(argv[3]);
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
