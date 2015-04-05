#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkParabolicOpenImageFilter.h"
#include "itkTimeProbe.h"
#include "itkMultiThreader.h"

// sanity check of the image spacing option

int
main(int, char * argv[])
{
  // itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);
  const int dim = 2;

  typedef unsigned char          PType;
  typedef itk::Image<PType, dim> IType;


  typedef itk::ImageFileReader<IType> ReaderType;
  ReaderType::Pointer                 reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  typedef itk::ParabolicOpenImageFilter<IType, IType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(reader->GetOutput());
  filter->SetSafeBorder(true);
  FilterType::RadiusType scale;
  scale[0] = 1;
  scale[1] = 0.5;

  filter->SetScale(scale);
  filter->SetUseImageSpacing(false);
  filter->Update();

  typedef itk::ImageFileWriter<IType> WriterType;
  WriterType::Pointer                 writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  // now we'll change the image spacing and see if we can reproduce
  // the result
  typedef itk::ChangeInformationImageFilter<IType> ChangeType;
  ChangeType::Pointer                              changer = ChangeType::New();
  changer->SetInput(reader->GetOutput());
  ChangeType::SpacingType newspacing;

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
  filter->Update();
  writer->SetFileName(argv[3]);
  writer->Update();
  return EXIT_SUCCESS;
}
