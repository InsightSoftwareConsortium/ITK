#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkParabolicDilateImageFilter.h"
#include "itkTimeProbe.h"
#include "itkMultiThreader.h"

// sanity check of the image spacing option

int
itkParaDilateTest(int argc, char * argv[])
{
  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);
  const int dim = 2;

  typedef unsigned char          PType;
  typedef itk::Image<PType, dim> IType;

  float scale(1.0);
  if (argc > 4)
  {
    scale = atof(argv[4]);
  }

  typedef itk::ImageFileReader<IType> ReaderType;
  ReaderType::Pointer                 reader = ReaderType::New();
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

  typedef itk::ParabolicDilateImageFilter<IType, IType> FilterType;

  FilterType::Pointer filter = FilterType::New();


  filter->SetInput(reader->GetOutput());

  filter->SetScale(scale);
  filter->SetUseImageSpacing(true);
  filter->SetParabolicAlgorithm(FilterType::INTERSECTION);
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  typedef itk::ImageFileWriter<IType> WriterType;
  WriterType::Pointer                 writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  filter->SetScale(scale);
  filter->SetUseImageSpacing(true);
  filter->SetParabolicAlgorithm(FilterType::CONTACTPOINT);
  filter->Update();

  writer->SetInput(filter->GetOutput());
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
