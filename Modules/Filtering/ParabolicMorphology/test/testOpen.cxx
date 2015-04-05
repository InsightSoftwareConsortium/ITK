#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkParabolicOpenImageFilter.h"
#include "itkTimeProbe.h"
#include "itkMultiThreader.h"


int
main(int argc, char * argv[])
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

  filter->SetParabolicAlgorithm(FilterType::INTERSECTION);

  //   itk::SimpleFilterWatcher watcher(filter, "filter");
  itk::TimeProbe NewTime;

  for (unsigned i = 0; i < 1; i++)
  {
    filter->Modified();
    NewTime.Start();
    filter->Update();
    NewTime.Stop();
  }

  typedef itk::ImageFileWriter<IType> WriterType;
  WriterType::Pointer                 writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();
  std::cout << std::setprecision(3) << NewTime.GetMean() << std::endl;

  if (argc > 3)
  {
    // testing equivalence
    filter->SetParabolicAlgorithm(FilterType::CONTACTPOINT);
    writer->SetFileName(argv[3]);
    writer->Update();
  }

  return EXIT_SUCCESS;
}
