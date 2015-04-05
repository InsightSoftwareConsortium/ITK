#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkMorphologicalSignedDistanceTransformImageFilter.h"
// #include "itkMorphologicalDistanceTransformImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkSubtractImageFilter.h"

#include "itkTimeProbe.h"
#include "itkMultiThreader.h"

// sanity check of the image spacing option

int
main(int argc, char * argv[])
{
  // itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);
  const int dim = 3;

  typedef unsigned char          PType;
  typedef itk::Image<PType, dim> IType;

  typedef itk::Image<float, dim> FType;

  typedef itk::ImageFileReader<IType> ReaderType;
  ReaderType::Pointer                 reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  typedef itk::MorphologicalSignedDistanceTransformImageFilter<IType, FType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(reader->GetOutput());
  filter->SetOutsideValue(atoi(argv[3]));
  filter->SetUseImageSpacing(true);

  typedef itk::ImageFileWriter<FType> WriterType;
  WriterType::Pointer                 writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  typedef itk::SignedMaurerDistanceMapImageFilter<IType, FType> MaurerType;
  MaurerType::Pointer                                           maurer = MaurerType::New();
  maurer->SetInput(reader->GetOutput());
  maurer->SetUseImageSpacing(true);
  maurer->SetSquaredDistance(false);

  writer->SetInput(maurer->GetOutput());
  writer->SetFileName(argv[3]);
  writer->Update();

  typedef itk::SubtractImageFilter<FType, FType, FType> SubType;
  SubType::Pointer                                      sub = SubType::New();
  sub->SetInput(filter->GetOutput());
  sub->SetInput2(maurer->GetOutput());

  writer->SetInput(sub->GetOutput());
  writer->SetFileName(argv[4]);
  writer->Update();
  return EXIT_SUCCESS;
}
