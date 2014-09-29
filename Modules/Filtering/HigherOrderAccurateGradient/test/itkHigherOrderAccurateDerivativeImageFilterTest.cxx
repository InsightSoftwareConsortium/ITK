#include <sstream>

#include "itkDerivativeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkHigherOrderAccurateDerivativeImageFilter.h"

int
itkHigherOrderAccurateDerivativeImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                       Dimension = 2;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;

  std::string outputPrefix = argv[2];

  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer                     reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  typedef itk::HigherOrderAccurateDerivativeImageFilter<ImageType, ImageType> HigherFilterType;
  HigherFilterType::Pointer                                                   nthFilter = HigherFilterType::New();
  nthFilter->SetInput(reader->GetOutput());
  nthFilter->SetOrder(1);

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer                     nthWriter = WriterType::New();
  nthWriter->SetInput(nthFilter->GetOutput());

  // First order accurate.
  typedef itk::DerivativeImageFilter<ImageType, ImageType> FirstFilterType;
  FirstFilterType::Pointer                                 firstFilter = FirstFilterType::New();
  firstFilter->SetInput(reader->GetOutput());
  firstFilter->SetOrder(1);

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer                     firstWriter = WriterType::New();
  firstWriter->SetInput(firstFilter->GetOutput());

  std::ostringstream ostrm;
  try
  {
    for (unsigned int direction = 0; direction < 2; ++direction)
    {
      firstFilter->SetDirection(direction);
      ostrm.str("");
      ostrm << outputPrefix << "_DerivativeImageFilter_Direction" << direction << ".mha";
      firstWriter->SetFileName(ostrm.str());
      firstWriter->Update();

      for (unsigned int accuracy = 1; accuracy < 6; ++accuracy)
      {
        nthFilter->SetDirection(direction);
        nthFilter->SetOrderOfAccuracy(accuracy);
        ostrm.str("");
        ostrm << outputPrefix << "_Accuracy" << accuracy << "_Direction" << direction << ".mha";
        nthWriter->SetFileName(ostrm.str());
        nthWriter->Update();
      }
    }
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
