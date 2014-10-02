#include "itkTestMain.h"

void
RegisterTests()
{
  REGISTER_TEST(itkStrainImageFilterAccurateTest);
  REGISTER_TEST(itkStrainImageFilterRecursiveGaussianTest);
  REGISTER_TEST(itkStrainImageFilterBSplineTest);
  REGISTER_TEST(itkStrainImageFilterBSplineApproximationTest);
}

#include <string>

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNthElementImageAdaptor.h"
#include "itkVTKImageIO.h"

#include "itkStrainImageFilter.h"

#include "itkHigherOrderAccurateGradientImageFilter.h"
#include "itkDifferenceOfGaussiansGradientImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkBSplineGradientImageFilter.h"
#include "itkBSplineApproximationGradientImageFilter.h"

template <class TDisplacementImageType>
int
ReadInDisplacements(const char * inputFile, typename TDisplacementImageType::Pointer & inputImage)
{
  typedef TDisplacementImageType InputImageType;

  typedef itk::ImageFileReader<InputImageType> ReaderType;

  typename ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(inputFile);

  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  inputImage = reader->GetOutput();

  return EXIT_SUCCESS;
}

template <class TPixel, unsigned int Dimension, class TTensorImage>
int
WriteOutStrains(char * outputPrefix, TTensorImage * strainImage)
{
  typedef TPixel                           PixelType;
  typedef TTensorImage                     TensorImageType;
  typedef itk::Image<PixelType, Dimension> ComponentImageType;

  typedef itk::ImageFileWriter<TensorImageType>                   TensorWriterType;
  typedef itk::ImageFileWriter<ComponentImageType>                TensorComponentWriterType;
  typedef itk::NthElementImageAdaptor<TensorImageType, PixelType> AdaptorType;
  typedef itk::VTKImageIO                                         IOType;

  typename TensorWriterType::Pointer          tensorWriter = TensorWriterType::New();
  typename TensorComponentWriterType::Pointer tensorComponentWriter = TensorComponentWriterType::New();
  typename ComponentImageType::Pointer        outImage = ComponentImageType::New();
  typename AdaptorType::Pointer               adaptor = AdaptorType::New();
  IOType::Pointer                             io = IOType::New();

  tensorWriter->SetFileName(std::string(outputPrefix) + "Output.vtk");
  tensorWriter->SetInput(strainImage);
  io->SetFileTypeToBinary();
  tensorWriter->SetImageIO(io);

  outImage->SetRegions(strainImage->GetLargestPossibleRegion());
  outImage->Allocate();
  tensorComponentWriter->SetInput(outImage);
  adaptor->SetImage(strainImage);

  std::ostringstream ostr;
  try
  {
    tensorWriter->Update();
    for (unsigned int i = 0; i < 3; ++i)
    {
      ostr.str("");
      ostr << outputPrefix << "Component" << i << ".mha";
      adaptor->SelectNthElement(i);
      adaptor->Update();
      itk::ImageRegionConstIterator<AdaptorType>   adaptorIt(adaptor, adaptor->GetBufferedRegion());
      itk::ImageRegionIterator<ComponentImageType> outputIt(outImage, outImage->GetLargestPossibleRegion());
      for (adaptorIt.GoToBegin(), outputIt.GoToBegin(); !adaptorIt.IsAtEnd(); ++adaptorIt, ++outputIt)
      {
        outputIt.Set(adaptorIt.Get());
      }
      tensorComponentWriter->SetFileName(ostr.str());
      tensorComponentWriter->Update();
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

int
itkStrainImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                                    Dimension = 2;
  typedef double                                        PixelType;
  typedef itk::Vector<PixelType, Dimension>             DisplacementVectorType;
  typedef itk::Image<DisplacementVectorType, Dimension> InputImageType;

  typedef itk::StrainImageFilter<InputImageType, PixelType, PixelType> FilterType;
  typedef FilterType::OutputImageType                                  TensorImageType;

  FilterType::Pointer filter = FilterType::New();

  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(argv[1], inputDisplacements) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  filter->SetInput(inputDisplacements);
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(argv[2], filter->GetOutput()) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkStrainImageFilterLagrangianTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                                    Dimension = 2;
  typedef double                                        PixelType;
  typedef itk::Vector<PixelType, Dimension>             DisplacementVectorType;
  typedef itk::Image<DisplacementVectorType, Dimension> InputImageType;

  typedef itk::StrainImageFilter<InputImageType, PixelType, PixelType> FilterType;
  typedef FilterType::OutputImageType                                  TensorImageType;

  FilterType::Pointer filter = FilterType::New();

  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(argv[1], inputDisplacements) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  filter->SetInput(inputDisplacements);
  try
  {
    filter->Update();
    filter->SetStrainForm(FilterType::GREENLAGRANGIAN);
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(argv[2], filter->GetOutput()) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkStrainImageFilterEulerianTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                                    Dimension = 2;
  typedef double                                        PixelType;
  typedef itk::Vector<PixelType, Dimension>             DisplacementVectorType;
  typedef itk::Image<DisplacementVectorType, Dimension> InputImageType;

  typedef itk::StrainImageFilter<InputImageType, PixelType, PixelType> FilterType;
  typedef FilterType::OutputImageType                                  TensorImageType;

  FilterType::Pointer filter = FilterType::New();

  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(argv[1], inputDisplacements) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  filter->SetInput(inputDisplacements);
  try
  {
    filter->Update();
    filter->SetStrainForm(FilterType::EULERIANALMANSI);
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(argv[2], filter->GetOutput()) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkStrainImageFilterAccurateTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                                    Dimension = 2;
  typedef double                                        PixelType;
  typedef itk::Vector<PixelType, Dimension>             DisplacementVectorType;
  typedef itk::Image<DisplacementVectorType, Dimension> InputImageType;

  typedef itk::StrainImageFilter<InputImageType, PixelType, PixelType> FilterType;
  typedef FilterType::OutputImageType                                  TensorImageType;

  typedef itk::HigherOrderAccurateGradientImageFilter<itk::Image<PixelType, Dimension>, PixelType, PixelType>
    GradientFilterType;

  FilterType::Pointer         filter = FilterType::New();
  GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
  gradientFilter->SetOrderOfAccuracy(3);
  filter->SetGradientFilter(gradientFilter.GetPointer());

  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(argv[1], inputDisplacements) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  filter->SetInput(inputDisplacements);
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(argv[2], filter->GetOutput()) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkStrainImageFilterDoGTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                                    Dimension = 2;
  typedef double                                        PixelType;
  typedef itk::Vector<PixelType, Dimension>             DisplacementVectorType;
  typedef itk::Image<DisplacementVectorType, Dimension> InputImageType;

  typedef itk::StrainImageFilter<InputImageType, PixelType, PixelType> FilterType;
  typedef FilterType::OutputImageType                                  TensorImageType;

  typedef itk::DifferenceOfGaussiansGradientImageFilter<itk::Image<PixelType, Dimension>, PixelType> GradientFilterType;

  FilterType::Pointer         filter = FilterType::New();
  GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
  gradientFilter->SetWidth(1.0);
  filter->SetGradientFilter(gradientFilter.GetPointer());

  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(argv[1], inputDisplacements) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  filter->SetInput(inputDisplacements);
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(argv[2], filter->GetOutput()) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkStrainImageFilterRecursiveGaussianTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                                    Dimension = 2;
  typedef double                                        PixelType;
  typedef itk::Vector<PixelType, Dimension>             DisplacementVectorType;
  typedef itk::Image<DisplacementVectorType, Dimension> InputImageType;

  typedef itk::StrainImageFilter<InputImageType, PixelType, PixelType> FilterType;
  typedef FilterType::OutputImageType                                  TensorImageType;
  typedef FilterType::GradientOutputImageType                          GradientOutputImageType;

  typedef itk::GradientRecursiveGaussianImageFilter<itk::Image<PixelType, Dimension>, GradientOutputImageType>
    GradientFilterType;

  FilterType::Pointer         filter = FilterType::New();
  GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
  gradientFilter->SetSigma(1.0);
  filter->SetGradientFilter(gradientFilter.GetPointer());

  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(argv[1], inputDisplacements) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  filter->SetInput(inputDisplacements);
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(argv[2], filter->GetOutput()) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkStrainImageFilterBSplineTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                                    Dimension = 2;
  typedef double                                        PixelType;
  typedef itk::Vector<PixelType, Dimension>             DisplacementVectorType;
  typedef itk::Image<DisplacementVectorType, Dimension> InputImageType;

  typedef itk::StrainImageFilter<InputImageType, PixelType, PixelType> FilterType;
  typedef FilterType::OutputImageType                                  TensorImageType;
  typedef FilterType::GradientOutputImageType                          GradientOutputImageType;

  typedef itk::BSplineGradientImageFilter<itk::Image<PixelType, Dimension>, PixelType, PixelType> GradientFilterType;

  FilterType::Pointer         filter = FilterType::New();
  GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
  filter->SetGradientFilter(gradientFilter.GetPointer());

  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(argv[1], inputDisplacements) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  filter->SetInput(inputDisplacements);
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(argv[2], filter->GetOutput()) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkStrainImageFilterBSplineApproximationTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                                    Dimension = 2;
  typedef double                                        PixelType;
  typedef itk::Vector<PixelType, Dimension>             DisplacementVectorType;
  typedef itk::Image<DisplacementVectorType, Dimension> InputImageType;

  typedef itk::StrainImageFilter<InputImageType, PixelType, PixelType> FilterType;
  typedef FilterType::OutputImageType                                  TensorImageType;
  typedef FilterType::GradientOutputImageType                          GradientOutputImageType;

  typedef itk::BSplineApproximationGradientImageFilter<InputImageType, PixelType> GradientFilterType;

  FilterType::Pointer         filter = FilterType::New();
  GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
  gradientFilter->SetControlPointSpacingRatio(1.3);
  filter->SetVectorGradientFilter(gradientFilter.GetPointer());

  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(argv[1], inputDisplacements) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  filter->SetInput(inputDisplacements);
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(argv[2], filter->GetOutput()) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
