#include "itkImageFileReader.h"
#include "itkImageToVTKImageFilter.h"
#include "itkNormalizeImageFilter.h"
#include "itkChangeInformationImageFilter.h"
#include <vtkRenderWindowInteractor.h>

#include "itkFDFImageIOFactory.h"
#include "itkFDFImageIO.h"

#include <vtkImageViewer.h>

#include "itkImage.h"

int
main(int argc, char ** argv)
{
  typedef float      PixelType;
  const unsigned int Dimension = 2;

  typedef itk::Image<PixelType, Dimension>                ImageType;
  typedef itk::ImageFileReader<ImageType>                 ReaderType;
  typedef itk::ImageToVTKImageFilter<ImageType>           ImageToVTKType;
  typedef itk::NormalizeImageFilter<ImageType, ImageType> NormalizeFilter;
  typedef itk::ChangeInformationImageFilter<ImageType>    ChangeInformationFilter;

  // Register FDF Factory
  itk::FDFImageIOFactory::RegisterOneFactory();

  ReaderType::Pointer              reader = ReaderType::New();
  NormalizeFilter::Pointer         normalizer = NormalizeFilter::New();
  ChangeInformationFilter::Pointer movingChange = ChangeInformationFilter::New();

  reader->SetFileName("/home/glenn/development/reader/test.fdf");

  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & exp)
  {
    std::cerr << "Exception caught" << std::endl;
    std::cerr << exp << std::endl;
  }

  std::cout << reader << std::endl;

  normalizer->SetInput(reader->GetOutput());

  movingChange->SetInput(normalizer->GetOutput());
  movingChange->CenterImageOn();


  vtkRenderWindowInteractor * iren = vtkRenderWindowInteractor::New();

  ImageToVTKType::Pointer bridge = ImageToVTKType::New();
  bridge->SetInput(movingChange->GetOutput());

  vtkImageViewer * viewer = vtkImageViewer::New();
  viewer->SetInput(bridge->GetOutput());
  viewer->SetColorWindow(1);
  viewer->SetColorLevel(0.1);
  // viewer->SetupInteractor(iren);

  while (1)
    viewer->Render();

  viewer->Delete();
  // iren->Delete();

  return 0;
}
