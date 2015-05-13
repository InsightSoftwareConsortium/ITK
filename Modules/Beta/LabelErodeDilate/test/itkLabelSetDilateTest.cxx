#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkLabelSetDilateImageFilter.h"


template <class MaskPixType, int dim>
int doDilate(char *In, char *Out, int radius)
{
  typedef typename itk::Image<MaskPixType, dim> MaskImType;

  // load
  typedef itk::ImageFileReader< MaskImType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( In );
  try 
    {
      reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
    }

  // Label dilation
  itk::Instance< itk::LabelSetDilateImageFilter<MaskImType, MaskImType> > Dilate;
  Dilate->SetInput(reader->GetOutput());
  Dilate->SetRadius(radius);
  Dilate->SetUseImageSpacing(true);
  typedef itk::ImageFileWriter< MaskImType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( Dilate->GetOutput() );
  writer->SetFileName( Out );
  try
    {
      writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}

/////////////////////////////////

int itkLabelSetDilateTest(int argc, char * argv[])
{

  int dim1;
  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);
  itk::ImageIOBase::IOComponentType ComponentType;

  if (argc != 4) 
    {
      std::cerr << "Usage: " << argv[0] << "inputimage radius outputimage" << std::endl;
      return(EXIT_FAILURE);
    }

  if (!readImageInfo(argv[1], &ComponentType, &dim1))
    {
    std::cerr << "Failed to open " << CmdLineObj.InputIm << std::endl;
    return(EXIT_FAILURE);
    }

  int status = EXIT_FAILURE;
  switch (dim1)
    {
    case 2:
      status=doDilate<unsigned char, 2>(argv[1], argv[3], atoi(argv[2]));
      break;
    case 3:
      status=doDilate<unsigned char, 3>(argv[1], argv[3], atoi(argv[2]));
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return (EXIT_FAILURE);
      break;
    }
  return status;
}
