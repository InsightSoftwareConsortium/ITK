#include <string>
#include "itkGEAdwImageIO.h"
#include "itkGE4ImageIO.h"
#include "itkGE5ImageIO.h"
#include "itkSiemensVisionImageIO.h"
#include "itkExceptionObject.h"
#include "itkImageFileReader.h"
#include "itkImage.h"
#include <itksys/SystemTools.hxx>

typedef itk::Image<signed short, 3> ImageType ;
typedef ImageType::Pointer ImagePointer ;
typedef itk::ImageFileReader< ImageType > ImageReaderType ;

int itkGEImageIOTest(int ac, char * av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
  }
  if(ac != 4)
    return 1;
  std::string failmode(av[1]);
  std::string filetype(av[2]);
  std::string filename(av[3]);
  bool Failmode = failmode == std::string("true");
  itk::ImageIOBase::Pointer io;
  if(filetype == "GE4")
    {
      io = itk::GE4ImageIO::New();
    }
  else if(filetype == "GE5")
    {
      io = itk::GE5ImageIO::New();
    }
  else if(filetype == "GEAdw")
    {
      io = itk::GEAdwImageIO::New();
    }
  else if(filetype == "Siemens")
    {
      io = itk::SiemensVisionImageIO::New();
    }
  else
    {
      return 1;
    }

  ImagePointer input ;
  ImageReaderType::Pointer imageReader = ImageReaderType::New() ;

  try
    {
      imageReader->SetImageIO(io);
      imageReader->SetFileName(filename.c_str()) ;
      imageReader->Update() ;
      input = imageReader->GetOutput() ;
    }
  catch (itk::ExceptionObject e)
    {
      return Failmode ? 1 : 0;
    }
  return Failmode ? 0 : 1;
}

