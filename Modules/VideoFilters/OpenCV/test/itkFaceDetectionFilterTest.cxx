#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFaceDetectionFilter.h"
#include "itkRegionOfInterestImageFilter.h"

int test_face_detection (char* input, char *outputMainImage, char *outputFacesWhithoutExtension , char *trainerFilename)
{

  typedef itk::Image< unsigned char,  2>   InputImageType;
  typedef itk::Image< unsigned char, 2>   OutputImageType;   
  
  itk::FaceDetectionFilter<InputImageType>::Pointer filter 
    = itk::FaceDetectionFilter<InputImageType>::New();
  
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(input);

  filter->SetInput(reader->GetOutput());
  filter->SetTrainerFileName(trainerFilename);
  filter->SetColor(1);
  filter->SetDrawRectangles(false);
  filter->SetGenerateROI(true);

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(outputMainImage);
  
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr<<"When detecting faces"<<std::endl;
    std::cerr<<e.GetFile()<<std::endl;
    std::cerr<<e.GetLine()<<std::endl;
    std::cerr<<e.GetLocation()<<std::endl;
    std::cerr<<e.GetNameOfClass()<<std::endl;
    std::cerr<<e.GetDescription()<<std::endl;
    return EXIT_FAILURE;
    }
  char table[] = "1234567890qwertyuiopasdfghjklzxcvbnm";
  std::list< itk::ImageRegion<2>* >* list = filter->GetFacesAsROI();

  itk::RegionOfInterestImageFilter<InputImageType ,InputImageType >::Pointer regionfilter =
    itk::RegionOfInterestImageFilter< InputImageType,InputImageType >::New();

  regionfilter->SetInput( filter->GetOutput() );

  int i;
  for ( i = 0; i < filter->GetFacesTotal() ; i++)
    {
    std::string title = outputFacesWhithoutExtension;
    title += table[i];
    title += ".png";
    
    regionfilter->SetRegionOfInterest( *list->front() );

    writer->SetInput( regionfilter->GetOutput() );
    writer->SetFileName(title.c_str());

    try
      {
      writer->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      std::cerr<<"When printing the regions detected, step #"<<i<<std::endl;
      std::cerr<<e.GetFile()<<std::endl;
      std::cerr<<e.GetLine()<<std::endl;
      std::cerr<<e.GetLocation()<<std::endl;
      std::cerr<<e.GetNameOfClass()<<std::endl;
      std::cerr<<e.GetDescription()<<std::endl;
      return EXIT_FAILURE;
      }
    list->pop_front();
    }

  return EXIT_SUCCESS;
}

int itkFaceDetectionFilterTest (int argv, char* argc[])
{
  /*int k = test_face_detection("C:/projects/ITK-Vid-A2D2/Data/faces3.png",
    "C:/projects/ITK-Vid-A2D2_build/Testing/FaceDetectionFilterResult.png",
    "C:/projects/ITK-Vid-A2D2_build/Testing/Face_",
    "C:/projects/ITK-Vid-A2D2/Data/haarcascade_frontalface_alt2.xml");
  std::cin>>k;
  return k;*/
 return test_face_detection(argc[1],argc[2],argc[3],argc[4]);
}

