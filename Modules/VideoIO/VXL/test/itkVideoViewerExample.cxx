#include "iostream"

#include "itkVideoViewer.h"
#include "itkVideoFileReader.h"

int main (int argv, char **argc)
{
  typedef itk::Image<unsigned char, 2>   ImageType;  
  itk::VideoViewer<ImageType>::Pointer viewer = itk::VideoViewer<ImageType>::New();
  itk::VideoViewer<ImageType>::Pointer viewer2 = itk::VideoViewer<ImageType>::New();
  itk::VideoFileReader< ImageType >::Pointer reader = itk::VideoFileReader< ImageType >::New();
  reader->SetFileName("C:/projects/ITK-Vid-A2D2/Data/inde-circulation.avi");
  viewer->SetInput(reader->GetOutput());
  viewer2->SetInput(reader->GetOutput());
  viewer2->SetWindowName("Viewer2");
  viewer2->UseOpenCV(false);
   
  reader->LoadVideo();
  int FrameTotal = reader->GetFrameTotal();
  for (int i =0; i<FrameTotal; i++)
    {
    try
      {
      viewer->Update();
      viewer2->Update();
      }
    catch (itk::ExceptionObject &e)
      {
      reader->Print(std::cout);
      std::cerr<<e.GetFile()<<std::endl;
      std::cerr<<e.GetLine()<<std::endl;
      std::cerr<<e.GetLocation()<<std::endl;
      std::cerr<<e.GetNameOfClass()<<std::endl;
      std::cerr<<e.GetDescription()<<std::endl;
      std::cin>>i;
      return EXIT_FAILURE;
      }
    if ( i == static_cast<int>(100) )
      {
      viewer->Close();
      }
    if ( i == 50 )
      {
      viewer2->Close();
      }
    reader->KeepReading();
    }
  std::cin>>FrameTotal;
  return EXIT_SUCCESS;
}