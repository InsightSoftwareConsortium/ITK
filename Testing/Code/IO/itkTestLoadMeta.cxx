

#include "itkImageFileReader.h"
#include "itkMetaImageIOFactory.h"


int main(int ac, char** av)
{
  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " Image\n";
    return EXIT_FAILURE;
    }

  typedef unsigned short PixelType;
  
  typedef itk::Image<PixelType, 3> myImage;

  itk::ImageFileReader<myImage>::Pointer reader 
                                  = itk::ImageFileReader<myImage>::New();
  
  // Register on factory capable of creating MetaImage readers
  itk::MetaImageIOFactory::RegisterOneFactory();

  reader->DebugOn();
  reader->SetFileName(av[1]);
  
  try
    {
    reader->Update();
    }
  catch (itk::ImageFileReaderException& e)
    {
    std::cerr << "exception in file reader \n"  << e.GetDescription();
    return EXIT_FAILURE;
    }
  
  myImage::Pointer image = reader->GetOutput();

  image->Print(std::cout );
  
  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  PixelType * data = image->GetPixelContainer()->GetBufferPointer();

  unsigned long numberOfPixels = region.GetNumberOfPixels(); 
  for(unsigned int i=0; i < numberOfPixels; i++ )
    {
      std::cout << i << " : " << *data++ << std::endl;
    }

  return EXIT_SUCCESS;
}
