#include "itkImageFileReader.h"

#ifdef ITK_HAS_VTK
#include "vtkImageImport.h"
#include "vtkImageViewer.h"
#include "vtkImageActor.h"
#include "vtkRenderer.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"
#endif

int main(int ac, char** av)
{
  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " Image\n";
    }
  typedef itk::Image<unsigned char, 2> myImage;
  itk::ImageFileReader<myImage>::Pointer reader 
    = itk::ImageFileReader<myImage>::New();
  reader->DebugOn();
  reader->SetFileName(av[1]);
  try{
  reader->Update();
  }
  catch (itk::ImageFileReaderException& e)
    {
    std::cerr << "exception in file reader \n"  << e.GetDescription();
    return -1;
    }
  
  myImage::Pointer image = reader->GetOutput();
  image->Print(std::cout);
  unsigned char* data = image->GetPixelContainer()->GetBufferPointer();
  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

#ifdef ITK_HAS_VTK  
// create an importer to read the data back in
  vtkImageImport *importer = vtkImageImport::New();
  importer->SetDataExtent(1,region.GetSize()[0],
                          1,region.GetSize()[1],
                          1,1);
  importer->SetDataScalarTypeToUnsignedChar();
  importer->SetImportVoidPointer(data);
  
  vtkRenderer *renderer = vtkRenderer::New();
  vtkRenderWindow *renWin = vtkRenderWindow::New();
    renWin->AddRenderer(renderer);
  vtkRenderWindowInteractor *iren = vtkRenderWindowInteractor::New();
    iren->SetRenderWindow(renWin);

  vtkImageActor *actor = vtkImageActor::New();
  actor->SetInput(importer->GetOutput());
    
  renderer->AddActor(actor);
  // interact with data
  renWin->Render();
  iren->Start();
#endif
  return 0;
}
