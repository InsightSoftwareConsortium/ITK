/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPNGImageIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkPNGImageIOFactory.h"
#include "itkRGBPixel.h"

#ifdef ITK_HAS_VTK
#include "vtkImageImport.h"
#include "vtkImageMapper.h"
#include "vtkActor2D.h"
#include "vtkRenderer.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"
#endif

int main(int ac, char** av)
{
  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " Image\n";
    return -1;
    }
  
  // Register one Factory of PNG readers
  itk::PNGImageIOFactory::RegisterOneFactory();
  
  typedef unsigned char PixelType;

  typedef itk::Image<PixelType, 2> myImage;
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
  PixelType * data = image->GetPixelContainer()->GetBufferPointer();
  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;
#ifdef ITK_HAS_VTK  
// create an importer to read the data back in
  vtkImageImport *importer = vtkImageImport::New();
  importer->SetWholeExtent(1,region.GetSize()[0],
                          1,region.GetSize()[1],
                          1,1);
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

  vtkImageMapper *mapper = vtkImageMapper::New();
    mapper->SetInput(importer->GetOutput());
  
  vtkActor2D *actor = vtkActor2D::New();
  actor->SetMapper(mapper);
    
  renderer->AddActor(actor);
  // interact with data
  renWin->Render();
  char c; std::cin >> c;
  iren->Start();
#endif
  return 0;
}
