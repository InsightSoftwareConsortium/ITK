/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIOPrintTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImage.h"
#include "itkAnalyzeImageIO.h"
#include "itkDICOMImageIO2.h"
#include "itkDicomImageIO.h"
#include "itkGE4ImageIO.h"
#include "itkGE5ImageIO.h"
#include "itkGEAdwImageIO.h"
#include "itkGiplImageIO.h"
#include "itkIPLCommonImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMetaImageIO.h"
#include "itkPNGImageIO.h"
#include "itkRawImageIO.h"
#include "itkStimulateImageIO.h"
#include "itkVOLImageIO.h"
#include "itkVTKImageIO.h"

int itkIOPrintTest(int , char* [])
{
  typedef itk::Image<unsigned char,2> ImageType;
  itk::ImageFileReader<ImageType>::Pointer reader =
    itk::ImageFileReader<ImageType>::New();
 
  itk::PNGImageIO::Pointer PNGio;
  PNGio = itk::PNGImageIO::New();
  reader->SetImageIO(PNGio);
  std::cout << "---------------PNG" << reader;

  itk::AnalyzeImageIO::Pointer Analyzeio;
  Analyzeio = itk::AnalyzeImageIO::New();
  reader->SetImageIO(Analyzeio);
  std::cout << "---------------Analyze" << reader;

  itk::DicomImageIO::Pointer Dicomio;
  Dicomio = itk::DicomImageIO::New();
  reader->SetImageIO(Dicomio);
  std::cout << "---------------Dicom" << reader;

  itk::GE4ImageIO::Pointer GE4io;
  GE4io = itk::GE4ImageIO::New();
  reader->SetImageIO(GE4io);
  std::cout << "---------------GE4" << reader;

  itk::GE5ImageIO::Pointer GE5io;
  GE5io = itk::GE5ImageIO::New();
  reader->SetImageIO(GE5io);
  std::cout << "---------------GE5" << reader;

  itk::GEAdwImageIO::Pointer GEAdwio;
  GEAdwio = itk::GEAdwImageIO::New();
  reader->SetImageIO(GEAdwio);
  std::cout << "---------------GEAdw" << reader;

  itk::MetaImageIO::Pointer Metaio;
  Metaio = itk::MetaImageIO::New();
  reader->SetImageIO(Metaio);
  std::cout << "---------------Meta" << reader;

  itk::RawImageIO<unsigned char>::Pointer Rawio;
  Rawio = itk::RawImageIO<unsigned char>::New();
  reader->SetImageIO(Rawio);
  std::cout << "---------------Raw" << reader;

  itk::StimulateImageIO::Pointer Stimulateio;
  Stimulateio = itk::StimulateImageIO::New();
  reader->SetImageIO(Stimulateio);
  std::cout << "---------------Stimulate" << reader;

  itk::VOLImageIO::Pointer VOLio;
  VOLio = itk::VOLImageIO::New();
  reader->SetImageIO(VOLio);
  std::cout << "---------------VOL" << reader;

  itk::VTKImageIO::Pointer VTKio;
  VTKio = itk::VTKImageIO::New();
  reader->SetImageIO(VTKio);
  std::cout << "---------------VTK" << reader;

  return 0;
}
