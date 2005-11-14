/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIOPrintTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkAnalyzeImageIO.h"
#include "itkBioRadImageIO.h"
#include "itkDICOMImageIO2.h"
#include "itkDicomImageIO.h"
#include "itkGE4ImageIO.h"
#include "itkGE5ImageIO.h"
#include "itkGEAdwImageIO.h"
#include "itkGiplImageIO.h"
#include "itkIPLCommonImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkJPEGImageIO.h"
#include "itkLSMImageIO.h"
#include "itkMetaImageIO.h"
#include "itkPNGImageIO.h"
#include "itkRawImageIO.h"
#include "itkStimulateImageIO.h"
#include "itkTIFFImageIO.h"
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

  itk::BioRadImageIO::Pointer BioRadio;
  BioRadio = itk::BioRadImageIO::New();
  reader->SetImageIO(BioRadio);
  std::cout << "---------------BioRad" << reader;

  itk::DicomImageIO::Pointer Dicomio;
  Dicomio = itk::DicomImageIO::New();
  reader->SetImageIO(Dicomio);
  std::cout << "---------------Dicom" << reader;

  itk::DICOMImageIO2::Pointer DICOMio;
  DICOMio = itk::DICOMImageIO2::New();
  reader->SetImageIO(DICOMio);
  std::cout << "---------------DICOM" << reader;

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

  itk::GiplImageIO::Pointer Giplio;
  Giplio = itk::GiplImageIO::New();
  reader->SetImageIO(Giplio);
  std::cout << "---------------Gipl" << reader;

  itk::JPEGImageIO::Pointer JPEGio;
  JPEGio = itk::JPEGImageIO::New();
  reader->SetImageIO(JPEGio);
  std::cout << "---------------JPEG" << reader;

  itk::LSMImageIO::Pointer LSMio;
  LSMio = itk::LSMImageIO::New();
  reader->SetImageIO(LSMio);
  std::cout << "---------------LSM" << reader;

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

  itk::TIFFImageIO::Pointer Tiffio;
  Tiffio = itk::TIFFImageIO::New();
  reader->SetImageIO(Tiffio);
  std::cout << "---------------TIFF" << reader;

  itk::VTKImageIO::Pointer VTKio;
  VTKio = itk::VTKImageIO::New();
  reader->SetImageIO(VTKio);
  std::cout << "---------------VTK" << reader;

  return EXIT_SUCCESS;
}
