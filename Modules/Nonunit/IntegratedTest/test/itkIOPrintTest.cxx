/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkBioRadImageIO.h"
#include "itkGE4ImageIO.h"
#include "itkGE5ImageIO.h"
#include "itkGEAdwImageIO.h"
#include "itkGiplImageIO.h"
#include "itkImageFileReader.h"
#include "itkJPEGImageIO.h"
#include "itkLSMImageIO.h"
#include "itkMetaImageIO.h"
#include "itkPNGImageIO.h"
#include "itkRawImageIO.h"
#include "itkStimulateImageIO.h"
#include "itkTIFFImageIO.h"
#include "itkVTKImageIO.h"

int
itkIOPrintTest(int, char *[])
{
  using ImageType = itk::Image<unsigned char, 2>;
  const itk::ImageFileReader<ImageType>::Pointer reader = itk::ImageFileReader<ImageType>::New();

  const itk::PNGImageIO::Pointer PNGio = itk::PNGImageIO::New();
  reader->SetImageIO(PNGio);
  std::cout << "---------------PNG" << reader;

  const itk::BioRadImageIO::Pointer BioRadio = itk::BioRadImageIO::New();
  reader->SetImageIO(BioRadio);
  std::cout << "---------------BioRad" << reader;

  const itk::GE4ImageIO::Pointer GE4io = itk::GE4ImageIO::New();
  reader->SetImageIO(GE4io);
  std::cout << "---------------GE4" << reader;

  const itk::GE5ImageIO::Pointer GE5io = itk::GE5ImageIO::New();
  reader->SetImageIO(GE5io);
  std::cout << "---------------GE5" << reader;

  const itk::GEAdwImageIO::Pointer GEAdwio = itk::GEAdwImageIO::New();
  reader->SetImageIO(GEAdwio);
  std::cout << "---------------GEAdw" << reader;

  const itk::GiplImageIO::Pointer Giplio = itk::GiplImageIO::New();
  reader->SetImageIO(Giplio);
  std::cout << "---------------Gipl" << reader;

  const itk::JPEGImageIO::Pointer JPEGio = itk::JPEGImageIO::New();
  reader->SetImageIO(JPEGio);
  std::cout << "---------------JPEG" << reader;

  const itk::LSMImageIO::Pointer LSMio = itk::LSMImageIO::New();
  reader->SetImageIO(LSMio);
  std::cout << "---------------LSM" << reader;

  const itk::MetaImageIO::Pointer Metaio = itk::MetaImageIO::New();
  reader->SetImageIO(Metaio);
  std::cout << "---------------Meta" << reader;

  const itk::RawImageIO<unsigned char>::Pointer Rawio = itk::RawImageIO<unsigned char>::New();
  reader->SetImageIO(Rawio);
  std::cout << "---------------Raw" << reader;

  const itk::StimulateImageIO::Pointer Stimulateio = itk::StimulateImageIO::New();
  reader->SetImageIO(Stimulateio);
  std::cout << "---------------Stimulate" << reader;

  const itk::TIFFImageIO::Pointer Tiffio = itk::TIFFImageIO::New();
  reader->SetImageIO(Tiffio);
  std::cout << "---------------TIFF" << reader;

  const itk::VTKImageIO::Pointer VTKio = itk::VTKImageIO::New();
  reader->SetImageIO(VTKio);
  std::cout << "---------------VTK" << reader;

  return EXIT_SUCCESS;
}
