/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkImageIOFactory.h"
#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"


#if defined(ITKIO_SUPPORTS_BIORAD_IMAGEIO)
#include "itkBioRadImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_BMP_IMAGEIO)
#include "itkBMPImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_GDCM_IMAGEIO)
#include "itkGDCMImageIOFactory.h"
#endif

#if !defined(ITK_USE_MODULAR_BUILD)
#  if defined(ITKIO_SUPPORTS_DICOM2_IMAGEIO)
#    include "itkDICOMImageIO2Factory.h"
#  endif
#endif

#if defined(ITKIO_SUPPORTS_ANALYZE_IMAGEIO)
#include "itkAnalyzeImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_NIFTI_IMAGEIO)
#include "itkNiftiImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_GIPL_IMAGEIO)
#include "itkGiplImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_JPEG_IMAGEIO)
#include "itkJPEGImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_LSM_IMAGEIO)
#include "itkLSMImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_META_IMAGEIO)
#include "itkMetaImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_PNG_IMAGEIO)
#include "itkPNGImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_NRRD_IMAGEIO)
#include "itkNrrdImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_TIFF_IMAGEIO)
#include "itkTIFFImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_VTK_IMAGEIO)
#include "itkVTKImageIOFactory.h"
#endif

#if defined(ITKIO_SUPPORTS_STIMULATE_IMAGEIO)
#include "itkStimulateImageIOFactory.h"
#endif


namespace itk
{
ImageIOBase::Pointer
ImageIOFactory::CreateImageIO(const char *path, FileModeType mode)
{
  RegisterBuiltInFactories();

  std::list< ImageIOBase::Pointer > possibleImageIO;
  std::list< LightObject::Pointer > allobjects =
    ObjectFactoryBase::CreateAllInstance("itkImageIOBase");
  for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
        i != allobjects.end(); ++i )
    {
    ImageIOBase *io = dynamic_cast< ImageIOBase * >( i->GetPointer() );
    if ( io )
      {
      possibleImageIO.push_back(io);
      }
    else
      {
      std::cerr << "Error ImageIO factory did not return an ImageIOBase: "
                << ( *i )->GetNameOfClass()
                << std::endl;
      }
    }
  for ( std::list< ImageIOBase::Pointer >::iterator k = possibleImageIO.begin();
        k != possibleImageIO.end(); ++k )
    {
    if ( mode == ReadMode )
      {
      if ( ( *k )->CanReadFile(path) )
        {
        return *k;
        }
      }
    else if ( mode == WriteMode )
      {
      if ( ( *k )->CanWriteFile(path) )
        {
        return *k;
        }
      }
    }
  return 0;
}

void
ImageIOFactory::RegisterBuiltInFactories()
{
  static bool firstTime = true;

  static SimpleMutexLock mutex;

    {
    // This helper class makes sure the Mutex is unlocked
    // in the event an exception is thrown.
    MutexLockHolder< SimpleMutexLock > mutexHolder(mutex);
    if ( firstTime )
      {
#if defined(ITKIO_SUPPORTS_BIORAD_IMAGEIO)
      // BioRad should be registered before GDCM
      ObjectFactoryBase::RegisterFactory( BioRadImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_GDCM_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( GDCMImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_META_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( MetaImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_PNG_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( PNGImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_VTK_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( VTKImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_GIPL_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( GiplImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_LSM_IMAGEIO)
      // LSM should be registered before TIFF
      ObjectFactoryBase::RegisterFactory( LSMImageIOFactory::New() );
#endif


#if defined(ITKIO_SUPPORTS_ANALYZE_IMAGEIO)
      // ITK Teleconference 2008-02-22 determined that
      // AnalyzeImageIOFactory MUST be the default for reading all
      // non-nifti identified .hdr/.img images.  This requires
      // that the AnalyzeImageIOFactory can now come before the Nifti
      // Factory because AnalyzeImageIO->CanRead() has been instrumented to
      // reject .hdr/.img files with the nifti magic number tags.
      ObjectFactoryBase::RegisterFactory( AnalyzeImageIOFactory::New() );
#endif


#if defined(ITKIO_SUPPORTS_NIFTI_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( NiftiImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_STIMULATE_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( StimulateImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_JPEG_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( JPEGImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_TIFF_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( TIFFImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_NRRD_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( NrrdImageIOFactory::New() );
#endif

#if defined(ITKIO_SUPPORTS_BMP_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( BMPImageIOFactory::New() );
#endif

#if !defined(ITK_USE_MODULAR_BUILD)
#if defined(ITKIO_SUPPORTS_DICOM2_IMAGEIO)
      ObjectFactoryBase::RegisterFactory( DICOMImageIO2Factory::New() );
#endif

#endif
      firstTime = false;
      }
    }
}
} // end namespace itk
