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
#include "itkBioRadImageIOFactory.h"
#include "itkBMPImageIOFactory.h"
#include "itkGDCMImageIOFactory.h"
#include "itkDICOMImageIO2Factory.h"
#include "itkNiftiImageIOFactory.h"
#include "itkAnalyzeImageIOFactory.h"
#include "itkNiftiImageIOFactory.h"
#include "itkGiplImageIOFactory.h"
#include "itkJPEGImageIOFactory.h"
#include "itkLSMImageIOFactory.h"
#include "itkMetaImageIOFactory.h"
#include "itkPNGImageIOFactory.h"
#include "itkNrrdImageIOFactory.h"
#include "itkTIFFImageIOFactory.h"
#include "itkVTKImageIOFactory.h"
#include "itkStimulateImageIOFactory.h"
#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"

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
      ObjectFactoryBase::RegisterFactory( BioRadImageIOFactory::New() ); //should
                                                                         // be
                                                                         // before
                                                                         // GDCM
      ObjectFactoryBase::RegisterFactory( GDCMImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( MetaImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( PNGImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( VTKImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( GiplImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( LSMImageIOFactory::New() ); //should
                                                                      // be
                                                                      // before
                                                                      // TIFF
      // ITK Teleconference 2008-02-22 determined that
      // AnalyzeImageIOFactory MUST be the default for reading all
      // non-nifti identified .hdr/.img images.  This requires
      // that the AnalyzeImageIOFactory can now come before the Nifti
      // Factory because AnalyzeImageIO->CanRead() has been instrumented to
      // reject .hdr/.img files with the nifti magic number tags.
      ObjectFactoryBase::RegisterFactory( AnalyzeImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( NiftiImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( StimulateImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( JPEGImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( TIFFImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( NrrdImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( BMPImageIOFactory::New() );
      ObjectFactoryBase::RegisterFactory( DICOMImageIO2Factory::New() );
      firstTime = false;
      }
    }
}
} // end namespace itk
