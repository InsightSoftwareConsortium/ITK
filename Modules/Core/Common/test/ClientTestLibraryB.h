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

#ifndef ClientTestLibraryB_h
#define ClientTestLibraryB_h

#include "ClientTestLibraryBExport.h"

#include "itkObject.h"

extern "C" {
  ITK_ABI_EXPORT itk::Object * EquivalencyTable();
  ITK_ABI_EXPORT itk::Object * Image();

  ITK_ABI_EXPORT int dynamic_castDownCastEquivalencyTable( const char * type, const char * instanceSource, itk::Object const * base );
  ITK_ABI_EXPORT int dynamic_castDownCastImage( const char * type, const char * instanceSource, itk::Object const * base );

}

namespace LibraryB
{

class ClientTestLibraryB_EXPORT ITKObjectProducer
{
public:
  ITKObjectProducer();

  itk::Object * EquivalencyTable();

  itk::Object * Image();

private:
  itk::Object::Pointer m_EquivalencyTable;
  itk::Object::Pointer m_Image;
};

} // end namespace LibraryB

#endif
