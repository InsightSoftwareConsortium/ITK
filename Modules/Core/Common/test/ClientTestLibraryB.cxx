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

#include "ClientTestLibraryB.h"

#include "itkEquivalencyTable.h"
#include "itkImage.h"

namespace
{

template< typename TDerived >
int
dynamic_castDownCast( const char * type, const char * instanceSource, itk::Object const * base )
{
  typedef TDerived DerivedType;

  const static int passed = 0;
  const static int failed = 1;

  DerivedType const * derived = dynamic_cast< DerivedType const * >( base );
  if( derived != ITK_NULLPTR )
    {
    std::cout << type << " cast in library B      for an instance from " << instanceSource << "\tsucceeded." << std::endl;
    return passed;
    }
  std::cerr << type << " cast in library B      for an instance from " << instanceSource << "\tfailed!" << std::endl;
  return failed;
}

} // end anonymous namespace

namespace LibraryB
{

ITKObjectProducer
::ITKObjectProducer()
{
  m_EquivalencyTable = itk::EquivalencyTable::New();
  typedef itk::Image< float, 3 > ImageType;
  m_Image = ImageType::New();
}


itk::Object *
ITKObjectProducer
::EquivalencyTable()
{
  return m_EquivalencyTable.GetPointer();
}


itk::Object *
ITKObjectProducer
::Image()
{
  return m_Image.GetPointer();
}

} // end namespace LibraryB


int
dynamic_castDownCastEquivalencyTable( const char * type, const char * instanceSource, itk::Object const * base )
{
  typedef itk::EquivalencyTable EquivalencyTableType;
  return dynamic_castDownCast< EquivalencyTableType >( type, instanceSource, base );
}


int
dynamic_castDownCastImage( const char * type, const char * instanceSource, itk::Object const * base )
{
  typedef itk::Image< float, 3 > ImageType;
  return dynamic_castDownCast< ImageType >( type, instanceSource, base );
}


static LibraryB::ITKObjectProducer staticITKObjectProducer;

itk::Object * EquivalencyTable()
{
  return staticITKObjectProducer.EquivalencyTable();
}


itk::Object * Image()
{
  return staticITKObjectProducer.Image();
}
