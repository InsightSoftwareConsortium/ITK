/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOFileNameExtensionsTests.cxx
  Language:  C++
  Date:      $Date$xgoto-l

  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkImageIOBase.h"

#include <list>

int itkImageIOFileNameExtensionsTests( int , char * [] )
{
  typedef itk::ImageIOBase                        IOBaseType;
  typedef std::list<itk::LightObject::Pointer>    ArrayOfImageIOType;
  typedef IOBaseType::ArrayOfExtensionsType       ArrayOfExtensionsType;

  ArrayOfImageIOType allobjects = itk::ObjectFactoryBase::CreateAllInstance("itkImageIOBase");

  ArrayOfImageIOType::iterator itr = allobjects.begin();

  while( itr != allobjects.end() )
    {

    IOBaseType * io = dynamic_cast< IOBaseType * >( itr->GetPointer() );

    if( ! io )
      {
      std::cerr << "Got a null pointer in the array" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "---------------------------------" << std::endl;
      std::cout << "ImageIO: " << io->GetNameOfClass() << std::endl; 

      const ArrayOfExtensionsType & readExtensions  = io->GetSupportedReadExtensions();
      const ArrayOfExtensionsType & writeExtensions = io->GetSupportedWriteExtensions();

      ArrayOfExtensionsType::const_iterator readItr  = readExtensions.begin();
      ArrayOfExtensionsType::const_iterator writeItr = writeExtensions.begin();

      std::cout << "Supported Read Extensions" << std::endl;
      while( readItr != readExtensions.end() )
        {
        std::cout << *readItr << std::endl;
        ++readItr;
        }

      std::cout << "Supported Write Extensions" << std::endl;
      while( writeItr != writeExtensions.end() )
        {
        std::cout << *writeItr << std::endl;
        ++writeItr;
        }

      }
    ++itr;
    }

  return EXIT_SUCCESS;
}
