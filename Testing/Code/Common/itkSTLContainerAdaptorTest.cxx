/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSTLContainerAdaptorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkVectorContainer.h"
#include "itkMapContainer.h"

#include "itkSTLContainerAdaptor.h"
#include "itkSTLConstContainerAdaptor.h"


int itkSTLContainerAdaptorTest(int, char**)
{

  typedef unsigned long   IndexType;
  typedef int             ElementType;

  const unsigned int containerSize = 100;

  // Test with the VectorContainer
  { // create a local scope

    std::cout << "Testing the VectorContainer " << std::endl;

    typedef itk::VectorContainer<IndexType, ElementType>  VectorContainerType;

    VectorContainerType::Pointer vectorContainer = VectorContainerType::New();

    typedef std::vector<ElementType> STLVectorType;

    STLVectorType  vectorSource;

    for (unsigned int i = 0; i < containerSize; i++) 
      {
      vectorSource.push_back(containerSize - i);
      }

    const unsigned int containerSize = vectorSource.size();

    typedef itk::STLContainerAdaptor<VectorContainerType>       AdaptorType;
    typedef AdaptorType::TargetType                       TargetType;

    std::cout << "----- Testing non-const Adaptor " << std::endl;

    vectorContainer->Print(std::cout);

    { // define a local scope
      AdaptorType adaptor( vectorContainer );
      TargetType & targetRef = adaptor.GetSTLContainerRef();

      std::cout << "Testing assignment... "; 

      targetRef.reserve( vectorSource.size() );
      targetRef.assign( vectorSource.begin(), vectorSource.end() );

      STLVectorType::const_iterator it    = vectorSource.begin();
      VectorContainerType::ConstIterator  cIter = vectorContainer->Begin();
      while( it != vectorSource.end() && cIter != vectorContainer->End() )
        {
        if( *it != cIter.Value() )
          {
          std::cerr << "Error in comparision !" << std::endl;
          return EXIT_FAILURE;
          }
        ++cIter;
        ++it;
        }
      // here, check for premature ending of the while loop
      if( it != vectorSource.end() || cIter != vectorContainer->End() )
        {
        std::cerr << "Error, iteration on containers didn't finished simultaneously" << std::endl;
        }
      std::cout << "Passed !" << std::endl;

      
      // Test of index access
      std::cout << "Testing index access... "; 
      for (unsigned int i = 0; i < containerSize; i++)
        {
        if( vectorSource[i] != vectorContainer->GetElement(i) )
          {
          std::cerr << "Error, comparing element # " << i << std::endl;
          return EXIT_FAILURE;
          }
        }
      std::cout << "Passed !" << std::endl;


    }



    typedef itk::STLConstContainerAdaptor<VectorContainerType>  ConstAdaptorType;
    typedef ConstAdaptorType::TargetType                  ConstTargetType;

    std::cout << "----- Testing const Adaptor " << std::endl;

   
    { // define a local scope
      ConstAdaptorType constAdaptor(vectorContainer);
      ConstTargetType & constTargetRef = constAdaptor.GetSTLConstContainerRef();

      STLVectorType destination;

      std::cout << "Testing reading assignment... "; 
      destination.assign( constTargetRef.begin(), constTargetRef.end() );

      STLVectorType::const_iterator it    = destination.begin();
      VectorContainerType::ConstIterator  cIter = vectorContainer->Begin();
      while( it != destination.end() && cIter != vectorContainer->End() )
        {
        if( *it != cIter.Value() )
          {
          std::cerr << "Error in comparision !" << std::endl;
          return EXIT_FAILURE;
          }
        ++it;
        ++cIter;
        }
      // here, check for premature ending of the while loop
      if( it != destination.end() || cIter != vectorContainer->End() )
        {
        std::cerr << "Error, iteration on containers didn't finished simultaneously" << std::endl;
        }
      std::cout << "Passed !" << std::endl;

      
      // Test of index access
      std::cout << "Testing index access... "; 
      for (unsigned int i = 0; i < containerSize; i++)
        {
        if( destination[i] != vectorContainer->GetElement(i) )
          {
          std::cerr << "Error, comparing element # " << i << std::endl;
          return EXIT_FAILURE;
          }
        }
      std::cout << "Passed !" << std::endl;

    }


  std::cout << std::endl;
  std::cout << "VectorContainer test passed ! " << std::endl;

  }



  // Test with the MapContainer

  typedef itk::MapContainer<IndexType, ElementType>  MapContainerType;

  { // create a local scope

    std::cout << "Testing the MapContainer " << std::endl;

    typedef itk::MapContainer<IndexType, ElementType>  MapContainerType;

    MapContainerType::Pointer mapContainer = MapContainerType::New();

    typedef std::map<int,ElementType>  STLMapType;
    STLMapType mapSource;

    for (unsigned int i = 0; i < containerSize; i++) 
      {
      mapSource[i] = containerSize - i;
    }


    const unsigned int containerSize = mapSource.size();

    typedef itk::STLContainerAdaptor<MapContainerType>       AdaptorType;
    typedef AdaptorType::TargetType                       TargetType;

    std::cout << "----- Testing non-const Adaptor " << std::endl;

    mapContainer->Print(std::cout);

    { // define a local scope
      AdaptorType adaptor( mapContainer );
      TargetType & targetRef = adaptor.GetSTLContainerRef();

      std::cout << "Testing assignment... "; 

      for(unsigned int i=0; i < containerSize; i++)
        {
        targetRef[i] = mapSource[i];
        }

      STLMapType::const_iterator it    = mapSource.begin();
      MapContainerType::ConstIterator  cIter = mapContainer->Begin();
      while( it != mapSource.end() && cIter != mapContainer->End() )
        {
        if( it->second != cIter.Value() )
          {
          std::cerr << "Error in comparision !" << std::endl;
          return EXIT_FAILURE;
          }
        ++cIter;
        ++it;
        }
      // here, check for premature ending of the while loop
      if( it != mapSource.end() || cIter != mapContainer->End() )
        {
        std::cerr << "Error, iteration on containers didn't finished simultaneously" << std::endl;
        }
      std::cout << "Passed !" << std::endl;

      
      // Test of index access
      std::cout << "Testing index access... "; 
      for (unsigned int j = 0; j < containerSize; j++)
        {
        if( mapSource[j] != mapContainer->GetElement(j) )
          {
          std::cerr << "Error, comparing element # " << j << std::endl;
          return EXIT_FAILURE;
          }
        }
      std::cout << "Passed !" << std::endl;


    }



    typedef itk::STLConstContainerAdaptor<MapContainerType>  ConstAdaptorType;
    typedef ConstAdaptorType::TargetType                  ConstTargetType;

    std::cout << "----- Testing const Adaptor " << std::endl;

   
    { // define a local scope
      ConstAdaptorType constAdaptor(mapContainer);
      ConstTargetType & constTargetRef = constAdaptor.GetSTLConstContainerRef();

      STLMapType destination;

      std::cout << "Testing reading assignment... "; 
      for( unsigned int i=0; i < containerSize; i++)
        {
        destination[i] = constTargetRef.find(i)->second;
        }

      STLMapType::const_iterator it    = destination.begin();
      MapContainerType::ConstIterator  cIter = mapContainer->Begin();
      while( it != destination.end() && cIter != mapContainer->End() )
        {
        if( it->second != cIter.Value() )
          {
          std::cerr << "Error in comparision !" << std::endl;
          return EXIT_FAILURE;
          }
        ++it;
        ++cIter;
        }
      // here, check for premature ending of the while loop
      if( it != destination.end() || cIter != mapContainer->End() )
        {
        std::cerr << "Error, iteration on containers didn't finished simultaneously" << std::endl;
        }
      std::cout << "Passed !" << std::endl;

      
      // Test of index access
      std::cout << "Testing index access... "; 
      for (unsigned int j = 0; j < containerSize; j++)
        {
        if( destination[j] != mapContainer->GetElement(j) )
          {
          std::cerr << "Error, comparing element # " << j << std::endl;
          return EXIT_FAILURE;
          }
        }
      std::cout << "Passed !" << std::endl;

    }


  std::cout << std::endl;
  std::cout << "MapContainer test passed ! " << std::endl;

  }

 

  return EXIT_SUCCESS;
}
