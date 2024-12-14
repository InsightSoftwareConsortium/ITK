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

#include "itkVectorContainer.h"
#include "itkMapContainer.h"

#include "itkSTLContainerAdaptor.h"
#include "itkSTLConstContainerAdaptor.h"


int
itkSTLContainerAdaptorTest(int, char *[])
{

  using IndexType = unsigned long;
  using ElementType = int;

  unsigned int containerSize = 100;

  // Test with the VectorContainer
  { // create a local scope

    std::cout << "Testing the VectorContainer " << '\n';

    using VectorContainerType = itk::VectorContainer<ElementType>;

    auto vectorContainer = VectorContainerType::New();

    using STLVectorType = std::vector<ElementType>;

    STLVectorType vectorSource;

    for (unsigned int i = 0; i < containerSize; ++i)
    {
      vectorSource.push_back(containerSize - i);
    }

    containerSize = static_cast<unsigned int>(vectorSource.size());

    using AdaptorType = itk::STLContainerAdaptor<VectorContainerType>;
    using TargetType = AdaptorType::TargetType;

    std::cout << "----- Testing non-const Adaptor " << '\n';

    vectorContainer->Print(std::cout);

    { // define a local scope
      AdaptorType  adaptor(vectorContainer);
      TargetType & targetRef = adaptor.GetSTLContainerRef();

      std::cout << "Testing assignment... ";

      targetRef.reserve(vectorSource.size());
      targetRef.assign(vectorSource.begin(), vectorSource.end());

      auto                               it = vectorSource.begin();
      VectorContainerType::ConstIterator cIter = vectorContainer->Begin();
      while (it != vectorSource.end() && cIter != vectorContainer->End())
      {
        if (*it != cIter.Value())
        {
          std::cerr << "Error in comparison !" << '\n';
          return EXIT_FAILURE;
        }
        ++cIter;
        ++it;
      }
      // here, check for premature ending of the while loop
      if (it != vectorSource.end() || cIter != vectorContainer->End())
      {
        std::cerr << "Error, iteration on containers didn't finished simultaneously" << '\n';
      }
      std::cout << "Passed !" << '\n';


      // Test of index access
      std::cout << "Testing index access... ";
      for (unsigned int i = 0; i < containerSize; ++i)
      {
        if (vectorSource[i] != vectorContainer->GetElement(i))
        {
          std::cerr << "Error, comparing element # " << i << '\n';
          return EXIT_FAILURE;
        }
      }
      std::cout << "Passed !" << '\n';
    }

    using ConstAdaptorType = itk::STLConstContainerAdaptor<VectorContainerType>;
    using ConstTargetType = ConstAdaptorType::TargetType;

    std::cout << "----- Testing const Adaptor " << '\n';


    { // define a local scope
      ConstAdaptorType  constAdaptor(vectorContainer);
      ConstTargetType & constTargetRef = constAdaptor.GetSTLConstContainerRef();

      STLVectorType destination;

      std::cout << "Testing reading assignment... ";
      destination.assign(constTargetRef.begin(), constTargetRef.end());

      auto                               it = destination.begin();
      VectorContainerType::ConstIterator cIter = vectorContainer->Begin();
      while (it != destination.end() && cIter != vectorContainer->End())
      {
        if (*it != cIter.Value())
        {
          std::cerr << "Error in comparison !" << '\n';
          return EXIT_FAILURE;
        }
        ++it;
        ++cIter;
      }
      // here, check for premature ending of the while loop
      if (it != destination.end() || cIter != vectorContainer->End())
      {
        std::cerr << "Error, iteration on containers didn't finished simultaneously" << '\n';
      }
      std::cout << "Passed !" << '\n';


      // Test of index access
      std::cout << "Testing index access... ";
      for (unsigned int i = 0; i < containerSize; ++i)
      {
        if (destination[i] != vectorContainer->GetElement(i))
        {
          std::cerr << "Error, comparing element # " << i << '\n';
          return EXIT_FAILURE;
        }
      }
      std::cout << "Passed !" << '\n';
    }


    std::cout << '\n';
    std::cout << "VectorContainer test passed ! " << '\n';
  }

  // Test with the MapContainer

  using MapContainerType = itk::MapContainer<IndexType, ElementType>;

  { // create a local scope

    std::cout << "Testing the MapContainer " << '\n';

    auto mapContainer = MapContainerType::New();

    using STLMapType = std::map<int, ElementType>;
    STLMapType mapSource;

    for (unsigned int i = 0; i < containerSize; ++i)
    {
      mapSource[i] = containerSize - i;
    }


    containerSize = static_cast<unsigned int>(mapSource.size());

    using AdaptorType = itk::STLContainerAdaptor<MapContainerType>;
    using TargetType = AdaptorType::TargetType;

    std::cout << "----- Testing non-const Adaptor " << '\n';

    mapContainer->Print(std::cout);

    { // define a local scope
      AdaptorType  adaptor(mapContainer);
      TargetType & targetRef = adaptor.GetSTLContainerRef();

      std::cout << "Testing assignment... ";

      for (unsigned int i = 0; i < containerSize; ++i)
      {
        targetRef[i] = mapSource[i];
      }

      auto                            it = mapSource.begin();
      MapContainerType::ConstIterator cIter = mapContainer->Begin();
      while (it != mapSource.end() && cIter != mapContainer->End())
      {
        if (it->second != cIter.Value())
        {
          std::cerr << "Error in comparison !" << '\n';
          return EXIT_FAILURE;
        }
        ++cIter;
        ++it;
      }
      // here, check for premature ending of the while loop
      if (it != mapSource.end() || cIter != mapContainer->End())
      {
        std::cerr << "Error, iteration on containers didn't finished simultaneously" << '\n';
      }
      std::cout << "Passed !" << '\n';


      // Test of index access
      std::cout << "Testing index access... ";
      for (unsigned int j = 0; j < containerSize; ++j)
      {
        if (mapSource[j] != mapContainer->GetElement(j))
        {
          std::cerr << "Error, comparing element # " << j << '\n';
          return EXIT_FAILURE;
        }
      }
      std::cout << "Passed !" << '\n';
    }

    using ConstAdaptorType = itk::STLConstContainerAdaptor<MapContainerType>;
    using ConstTargetType = ConstAdaptorType::TargetType;

    std::cout << "----- Testing const Adaptor " << '\n';


    { // define a local scope
      ConstAdaptorType  constAdaptor(mapContainer);
      ConstTargetType & constTargetRef = constAdaptor.GetSTLConstContainerRef();

      STLMapType destination;

      std::cout << "Testing reading assignment... ";
      for (unsigned int i = 0; i < containerSize; ++i)
      {
        destination[i] = constTargetRef.find(i)->second;
      }

      auto                            it = destination.begin();
      MapContainerType::ConstIterator cIter = mapContainer->Begin();
      while (it != destination.end() && cIter != mapContainer->End())
      {
        if (it->second != cIter.Value())
        {
          std::cerr << "Error in comparison !" << '\n';
          return EXIT_FAILURE;
        }
        ++it;
        ++cIter;
      }
      // here, check for premature ending of the while loop
      if (it != destination.end() || cIter != mapContainer->End())
      {
        std::cerr << "Error, iteration on containers didn't finished simultaneously" << '\n';
      }
      std::cout << "Passed !" << '\n';


      // Test of index access
      std::cout << "Testing index access... ";
      for (unsigned int j = 0; j < containerSize; ++j)
      {
        if (destination[j] != mapContainer->GetElement(j))
        {
          std::cerr << "Error, comparing element # " << j << '\n';
          return EXIT_FAILURE;
        }
      }
      std::cout << "Passed !" << '\n';
    }


    std::cout << '\n';
    std::cout << "MapContainer test passed ! " << '\n';
  }

  return EXIT_SUCCESS;
}
