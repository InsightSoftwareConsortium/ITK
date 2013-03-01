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

#include <cstdlib>
#include <metaLandmark.h>
#include "itksys/SystemTools.hxx"

int testMetaLandmark(int argc, char * argv[])
{
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }

  std::cout << "Creating test file ..." << std::endl;
  MetaLandmark Landmark0;
  MetaLandmark Landmark(3);
  Landmark.ID(0);
  LandmarkPnt* pnt;

  std::cout << "Allocating points..." << std::endl;
  unsigned int i;
  for(i=0;i<10;i++)
  {
    pnt = new LandmarkPnt(3);
    pnt->m_X[0]=(float)0.2;
    pnt->m_X[1]=static_cast<float>(i);
    pnt->m_X[2]=static_cast<float>(i);
    Landmark.GetPoints().push_back(pnt);
  }

  std::cout << "Writing test file ..." << std::endl;

  Landmark.BinaryData(true);
  Landmark.ElementType(MET_FLOAT);
  Landmark.Write("Landmarks.meta");

  std::cout << "  done" << std::endl;

  std::cout << "Reading test file ..." << std::endl;
  Landmark.Read("Landmarks.meta");
  MetaLandmark landmarkRead("Landmarks.meta");
  MetaLandmark landmarkCopy(&landmarkRead);

  std::cout << "PointDim = " << landmarkCopy.PointDim() << std::endl;
  std::cout << "NPoints = "  << landmarkCopy.NPoints() << std::endl;
  std::cout << "ElementType = " << landmarkCopy.ElementType() << std::endl;

  std::cout << "  done" << std::endl;

  Landmark.PrintInfo();

  std::cout << "Accessing pointlist..." << std::endl;

  MetaLandmark::PointListType plist =  Landmark.GetPoints();
  MetaLandmark::PointListType::const_iterator it = plist.begin();

  while(it != plist.end())
  {
    for(unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }

    std::cout << std::endl;
    ++it;
  }

  std::cout << "done" << std::endl;
  return EXIT_SUCCESS;
}
