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

#include <metaBlob.h>
#include <cstdlib>
#include "itksys/SystemTools.hxx"

int testMetaBlob(int argc, char * argv[])
{
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }

  std::cout << "Testing default constructor ..." <<  std::endl;
  MetaBlob blob1;

  std::cout << "Creating test file ..." << std::endl;
  MetaBlob blob(3);
  blob.ID(0);
  BlobPnt* pnt;

  std::cout << "Allocating points..." << std::endl;
  unsigned int i;
  for(i=0;i<10;i++)
  {
    pnt = new BlobPnt(3);
    pnt->m_X[0]=(float)0.2;
    pnt->m_X[1]=static_cast<float>(i);
    pnt->m_X[2]=static_cast<float>(i);
    blob.GetPoints().push_back(pnt);
  }

  std::cout << "Writing test file ..." << std::endl;

  blob.BinaryData(true);
  blob.ElementType(MET_FLOAT);
  blob.Write("myCNC.meta");

  std::cout << "  done" << std::endl;

  std::cout << "Reading test file ..." << std::endl;
  MetaBlob blobRead("myCNC.meta"); // coverage
  blob.Read("myCNC.meta");
  MetaBlob blobCopy(&blob);

  std::cout << "NPoints = " << blobCopy.NPoints() << std::endl;
  std::cout << "PointDim = " << blobCopy.PointDim() << std::endl;
  std::cout << "ElementType = " << blobCopy.ElementType() << std::endl;

  std::cout << "  done" << std::endl;

  blob.PrintInfo();

  std::cout << "Accessing pointlist..." << std::endl;

  MetaBlob::PointListType plist =  blob.GetPoints();
  MetaBlob::PointListType::const_iterator it = plist.begin();

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
