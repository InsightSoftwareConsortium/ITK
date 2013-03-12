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

#include <iostream>
#include <cstdlib>
#include <metaTube.h>
#include <metaScene.h>
#include <metaEllipse.h>
#include "itksys/SystemTools.hxx"

int testMetaTube(int argc, char * argv[])
{
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }

  std::cout << "Initializing scene ..." << std::endl;
  MetaScene myScene = MetaScene(3);

  std::cout << "Creating test file ..." << std::endl;

  //MetaTubeNet* tubenet = new MetaTubeNet();

  // add two tube to the list of tubenet
  std::cout << "  Creating first tube ..." << std::endl;
  MetaTube* tube1 = new MetaTube(3);
  tube1->ID(0);
  TubePnt* pnt;

  unsigned int i;
  for(i=0;i<10;i++)
  {
    pnt = new TubePnt(3);
    pnt->m_X[0]=pnt->m_X[1]=pnt->m_X[2]=static_cast<float>(i);
    pnt->m_R=static_cast<float>(i);
    tube1->GetPoints().push_back(pnt);
  }

  std::cout << "  Creating second tube ..." << std::endl;
  MetaTube* tube2 = new MetaTube(3);
  tube2->ID(1);
  for(i=0;i<5;i++)
  {
    pnt = new TubePnt(3);
    pnt->m_X[0]=pnt->m_X[1]=pnt->m_X[2]=static_cast<float>(i);
    pnt->m_R=static_cast<float>(i);
    tube2->GetPoints().push_back(pnt);
  }

  // Add an ellipse
  std::cout << "  Creating ellipse ..." << std::endl;
  MetaEllipse* ellipse = new MetaEllipse();
  std::cout << "    Initializing ellipse ..." << std::endl;
  ellipse->InitializeEssential(3);
  std::cout << "    Setting radius ..." << std::endl;
  ellipse->Radius(1,2,3);

  myScene.AddObject(tube1);
  myScene.AddObject(tube2);
  myScene.AddObject(ellipse);

  myScene.Write("test.scn");

  std::cout << "done" << std::endl;
  std::cout << "Reading test file ..." << std::endl;

  // Read the result
  MetaScene myScene2 = MetaScene();
  myScene2.InitializeEssential(3);

  std::cout << "  ... reading scene " << std::endl;
  myScene2.Read("test.scn");
  std::cout << "  ... read scene " << std::endl;

  typedef  MetaScene::ObjectListType ListType;
  ListType * list = myScene2.GetObjectList();
  ListType::iterator it = list->begin();

  std::cout << "  ... beginning loop " << std::endl;
  for(i=0;i< list->size();i++)
  {

    (*it)->PrintInfo();
    if(!strncmp((*it)->ObjectTypeName(),"Tube",4))
    {
      typedef MetaTube::PointListType PointListType;
      MetaTube* tube = dynamic_cast<MetaTube*>(*it);
      PointListType::iterator it2 = tube->GetPoints().begin();

      for(unsigned int j=0;j< tube->GetPoints().size();j++)
      {
        std::cout << (*it2)->m_X[0]
        << " " << (*it2)->m_X[1] << " " << (*it2)->m_X[2] << std::endl;
        ++it2;
      }
    }

    ++it;
  }

  std::cout << "done" << std::endl;
  return EXIT_SUCCESS;
}
