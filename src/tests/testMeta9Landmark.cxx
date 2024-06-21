#include <iostream>

#include <metaLandmark.h>

int
main(int, char *[])
{
  std::cout << "Creating test file ..." << '\n';
  MetaLandmark Landmark(3);
  Landmark.ID(0);
  LandmarkPnt * pnt;

  std::cout << "Allocating points..." << '\n';
  unsigned int i;
  for (i = 0; i < 10; i++)
  {
    pnt = new LandmarkPnt(3);
    pnt->m_X[0] = static_cast<float>(0.2);
    pnt->m_X[1] = static_cast<float>(i);
    pnt->m_X[2] = static_cast<float>(i);
    Landmark.GetPoints().push_back(pnt);
  }

  std::cout << "Writing test file ..." << '\n';

  Landmark.BinaryData(true);
  Landmark.ElementType(MET_FLOAT);
  Landmark.Write("Landmarks.meta");

  std::cout << "  done" << '\n';

  std::cout << "Reading test file ..." << '\n';
  Landmark.Read("Landmarks.meta");
  MetaLandmark landmarkRead("Landmarks.meta");
  MetaLandmark landmarkCopy(&landmarkRead);

  std::cout << "PointDim = " << landmarkCopy.PointDim() << '\n';
  std::cout << "NPoints = " << landmarkCopy.NPoints() << '\n';
  std::cout << "ElementType = " << landmarkCopy.ElementType() << '\n';

  std::cout << "  done" << '\n';

  Landmark.PrintInfo();

  std::cout << "Accessing pointlist..." << '\n';

  MetaLandmark::PointListType                 plist = Landmark.GetPoints();
  MetaLandmark::PointListType::const_iterator it = plist.begin();

  while (it != plist.end())
  {
    for (unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }

    std::cout << '\n';
    ++it;
  }

  std::cout << "done" << '\n';
  return EXIT_SUCCESS;
}
