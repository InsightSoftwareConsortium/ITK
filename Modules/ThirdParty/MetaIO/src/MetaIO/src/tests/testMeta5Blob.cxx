#include <iostream>

#include <metaBlob.h>

int
main(int, char *[])
{

  std::cout << "Testing default constructor ..." << '\n';
  MetaBlob blob1;

  std::cout << "Creating test file ..." << '\n';
  MetaBlob blob(3);
  blob.ID(0);
  BlobPnt * pnt;

  std::cout << "Allocating points..." << '\n';
  unsigned int i;
  for (i = 0; i < 10; i++)
  {
    pnt = new BlobPnt(3);
    pnt->m_X[0] = static_cast<float>(0.2);
    pnt->m_X[1] = static_cast<float>(i);
    pnt->m_X[2] = static_cast<float>(i);
    blob.GetPoints().push_back(pnt);
  }

  std::cout << "Writing test file ..." << '\n';

  blob.BinaryData(true);
  blob.ElementType(MET_FLOAT);
  blob.Write("myCNC.meta");

  std::cout << "  done" << '\n';

  std::cout << "Reading test file ..." << '\n';
  MetaBlob blobRead("myCNC.meta"); // coverage
  blob.Read("myCNC.meta");
  MetaBlob blobCopy(&blob);

  std::cout << "NPoints = " << blobCopy.NPoints() << '\n';
  std::cout << "PointDim = " << blobCopy.PointDim() << '\n';
  std::cout << "ElementType = " << blobCopy.ElementType() << '\n';

  std::cout << "  done" << '\n';

  blob.PrintInfo();

  std::cout << "Accessing pointlist..." << '\n';

  MetaBlob::PointListType                 plist = blob.GetPoints();
  MetaBlob::PointListType::const_iterator it = plist.begin();

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
