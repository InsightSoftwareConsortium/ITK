// Regression tests for the "ElementDataFile = LIST" reader.

#include <cstdio>
#include <fstream>
#include <iostream>
#include <string>

#include <metaImage.h>

namespace
{

const char * const slice0 = "testMeta13_s0.raw";
const char * const slice1 = "testMeta13_s1.raw";

void
WriteSlice(const char * name, unsigned char value)
{
  std::ofstream out(name, std::ios::binary);
  const unsigned char buf[4] = { value, value, value, value };
  out.write(reinterpret_cast<const char *>(buf), 4);
}

// A 2x2x2 MET_UCHAR volume whose slices come from a LIST.  "listBody" supplies
// the ElementDataFile value and the file names that follow it.
void
WriteHeader(const std::string & name, const std::string & listBody)
{
  std::ofstream out(name.c_str());
  out << "ObjectType = Image\n"
      << "NDims = 3\n"
      << "DimSize = 2 2 2\n"
      << "ElementType = MET_UCHAR\n"
      << "ElementSpacing = 1 1 1\n"
      << "ElementByteOrderMSB = False\n"
      << listBody;
}

bool
SlicesAreOneThenTwo(MetaImage & image)
{
  for (int i = 0; i < 8; ++i)
  {
    const int expected = (i < 4) ? 1 : 2;
    if (static_cast<int>(image.ElementData(i)) != expected)
    {
      std::cout << "  element " << i << " is " << image.ElementData(i) << ", expected " << expected << '\n';
      return false;
    }
  }
  return true;
}

} // namespace

int
main(int, char *[])
{
  WriteSlice(slice0, 1);
  WriteSlice(slice1, 2);

  const std::string bothSlices = std::string(slice0) + "\n" + slice1 + "\n";

  // A well-formed list reads both slices.
  WriteHeader("testMeta13_valid.mhd", "ElementDataFile = LIST\n" + bothSlices);
  {
    MetaImage image;
    if (!image.Read("testMeta13_valid.mhd"))
    {
      std::cout << "Well-formed LIST failed to read: FAIL" << '\n';
      return EXIT_FAILURE;
    }
    if (!SlicesAreOneThenTwo(image))
    {
      std::cout << "Well-formed LIST read wrong values: FAIL" << '\n';
      return EXIT_FAILURE;
    }
  }

  // A list naming fewer files than DimSize requires must not report success,
  // because the tail of the buffer is never written.
  WriteHeader("testMeta13_short.mhd", std::string("ElementDataFile = LIST\n") + slice0 + "\n");
  {
    MetaImage image;
    if (image.Read("testMeta13_short.mhd"))
    {
      std::cout << "Short LIST reported success: FAIL" << '\n';
      return EXIT_FAILURE;
    }
  }

  // An out-of-range file dimension must fall back to NDims - 1 rather than
  // indexing m_DimSize/m_SubQuantity outside [0, NDims).
  const char * const outOfRange[] = { "ElementDataFile = LIST -1\n", "ElementDataFile = LIST 3\n" };
  for (const char * const listType : outOfRange)
  {
    WriteHeader("testMeta13_range.mhd", listType + bothSlices);
    MetaImage image;
    if (!image.Read("testMeta13_range.mhd"))
    {
      std::cout << "Out-of-range file dimension failed to read: FAIL (" << listType << ')' << '\n';
      return EXIT_FAILURE;
    }
    if (!SlicesAreOneThenTwo(image))
    {
      std::cout << "Out-of-range file dimension read wrong values: FAIL (" << listType << ')' << '\n';
      return EXIT_FAILURE;
    }
  }

  std::remove(slice0);
  std::remove(slice1);
  std::remove("testMeta13_valid.mhd");
  std::remove("testMeta13_short.mhd");
  std::remove("testMeta13_range.mhd");

  std::cout << "LIST element data file tests: PASS" << '\n';
  return EXIT_SUCCESS;
}
