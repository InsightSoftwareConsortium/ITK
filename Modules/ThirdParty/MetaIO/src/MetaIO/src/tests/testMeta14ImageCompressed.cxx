// Regression tests for reading compressed element data.

#include <cstdio>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <metaImage.h>

namespace
{

const char * const localName = "testMeta14_local.mha";

// Write a compressed single-file (LOCAL) image, then strip CompressedDataSize
// from its header so the reader has to determine the size itself.
bool
WriteLocalWithoutCompressedDataSize(int quantity)
{
  {
    std::vector<unsigned char> values(static_cast<size_t>(quantity));
    for (int i = 0; i < quantity; ++i)
    {
      values[static_cast<size_t>(i)] = static_cast<unsigned char>(i % 251);
    }
    MetaImage image(quantity, 1, 1, 1, MET_UCHAR, 1, values.data());
    image.CompressedData(true);
    if (!image.Write(localName))
    {
      return false;
    }
  }

  std::ifstream in(localName, std::ios::binary);
  std::string   contents((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
  in.close();

  const std::string key = "CompressedDataSize = ";
  const size_t      at = contents.find(key);
  if (at == std::string::npos)
  {
    std::cout << "  no CompressedDataSize field was written" << '\n';
    return false;
  }
  const size_t eol = contents.find('\n', at);
  contents.erase(at, eol - at + 1);

  std::ofstream out(localName, std::ios::binary);
  out.write(contents.data(), static_cast<std::streamsize>(contents.size()));
  return true;
}

} // namespace

int
main(int, char *[])
{
  const int quantity = 4096;

  // A LOCAL image whose header omits CompressedDataSize: the compressed data
  // starts after the header, not at the start of the file.
  if (!WriteLocalWithoutCompressedDataSize(quantity))
  {
    std::cout << "Could not prepare the LOCAL test image: FAIL" << '\n';
    return EXIT_FAILURE;
  }
  {
    MetaImage image;
    if (!image.Read(localName))
    {
      std::cout << "LOCAL image without CompressedDataSize failed to read: FAIL" << '\n';
      return EXIT_FAILURE;
    }
    for (int i = 0; i < quantity; ++i)
    {
      const int expected = i % 251;
      if (static_cast<int>(image.ElementData(i)) != expected)
      {
        std::cout << "  element " << i << " is " << image.ElementData(i) << ", expected " << expected << '\n';
        std::cout << "LOCAL image without CompressedDataSize read wrong values: FAIL" << '\n';
        return EXIT_FAILURE;
      }
    }
  }

  // Truncating the compressed stream must be reported, not silently accepted.
  {
    std::ifstream in(localName, std::ios::binary);
    std::string   contents((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
    in.close();
    contents.resize(contents.size() - 16);
    std::ofstream out("testMeta14_truncated.mha", std::ios::binary);
    out.write(contents.data(), static_cast<std::streamsize>(contents.size()));
    out.close();

    MetaImage image;
    if (image.Read("testMeta14_truncated.mha"))
    {
      std::cout << "Truncated compressed data reported success: FAIL" << '\n';
      return EXIT_FAILURE;
    }
  }

  std::remove(localName);
  std::remove("testMeta14_truncated.mha");

  std::cout << "Compressed element data tests: PASS" << '\n';
  return EXIT_SUCCESS;
}
