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

// Script to add in-class {} member initializers to ITK classes that have one or more virtual member function,
// itkNewMacro, itkSimpleNewMacro, or itkCreateAnotherMacro calls.
//
// Tested with Visual Studio 2022 (C++14).
//
// Needs to have the directory path to the ITK sources as command-line argument, for example:
//
//     AddEmptyDefaultMemberInitializers.exe D:\src\ITK\Modules
//
// Initial version by Niels Dekker, LKEB, Leiden University Medical Center, 2023

#define _SILENCE_EXPERIMENTAL_FILESYSTEM_DEPRECATION_WARNING

#include <cassert>
#include <cctype>
#include <cstring>
#include <deque>
#include <experimental/filesystem>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <string>

using namespace std::experimental::filesystem::v1;

namespace
{
using Lines = std::deque<std::string>;

auto
ReadFile(const path & filePath)
{
  Lines result;

  std::ifstream inputFileStream{ filePath };
  std::string   line;
  while (std::getline(inputFileStream, line))
  {
    result.push_back(line);
  }
  return result;
}

void
WriteFile(const path & filePath, const Lines & lines)
{
  std::ofstream outputFileStream{ filePath };
  for (const auto & line : lines)
  {
    outputFileStream << line << '\n';
  }
}

template <unsigned int VLength>
bool
StringStartsWith(const std::string & str, const char (&substr)[VLength])
{
  assert(substr[VLength - 1] == '\0');
  return str.compare(0, VLength - 1, substr) == 0;
}

template <unsigned int VLength>
bool
StringEndsWith(const std::string & str, const char (&substr)[VLength])
{
  assert(substr[VLength - 1] == '\0');
  return str.size() >= VLength && str.compare(str.size() + 1 - VLength, VLength - 1, substr) == 0;
}

unsigned int
AddInitializers(Lines & lines)
{
  auto numberOfAddedInitializers = 0u;

  bool shouldThisClassHaveInitializers{ false };

  for (auto & line : lines)
  {
    const auto lineStartsWith = [&line](const auto & substr) { return StringStartsWith(line, substr); };
    const auto lineEndsWith = [&line](const auto & substr) { return StringEndsWith(line, substr); };
    const auto lineContains = [&line](const auto & substr) { return line.find(substr) != std::string::npos; };

    const auto numberOfChars = line.size();

    if (numberOfChars > 0)
    {
      if (lineStartsWith("  itkNewMacro(") || lineStartsWith("  itkSimpleNewMacro(") ||
          lineStartsWith("  itkCreateAnotherMacro(") || lineStartsWith("  virtual ") || lineEndsWith(" override") ||
          lineEndsWith(" override;") || lineEndsWith(" override = default;"))
      {
        // We assume that a class should *certainly* have in-class data member initializers when it has a call to one
        // of those macro's (itkNewMacro, itkSimpleNewMacro, or itkCreateAnotherMacro), or when it has a virtual member
        // function. The creation of an object of such a class is already quite expensive (relative to a simple data
        // struct), so the extra potential performance penalty of initializing all of its data should be neglectable.
        shouldThisClassHaveInitializers = true;
      }
      else
      {
        if (shouldThisClassHaveInitializers)
        {
          if (lineStartsWith("};"))
          {
            // Class definition ended here.
            shouldThisClassHaveInitializers = false;
          }
          else
          {
            // Note: Data members of type std::unique_ptr<T> or itk::SmartPointer<T> are excluded from automatically
            // adding initializers, because they _might_ get compilation errors from GCC (prior to GCC release 9.2) when
            // the template argument T is an incomplete type (a forwarded class) as addressed by Simon Rit:
            // - pull request https://github.com/InsightSoftwareConsortium/ITK/pull/3877
            // - commit eac289d25221d8e080a0ad6d2e6ce6ff0a2c576a
            // - "COMP: Remove in-class {} member initializers of unique_ptr"
            // - pull request https://github.com/InsightSoftwareConsortium/ITK/pull/3927
            // - commit f5f83678755319e35c225bfe563fb26ef1d0197e
            // - "COMP: Remove in class init of SmartPointer of forward declaration"
            // Data members of a C++ reference type (" & m_") and `static` data members are also excluded.
            // `m_ColorTable` was excluded, to avoid GCC warnings saying "const/copy propagation disabled " and " GCSE
            // disabled " [-Wdisabled-optimization], at line
            // https://github.com/InsightSoftwareConsortium/ITK/blob/v5.3.0/Modules/Core/Common/include/itkOctree.h#L222

            if (lineStartsWith("  ") && lineContains(" m_") && !lineContains(" & m_") && !lineContains(" static ") &&
                !lineContains(" std::unique_ptr<") && !lineContains("SmartPointer<") &&
                !lineContains("m_ColorTable[ColorTableSize];"))
            {
              // The regular expression explicitly skips lines of code that have a `)` at the end, like
              // `  itkSetVectorMacro(NumberOfIterations, unsigned int, m_NumberOfLevels);`, from
              // https://github.com/InsightSoftwareConsortium/ITK/blob/v5.3.0/Modules/Registration/PDEDeformable/include/itkMultiResolutionPDEDeformableRegistration.h#L224

              if (std::regex_match(line, std::regex{ R"delimiter(  \w.* m_\w([^{}=]*[^{}=\)])?;)delimiter" }))
              {
                line.pop_back();
                line.append("{};");
                std::cout << line << '\n';
                ++numberOfAddedInitializers;
              }
              else
              {
                // Address lines of code that have a trailing C++ comment.
                std::match_results<std::string::const_iterator> results;
                if (std::regex_match(
                      line,
                      results,
                      std::regex{ R"delimiter((  \w.* m_\w(?:[^{}=]*[^{}=\)])?);([ ]+/[/\*].+))delimiter" }) &&
                    results.size() == 3)
                {
                  line = results[1].str() + "{};" + results[2].str();
                  std::cout << line << '\n';
                  ++numberOfAddedInitializers;
                }
              }
            }
          }
        }
      }
    }
  }
  return numberOfAddedInitializers;
}

auto
ProcessFile(const path & filePath)
{
  auto       lines = ReadFile(filePath);
  const auto numberOfAddedInitializers = AddInitializers(lines);
  if (numberOfAddedInitializers > 0)
  {
    std::cout << "Added " << numberOfAddedInitializers << " initializer(s) to " << filePath.string() << std::endl;
    WriteFile(filePath, lines);
  }
  return numberOfAddedInitializers;
}

void
ProcessDirectory(const path & directoryPath)
{
  auto numberOfAddedInitializers = 0u;

  for (const auto & entry : recursive_directory_iterator{ directoryPath })
  {
    const auto & path = entry.path();
    const auto & extension = path.extension();

    if ((!extension.empty()) && extension.string() == ".h" && StringStartsWith(path.stem().string(), "itk") &&
        is_regular_file(path))
    {
      numberOfAddedInitializers += ProcessFile(path);
    }
  }
  std::cout << "Added in total " << numberOfAddedInitializers << " initializer(s) to " << directoryPath.string()
            << std::endl;
}
} // namespace


int
main(int argc, char ** argv)
{
  if (argc != 2)
  {
    std::cout << "Please specify the source directory path as command-line argument."
                 "\nNote: This program will modify the source files in-place!!!"
              << std::endl;
  }
  else
  {
    if (argv == nullptr)
    {
      return EXIT_FAILURE;
    }
    const char * const arg = argv[1];

    if (arg == nullptr)
    {
      return EXIT_FAILURE;
    }
    ProcessDirectory(arg);
  }

  std::cout << "Press anything to continue" << std::endl;
  std::cin.get();
  return EXIT_SUCCESS;
}
