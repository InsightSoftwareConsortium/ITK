/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.*/

/* Script to move ITK_DISALLOW_COPY_AND_MOVE calls to the public section
 * of the classes.
 *
 * Initial version by Niels Dekker, LKEB, Leiden University Medical Center, 2018

 * Discourse discussion: https://discourse.itk.org/t/noncopyable/648/24
 * compiling:
 * g++ Move_DISALLOW_COPY_to_public_section.cpp -lstdc++fs
 * clang++ Move_DISALLOW_COPY_to_public_section.cpp -lstdc++fs
 * Tested 29/03/2018 with:
 * g++ --version : 7.3.1
 * clang++ --version : 7.0.0 */

#include <cassert>
#include <cctype>
#include <deque>
#include <experimental/filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <cstring>
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

std::string
ExtractClassName(std::string candidateClassName)
{
  for (auto & c : candidateClassName)
  {
    if (c == ':')
    {
      c = '\0';
      return candidateClassName.c_str();
    }
    if (c == '<')
    {
      c = '\0';
      return candidateClassName.c_str();
    }
    if (!std::isalnum(c))
    {
      return {};
    }
  }
  return candidateClassName;
}


std::string
FindClassName(const char * const line)
{
  std::istringstream inputStringStream{ line };

  std::string candidateClassName;
  inputStringStream >> candidateClassName;

  const char exportPostfix[] = "_EXPORT";

  if (candidateClassName.size() >= sizeof(exportPostfix) &&
      candidateClassName.compare(
        candidateClassName.size() + 1 - sizeof(exportPostfix), sizeof(exportPostfix) - 1, exportPostfix) == 0)
  {
    inputStringStream >> candidateClassName;
  }
  return ExtractClassName(candidateClassName);
}

const char *
GoToFirstNonSpace(const char * ptr)
{
  while (*ptr == ' ')
  {
    ++ptr;
  }
  return ptr;
}

template <unsigned int VLength>
bool
StringStartsWithPrefix(const char *& str, const char (&prefix)[VLength])
{
  assert(prefix[VLength - 1] == '\0');
  if ((std::strlen(str) + 1 >= VLength) && (std::memcmp(str, prefix, VLength - 1) == 0))
  {
    // Move the 'str' pointer beyond the prefix.
    str += VLength - 1;
    return true;
  }
  return false;
}

struct Statistics
{
  unsigned int numberOfClassNameMismatches;
  unsigned int numberOfDisallowMacroCalls;
  unsigned int numberOfMissingPublicSections;
  unsigned int numberOfSuccessfullMoves;
  unsigned int numberOfDisallowMacroCallsAlreadyAtRightPlace;
};

Statistics
ModifyLines(Lines & lines)
{
  Statistics statistics = {};

  auto className = std::string{};
  auto publicLineNumber = Lines::size_type{};

  for (auto lineNumber = Lines::size_type{}; lineNumber < lines.size(); ++lineNumber)
  {
    const auto & line = lines[lineNumber];
    const auto   numberOfChars = line.size();

    if (numberOfChars > 0)
    {
      const char * c_str = GoToFirstNonSpace(line.c_str());

      if (StringStartsWithPrefix(c_str, "class") && (*c_str == ' '))
      {
        const auto newClassName = FindClassName(c_str);

        if (!newClassName.empty())
        {
          className = newClassName;
          publicLineNumber = 0;
        }
      }
      else
      {
        if ((publicLineNumber == 0) && (!className.empty()) && StringStartsWithPrefix(c_str, "public"))
        {
          if ((*GoToFirstNonSpace(c_str) == ':') && (*GoToFirstNonSpace(c_str + 1) == '\0'))
          {
            // Found the first 'public' section of the class named 'className'.
            publicLineNumber = lineNumber;
          }
        }
        else
        {
          if (StringStartsWithPrefix(c_str, "ITK_DISALLOW_COPY_AND_MOVE(") ||
              StringStartsWithPrefix(c_str, "ITK_DISALLOW_COPY_AND_ASSIGN("))
          {
            ++statistics.numberOfDisallowMacroCalls;

            if (publicLineNumber > 0)
            {
              assert(!className.empty());

              // Found a DISALLOW macro call, somewhere after a 'public' section of the class named 'className'.

              const char c = *GoToFirstNonSpace(c_str);

              if (c_str == (className + ");"))
              {
                if (lineNumber == publicLineNumber + 1)
                {
                  std::cout << "Macro call already at the right place: " << lines[lineNumber] << std::endl;
                  ++statistics.numberOfDisallowMacroCallsAlreadyAtRightPlace;
                }
                else
                {
                  {
                    // Move the DISALLOW macro call up to the begin of the 'public' section.
                    std::string disallowMacroCall = std::move(lines[lineNumber]);

                    for (auto i = lineNumber; i > publicLineNumber + 1; --i)
                    {
                      lines[i] = std::move(lines[i - 1]);
                    }
                    lines[publicLineNumber + 1] = std::move(disallowMacroCall);
                  }

                  if ((lines.size() > (lineNumber + 1)) && lines[lineNumber + 1].empty())
                  {
                    // Ensure that there is no empty line left below the original DISALLOW macro call location.
                    lines.erase(lines.begin() + lineNumber + 1);
                  }

                  if ((lines.size() > (lineNumber + 1)) && *GoToFirstNonSpace(lines[lineNumber + 1].c_str()) == '}')
                  {
                    auto ptr = GoToFirstNonSpace(lines[lineNumber].c_str());
                    if (StringStartsWithPrefix(ptr, "private"))
                    {
                      if ((*GoToFirstNonSpace(ptr) == ':') && (*GoToFirstNonSpace(ptr + 1) == '\0'))
                      {
                        // Erase empty private section.
                        lines.erase(lines.begin() + lineNumber);

                        if (lines[lineNumber - 1].empty())
                        {
                          // Erase empty line before the erased private section.
                          lines.erase(lines.begin() + lineNumber - 1);
                        }
                      }
                    }
                  }
                  if (!lines[publicLineNumber + 2].empty())
                  {
                    // Ensure that there is an empty line below the new DISALLOW macro call location.
                    lines.insert(lines.begin() + publicLineNumber + 2, std::string{});
                  }

                  // Reset local variables for the next iteration.
                  className = std::string{};
                  publicLineNumber = Lines::size_type{};
                  ++statistics.numberOfSuccessfullMoves;
                }
              }
              else
              {
                ++statistics.numberOfClassNameMismatches;
                std::cerr << "Mismatch! Class name: \"" << className << "\"; macro call:  " << lines[lineNumber]
                          << std::endl;
              }
            }
            else
            {
              ++statistics.numberOfMissingPublicSections;
              std::cerr << "No public section found for macro call " << lines[lineNumber] << std::endl;
            }
          }
        }
      }
    }
  }
  return statistics;
}

auto
ProcessFile(const path & filePath)
{
  auto       lines = ReadFile(filePath);
  const auto statistics = ModifyLines(lines);

  if (statistics.numberOfSuccessfullMoves > 0)
  {
    WriteFile(filePath, lines);
  }
  return statistics;
}

void
ProcessDirectory(const path & directoryPath)
{
  Statistics                         statistics = {};
  const recursive_directory_iterator end;

  unsigned int numberOfModifiedFiles = 0;

  for (recursive_directory_iterator it{ directoryPath }; it != end; ++it)
  {
    const auto & path = it->path();
    const auto & extension = path.extension();

    if ((!extension.empty()) && (extension.string() == ".h" || extension.string() == ".cxx") && is_regular_file(path))
    {
      const auto statisticsPerFile = ProcessFile(path);

      if (statisticsPerFile.numberOfDisallowMacroCalls > 0)
      {
        numberOfModifiedFiles += (statisticsPerFile.numberOfSuccessfullMoves > 0) ? 1 : 0;

        if ((statisticsPerFile.numberOfDisallowMacroCalls > 1) ||
            (statisticsPerFile.numberOfDisallowMacroCalls != statisticsPerFile.numberOfSuccessfullMoves))
          std::cout << statisticsPerFile.numberOfDisallowMacroCalls << ' ' << statisticsPerFile.numberOfSuccessfullMoves
                    << ' ' << statisticsPerFile.numberOfDisallowMacroCallsAlreadyAtRightPlace << ' '
                    << statisticsPerFile.numberOfClassNameMismatches << ' '
                    << statisticsPerFile.numberOfMissingPublicSections << ' ' << path << std::endl;
      }
      statistics.numberOfDisallowMacroCalls += statisticsPerFile.numberOfDisallowMacroCalls;
      statistics.numberOfSuccessfullMoves += statisticsPerFile.numberOfSuccessfullMoves;
      statistics.numberOfDisallowMacroCallsAlreadyAtRightPlace +=
        statisticsPerFile.numberOfDisallowMacroCallsAlreadyAtRightPlace;
      statistics.numberOfClassNameMismatches += statisticsPerFile.numberOfClassNameMismatches;
      statistics.numberOfMissingPublicSections += statisticsPerFile.numberOfMissingPublicSections;
    }
  }
  std::cout << "numberOfModifiedFiles:\t" << numberOfModifiedFiles << "\nDisallowMacroCalls:\t"
            << statistics.numberOfDisallowMacroCalls << "\nSuccessfullMoves:\t" << statistics.numberOfSuccessfullMoves
            << "\nDisallowMacroCallsAlreadyAtRightPlace:\t" << statistics.numberOfDisallowMacroCallsAlreadyAtRightPlace
            << "\nClassNameMismatches:\t" << statistics.numberOfClassNameMismatches << "\nMissingPublicSections:\t"
            << statistics.numberOfMissingPublicSections << std::endl;
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
