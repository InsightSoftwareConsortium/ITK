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

// First include the header files to be tested:
#include "itkImageFileReaderException.h"
#include "itkImageFileWriter.h"
#include "itkImageSeriesWriter.h"

#include <gtest/gtest.h>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>


namespace
{
const std::string testMessage = "test message";
const std::string testLocation = "test location";


bool
Contains(const std::string & text, const std::string & part)
{
  return text.find(part) != std::string::npos;
}


// Checks the behavior shared by the IO exceptions, which are constructed from a file, a line, and a message.
template <typename TException>
void
CheckIOException(const std::string & expectedName)
{
  static_assert(std::is_base_of_v<itk::ExceptionObject, TException>);
  static_assert(std::is_nothrow_destructible_v<TException>);

  const unsigned int line = __LINE__;
  const TException   withDefaultMessage(__FILE__, line);
  EXPECT_EQ(withDefaultMessage.GetNameOfClass(), expectedName);
  EXPECT_STREQ(withDefaultMessage.GetDescription(), "Error in IO");
  EXPECT_STREQ(withDefaultMessage.GetFile(), __FILE__);
  EXPECT_EQ(withDefaultMessage.GetLine(), line);

  const TException withMessage(__FILE__, __LINE__, testMessage);
  EXPECT_EQ(withMessage.GetDescription(), testMessage);

  std::ostringstream stream;
  stream << withMessage;
  EXPECT_TRUE(Contains(stream.str(), "itk::" + expectedName));
  EXPECT_TRUE(Contains(stream.str(), "Description: " + testMessage));

  TException copy(withMessage);
  EXPECT_EQ(copy, withMessage);

  const TException moved(std::move(copy));
  EXPECT_EQ(moved.GetDescription(), testMessage);

  TException assigned(__FILE__, __LINE__);
  assigned = withMessage;
  EXPECT_EQ(assigned, withMessage);

  const itk::ExceptionObject & base = withMessage;
  EXPECT_EQ(base.GetNameOfClass(), expectedName);
}


// Checks the IO exceptions that also accept a location.
template <typename TException>
void
CheckIOExceptionWithLocation(const std::string & expectedName)
{
  CheckIOException<TException>(expectedName);

  const TException exception(__FILE__, __LINE__, testMessage, testLocation);
  EXPECT_EQ(exception.GetLocation(), testLocation);

  TException moveAssigned(__FILE__, __LINE__);
  moveAssigned = TException(exception);
  EXPECT_EQ(moveAssigned.GetLocation(), testLocation);
}
} // namespace


TEST(ImageIOExceptions, ReaderAndWriterExceptions)
{
  CheckIOExceptionWithLocation<itk::ImageFileReaderException>("ImageFileReaderException");
  CheckIOExceptionWithLocation<itk::ImageFileWriterException>("ImageFileWriterException");
  CheckIOException<itk::ImageSeriesWriterException>("ImageSeriesWriterException");
}
