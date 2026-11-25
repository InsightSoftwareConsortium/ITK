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
#include "itkMeshFileReaderException.h"
#include "itkMeshFileWriterException.h"

#include <gtest/gtest.h>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>


namespace
{
bool
Contains(const std::string & text, const std::string & part)
{
  return text.find(part) != std::string::npos;
}


template <typename TException>
void
CheckMeshIOException(const std::string & expectedName)
{
  static_assert(std::is_base_of_v<itk::ExceptionObject, TException>);
  static_assert(std::is_nothrow_destructible_v<TException>);

  const unsigned int line = __LINE__;
  const TException   withDefaults(__FILE__, line);
  EXPECT_EQ(withDefaults.GetNameOfClass(), expectedName);
  EXPECT_STREQ(withDefaults.GetDescription(), "Error in IO");
  EXPECT_STREQ(withDefaults.GetFile(), __FILE__);
  EXPECT_EQ(withDefaults.GetLine(), line);
  EXPECT_STREQ(withDefaults.GetLocation(), "");

  const std::string testMessage = "test message";
  const std::string testLocation = "test location";
  const TException  exception(__FILE__, __LINE__, testMessage, testLocation);
  EXPECT_EQ(exception.GetDescription(), testMessage);
  EXPECT_EQ(exception.GetLocation(), testLocation);

  std::ostringstream stream;
  stream << exception;
  EXPECT_TRUE(Contains(stream.str(), "itk::" + expectedName));
  EXPECT_TRUE(Contains(stream.str(), "Location: \"" + testLocation + '"'));
  EXPECT_TRUE(Contains(stream.str(), "Description: " + testMessage));

  TException copy(exception);
  EXPECT_EQ(copy, exception);

  const TException moved(std::move(copy));
  EXPECT_EQ(moved.GetDescription(), testMessage);

  TException assigned(__FILE__, __LINE__);
  assigned = exception;
  EXPECT_EQ(assigned, exception);

  TException moveAssigned(__FILE__, __LINE__);
  moveAssigned = TException(exception);
  EXPECT_EQ(moveAssigned.GetLocation(), testLocation);

  const itk::ExceptionObject & base = exception;
  EXPECT_EQ(base.GetNameOfClass(), expectedName);
}
} // namespace


TEST(MeshIOExceptions, ReaderAndWriterExceptions)
{
  CheckMeshIOException<itk::MeshFileReaderException>("MeshFileReaderException");
  CheckMeshIOException<itk::MeshFileWriterException>("MeshFileWriterException");
}
