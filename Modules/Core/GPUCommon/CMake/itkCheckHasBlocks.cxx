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


// Check if blocks can be used for the GPU implementation.
// Blocks are a nonstandard extension added by Apple to their
// implementations of the C, C++, and Objective-C programming languages.
// Apple has implemented blocks both in their own branch of the
// GNU Compiler Collection and in the Clang LLVM compiler front end.
// For example macports gcc will not support Blocks on OS X because
// it does not use Apple's headers.
// You can recompile your gcc on OS X if needed, using:
// --with-native-system-header-dir=/usr/include
// --with-sysroot=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk

// This code was tested on OS X 10.10 with:
// Apple LLVM version 6.0 (clang-600.0.54): Compiles
// Homebrew gcc 4.9.2: Compiles
// Macports gcc 4.9.2: Fails
// Macports gcc 4.9.2: with native headers : Compiles

int main(void)
{
  typedef int (^IntBlock)();
  return 0;
}
