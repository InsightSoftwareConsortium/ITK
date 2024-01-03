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

#ifndef itkCopy_h
#define itkCopy_h

namespace itk
{

/** Returns a copy of its argument. Primarily used to make the act of copying (typically involving a copy-constructor
 * call) explicit, and to avoid warnings like:
 *
 * - Microsoft Visual Studio 2022, IntelliSense Code Linter [lnt-accidental-copy] "`auto` doesn't deduce references. A
 * possibly unintended copy is being made" (https://learn.microsoft.com/en-us/cpp/ide/lnt-accidental-copy)
 * - Microsoft Visual Studio 2022, Warning C26820, "This is a potentially expensive copy operation. Consider using a
 * reference unless a copy is required (p.9)" (https://learn.microsoft.com/en-us/cpp/code-quality/c26820?view=msvc-170)
 * - Synopsys Coverity 2023.6.2, "CID 331225:  Performance inefficiencies  (AUTO_CAUSES_COPY)", "Using the `auto`
 * keyword without an `&` causes the copy of an object...".
 * - SonarSource static code analysis, "Unnecessary expensive copy should be avoided when using `auto` as a placeholder
 * type", "Avoid this unnecessary copy by using a "const" reference. (cpp:S6032)",
 * https://rules.sonarsource.com/cpp/RSPEC-6032/
 *
 * Example:
   \code
     const auto possiblyUnintendedCopy = image->GetRequestedRegion(); // Warning!
     const auto intendedCopy = Copy(image->GetRequestedRegion());     // OK, no warning  :-)
     const auto [index, size] = Copy(image->GetRequestedRegion());    // OK, no warning  :-)
   \endcode
 *
 * \note In general, it is up to the programmer to choose between doing a copy and using a reference, of course. `Copy`
 should only be used when doing a copy is preferred over using a reference.
 */
template <typename T>
[[nodiscard]] constexpr T
Copy(const T & original)
{
  // May call the copy-constructor of `T`.
  return original;
}

} // namespace itk


#endif // itkCopy_h
