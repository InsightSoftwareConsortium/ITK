/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkConversion_h
#define itkConversion_h


namespace itk
{
namespace Experimental
{

/**
 * \class Conversion
 * Utility class for type conversion.
 *
 * \author Niels Dekker, LKEB, Leiden University Medical Center
 */
class Conversion
{
public:
  /** Returns the result of converting the function argument `arg` to
   * destination type `TDestination`, if the argument is implicitly convertible
   * to that type. Otherwise returns a value of type T, initialized by `{}`
   * (which is null for pointers, and zero for numerical types).
   */
  template <typename TDestination, typename TSource>
  static constexpr TDestination
  Convert(TSource arg)
  {
    return PrivateConvert<TDestination>(arg);
  }

private:
  template <typename T>
  static constexpr T
  PrivateConvert(T arg)
  {
    return arg;
  }

  template <typename T>
  static constexpr T
  PrivateConvert(...)
  {
    return {};
  }
};

} // namespace Experimental
} // namespace itk

#endif
