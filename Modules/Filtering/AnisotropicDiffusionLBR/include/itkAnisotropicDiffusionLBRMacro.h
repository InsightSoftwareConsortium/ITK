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

//
//  Created by Jean-Marie Mirebeau on 07/03/2014.
//
//

#ifndef itkAnisotropicDiffusionLBRMacro_h
#define itkAnisotropicDiffusionLBRMacro_h

/**
 Getters and setters for functor types, inspired by UnaryFunctorImageFilter.
 No equality test performed for the setter.
 */
#define GetSetFunctorMacro(name, type)                                                                                 \
  virtual type &       Get##name() { return this->m_##name; }                                                          \
  virtual const type & Get##name() const { return this->m_##name; }                                                    \
  virtual void         Set##name(const type & _arg)                                                                    \
  {                                                                                                                    \
    m_##name = _arg;                                                                                                   \
    this->Modified();                                                                                                  \
  }

#endif
