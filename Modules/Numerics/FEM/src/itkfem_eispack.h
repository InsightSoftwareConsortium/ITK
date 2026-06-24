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
#ifndef itkfem_eispack_h
#define itkfem_eispack_h

/* FEM-private EISPACK eigenvalue helpers (renamed tqlrat/pythag/epslon).
 * v3p_netlib drops eispack under ITK_FUTURE_LEGACY_REMOVE; the itk::fem ITPACK
 * solver (dsrc2c.c eigvns_) needs tqlrat, so FEM owns self-contained copies and
 * no longer depends on the v3p_netlib eispack in any configuration. */

#ifdef __cplusplus
extern "C"
{
#endif

  int
  itkfem_tqlrat_(long * n, double * d, double * e2, long * ierr);
  double
  itkfem_pythag_(double * a, double * b);
  double
  itkfem_epslon_(double * x);

#ifdef __cplusplus
}
#endif

#endif
