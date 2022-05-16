# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/

import itk
itk.auto_progress(2)

n_channels = 31

# Verify UC addition operation
vector_type = itk.VariableLengthVector[itk.UC]
vector1 = vector_type(n_channels)
vector2 = vector_type(n_channels)
assert len(vector1) == n_channels and len(vector2) == n_channels

vector1.Fill(16)
for idx in range(n_channels):
    vector2[idx] = idx

sum = vector1 + vector2
print(f'UC sum: {sum}')

for idx in range(n_channels):
    assert sum[idx] == 16 + idx, "Got unexpected result from vector sum"

# Verify float addition operation
vector_float_type = itk.VariableLengthVector[itk.F]
vector3 = vector_float_type(n_channels)
vector4 = vector_float_type(n_channels)
assert len(vector3) == n_channels and len(vector4) == n_channels

vector3.Fill(0.5)
for idx in range(n_channels):
    vector4.SetElement(idx, 0.1 * idx)

float_sum = vector3 + vector4
print(f'float sum: {float_sum}')

tolerance = 1e-6
for idx in range(n_channels):
    diff = abs(float_sum[idx] - (0.5 + 0.1 * idx))
    print(f'float sum[{idx}]: {float_sum[idx]:0.9f} diff: {diff:0.2e}')
    assert diff < tolerance, "Got unexpected result from vector float sum"
