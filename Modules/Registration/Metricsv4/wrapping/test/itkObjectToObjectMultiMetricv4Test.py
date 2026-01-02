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
# ==========================================================================

import itk

# Test with float images
Dimension = 2
ImageType = itk.Image[itk.F, Dimension]

# Test basic instantiation
multi_metric = itk.ObjectToObjectMultiMetricv4[Dimension, Dimension, ImageType].New()
assert multi_metric is not None

# Create a simple metric to add - now with explicit TVirtualImage
ms_metric = itk.MeanSquaresImageToImageMetricv4[ImageType, ImageType, ImageType].New()
assert ms_metric is not None

# Add metric to the multi-metric
multi_metric.AddMetric(ms_metric)

# Test that we can get the number of metrics
assert multi_metric.GetNumberOfMetrics() == 1

# Set metric weights
weights = itk.Array[itk.D](1)
weights[0] = 0.5
multi_metric.SetMetricWeights(weights)

# Verify weights were set
retrieved_weights = multi_metric.GetMetricWeights()
assert retrieved_weights.GetSize() == 1

print("Test passed!")
