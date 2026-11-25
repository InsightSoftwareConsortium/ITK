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
#
# Exercises the new Python wrapping for itk::ObjectToObjectMultiMetricv4
# together with the canonical 3-argument template form (TFixedImage,
# TMovingImage, TVirtualImage) of itk::ImageToImageMetricv4 and its derived
# classes, while asserting that the historical 2-argument form continues to
# resolve to the same SWIG class for backward compatibility.
#
# AddMetric() upcast helper
# -------------------------
# itk::ObjectToObjectMultiMetricv4<F, M, V>::AddMetric expects a pointer to
# itk::ObjectToObjectMetric<F, M, V>. SWIG cannot auto-upcast across the
# POINTER_WITH_2_SUPERCLASSES chain on the derived image-metric classes, so
# the caller has to perform an explicit upcast. itk.as_metric_base() wraps
# that idiom.

import itk

ImageType = itk.Image[itk.F, 2]

# ---------------------------------------------------------------------------
# Backward compatibility: the historical 2-argument template key continues
# to resolve. Any existing user code that omits the explicit TVirtualImage
# parameter must keep working.
# ---------------------------------------------------------------------------
i2i_2arg = itk.ImageToImageMetricv4[ImageType, ImageType].New()
ms_2arg = itk.MeanSquaresImageToImageMetricv4[ImageType, ImageType].New()
mattes_2arg = itk.MattesMutualInformationImageToImageMetricv4[
    ImageType, ImageType
].New()
corr_2arg = itk.CorrelationImageToImageMetricv4[ImageType, ImageType].New()
demons_2arg = itk.DemonsImageToImageMetricv4[ImageType, ImageType].New()
ants_2arg = itk.ANTSNeighborhoodCorrelationImageToImageMetricv4[
    ImageType, ImageType
].New()
jh_2arg = itk.JointHistogramMutualInformationImageToImageMetricv4[
    ImageType, ImageType
].New()
for inst in (
    i2i_2arg,
    ms_2arg,
    mattes_2arg,
    corr_2arg,
    demons_2arg,
    ants_2arg,
    jh_2arg,
):
    assert inst is not None

# ---------------------------------------------------------------------------
# New 3-argument key (canonical) resolves to the same SWIG class.
# ---------------------------------------------------------------------------
i2i_3arg = itk.ImageToImageMetricv4[ImageType, ImageType, ImageType].New()
ms_3arg = itk.MeanSquaresImageToImageMetricv4[ImageType, ImageType, ImageType].New()
assert type(i2i_2arg).__name__ == type(i2i_3arg).__name__
assert type(ms_2arg).__name__ == type(ms_3arg).__name__

# ---------------------------------------------------------------------------
# New ObjectToObjectMultiMetricv4 wrapping accepts metrics created via
# either template-key form, exercising AddMetric.
# ---------------------------------------------------------------------------
multi_metric = itk.ObjectToObjectMultiMetricv4[2, 2, ImageType].New()

multi_metric.AddMetric(itk.as_metric_base(ms_2arg))
multi_metric.AddMetric(itk.as_metric_base(mattes_2arg))
multi_metric.AddMetric(itk.as_metric_base(ms_3arg))
assert multi_metric.GetNumberOfMetrics() == 3

weights = itk.Array[itk.D](3)
weights[0] = 0.2
weights[1] = 0.3
weights[2] = 0.5
multi_metric.SetMetricWeights(weights)
assert multi_metric.GetMetricWeights().GetSize() == 3

print("Test passed!")
