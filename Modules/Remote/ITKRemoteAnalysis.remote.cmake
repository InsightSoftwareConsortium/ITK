# ITKRemoteAnalysis — consolidated remote module group
#
# Contains analysis-domain remote modules:
#   TextureFeatures, BoneMorphometry, BoneEnhancement, Thickness3D,
#   IsotropicWavelets, RANSAC, PerformanceBenchmarking, StructuralSimilarity
#
# Enable with: cmake -DITKGroup_Remote_Analysis=ON
# Individual modules toggleable via Module_<name>=ON/OFF
#
# See https://github.com/InsightSoftwareConsortium/ITK/issues/6060

itk_fetch_module_group(Analysis
  "Analysis domain: TextureFeatures, BoneMorphometry, BoneEnhancement, Thickness3D, IsotropicWavelets, RANSAC, PerformanceBenchmarking, StructuralSimilarity"
  GIT_REPOSITORY /Users/johnsonhj/src/ITKRemoteAnalysis
  GIT_TAG main
)
