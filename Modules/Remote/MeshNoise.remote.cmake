# Contact: Davis Marc Vigneault <davis.vigneault@gmail.com>
itk_fetch_module(MeshNoise
  "Perturb itk::Mesh and itk::QuadEdgeMesh coordinates with Gaussian noise.
  Please see the following Insight Journal article for an introduction to this module:
  Vigneault, DM.  Perturbing Mesh Vertices with Additive Gaussian Noise.
  http://hdl.handle.net/10380/3567
  "
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKMeshNoise.git
  GIT_TAG 14dc50513fd5324b1cd75ccb4752572aab9ea617
  )
