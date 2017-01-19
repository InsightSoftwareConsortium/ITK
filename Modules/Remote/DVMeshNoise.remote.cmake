# Contact: Davis Marc Vigneault <davis.vigneault@gmail.com>
itk_fetch_module(DVMeshNoise
  "Perturb itk::Mesh and itk::QuadEdgeMesh coordinates with Gaussian noise.
  Please see the following Insight Journal article for an introduction to this module:
  Vigneault, DM.  Perturbing Mesh Vertices with Additive Gaussian Noise.
  http://hdl.handle.net/10380/3567
  "
  GIT_REPOSITORY ${git_protocol}://github.com/DVigneault/DVMeshNoise
  GIT_TAG e7a360ec0aad87035d0b231741f63833c0ffe6f7)
