import sys

import itk

image = itk.imread(sys.argv[1])

if len(sys.argv) > 3:
    isosurface = sys.argv[3]
else:
    calculator = itk.MinimumMaximumImageCalculator[type(image)].New()
    calculator.SetImage(image)
    calculator.Compute()
    isosurface = (calculator.GetMaximum() + calculator.GetMinimum()) / 2

# convert number type
(input_image_template, (input_pixel_type, input_image_dimension)) = itk.template(image)
if input_pixel_type == itk.F or input_pixel_type == itk.D:
    isosurface = float(isosurface)
else:
    isosurface = int(isosurface)

mesh = itk.cuberille_image_to_mesh_filter(
    image,
    generate_triangle_faces=True,
    iso_surface_value=isosurface,
    project_vertices_to_iso_surface=True,
    project_vertex_surface_distance_threshold=0.05,
)

itk.meshwrite(mesh, sys.argv[2])
