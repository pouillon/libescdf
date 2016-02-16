#!/usr/bin/python3

# This script is meant to be called from the top source directory

import os
import sys

# Add path to ESCDF Python modules before calling them
pkg_dir, x = os.path.split(os.path.abspath(__file__))
pkg_dir, x = os.path.split(pkg_dir)
sys.path.insert(0, pkg_dir)

from escdf.fortran import EscdfFortranModule

# Fortran module
geo_mod_text = str(EscdfFortranModule(
    "geometry", "./python/specs/escdf-specs-geometry-0.1.yml"))
geo_mod_file = "./fortran/escdf_geometry.F90"
with open(geo_mod_file, "w") as geo_mod:
    geo_mod.write(geo_mod_text)
