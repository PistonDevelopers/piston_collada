# piston_collada
Rust library for parsing COLLADA files, intended for use within the Piston ecosystem.

#### Todo
* Currently depends on https://github.com/PistonDevelopers/wavefront_obj for its data structure declarations, so that
 COLLADA files can also be read into https://github.com/PistonDevelopers/geometry. Maybe those should be moved.
* Pull vertex weights from ColladaDocument into some general structure.
* Pull skeleton from ColladaDocument into some general structure.
* Pull animation data from ColladaDocument into some general structure.
