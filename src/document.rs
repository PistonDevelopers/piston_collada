use std::str::FromStr;
use std::old_io::{File};

use wavefront_obj::obj::*;
use xml;
use xml::Element;

use {BindDataSet, BindData, VertexWeight, JointIndex};

macro_rules! try_some {
    ($e:expr) => (match $e { Some(s) => s, None => return None })
}

pub struct ColladaDocument {
    pub root_element: xml::Element
}

impl ColladaDocument {

    ///
    /// Construct a ColladaDocument for the XML document at the given path
    ///
    pub fn from_path(path: &Path) -> Result<ColladaDocument, &'static str> {
        let file_result = File::open(path);

        let mut file = match file_result {
            Ok(file) => file,
            Err(_) => return Err("Failed to open COLLADA file at path.")
        };

        let xml_string = match file.read_to_string() {
            Ok(file_string) => file_string,
            Err(_) => return Err("Failed to read COLLADA file.")
        };

        match xml_string.parse() {
            Ok(root_element) => Ok(ColladaDocument{root_element: root_element}),
            Err(_) => Err("Error while parsing COLLADA document."),
        }
    }

    ///
    /// Populate and return an ObjSet for the meshes in the Collada document
    ///
    pub fn get_obj_set(&self) -> Option<ObjSet> {
        let library_geometries = try_some!(self.root_element.get_child("library_geometries", self.get_ns()));
        let geometries = library_geometries.get_children("geometry", self.get_ns());
        let objects = geometries.iter()
            .filter_map( |g| { self.get_object(g) }).collect();

        Some(ObjSet{
            material_library: None,
            objects: objects,
        })
    }

    ///
    /// Populate and return a BindDataSet from the Collada document
    ///
    pub fn get_bind_data_set(&self) -> Option<BindDataSet> {
        let library_controllers = try_some!(self.root_element.get_child("library_controllers", self.get_ns()));
        let controllers = library_controllers.get_children("controller", self.get_ns());
        let bind_data = controllers.iter()
            .filter_map( |c| { self.get_bind_data(c) }).collect();
        Some(BindDataSet{ bind_data: bind_data })
    }

    fn get_bind_data(&self, controller_element: &xml::Element) -> Option<BindData> {

        let skeleton_name = try_some!(controller_element.get_attribute("name", None));
        let skin_element = try_some!(controller_element.get_child("skin", self.get_ns()));
        let object_name = try_some!(skin_element.get_attribute("source", None)); // NOTE includes "#"

        let vertex_weights_element = try_some!(skin_element.get_child("vertex_weights", self.get_ns()));
        let vertex_weights = try_some!(self.get_vertex_weights(vertex_weights_element));

        let joint_input = try_some!(self.get_input(vertex_weights_element, "JOINT"));

        println!("{}", joint_input);

        let joint_names = try_some!(self.get_name_array(skin_element, joint_input));

        println!(".....");

        Some(BindData{
            object_name: object_name.to_string(),
            skeleton_name: skeleton_name.to_string(),
            joint_names: joint_names,
            inverse_bind_poses: Vec::new(), // TODO
            vertex_weights: vertex_weights
        })
    }

    fn get_vertex_weights(&self, vertex_weights_element: &xml::Element) -> Option<Vec<VertexWeight>> {

        let joint_index_offset = try_some!(self.get_input_offset(vertex_weights_element, "JOINT"));
        let weight_index_offset = try_some!(self.get_input_offset(vertex_weights_element, "WEIGHT"));

        let vcount_element = try_some!(vertex_weights_element.get_child("vcount", self.get_ns()));
        let weights_per_vertex: Vec<usize> = try_some!(get_array_content(vcount_element));
        let input_count = vertex_weights_element.get_children("input", self.get_ns()).len();

        let v_element = try_some!(vertex_weights_element.get_child("v", self.get_ns()));
        let joint_weight_indices: Vec<usize> = try_some!(get_array_content(v_element));
        let mut joint_weight_iter = joint_weight_indices.chunks(input_count);

        let mut vertex_indices: Vec<usize> = Vec::new();
        for (i, n) in weights_per_vertex.iter().enumerate() {
            for _ in range(0, *n) {
                vertex_indices.push(*n);
            }
        }

        let vertex_weights = vertex_indices.iter().filter_map( |vertex_index| {
            match joint_weight_iter.next() {
                Some(joint_weight) => {
                    Some(VertexWeight {
                        vertex: *vertex_index,
                        joint: joint_weight[joint_index_offset],
                        weight: joint_weight[weight_index_offset],
                    })
                }
                None => None
            }
        }).collect();

        Some(vertex_weights)
    }

    fn get_object(&self, geometry_element: &xml::Element) -> Option<Object> {
        let id = try_some!(geometry_element.get_attribute("id", None));
        let mesh_element = try_some!(geometry_element.get_child("mesh", self.get_ns()));
        let shapes = try_some!(self.get_shapes(mesh_element));
        let positions = self.get_coords_array(mesh_element, 3, "VERTEX", |coords| {
            Vertex {
                x: coords[0],
                y: coords[1],
                z: coords[2],
            }
        });
        let normals = self.get_coords_array(mesh_element, 3, "NORMAL", |coords| {
            Normal {
                x: coords[0],
                y: coords[1],
                z: coords[2],
            }
        });
        let texcoords = self.get_coords_array(mesh_element, 2, "TEXCOORD", |coords| {
            TVertex {
                x: coords[0],
                y: coords[1],
            }
        });
        Some(Object {
            name: id.to_string(),
            vertices: positions,
            tex_vertices: texcoords,
            normals: normals,
            geometry: vec![Geometry {
                material_name: None,
                smooth_shading_group: 0,
                shapes: shapes,
            }],
        })
    }

    fn get_ns(&self) -> Option<&str> {
        match self.root_element.ns {
            Some(ref ns) => Some(&ns[]),
            None => None,
        }
    }

    fn get_input_offset(&self, parent_element: &xml::Element, semantic : &str) -> Option<usize> {
        let inputs = parent_element.get_children("input", self.get_ns());
        let input = try_some!(inputs.iter().find( |i| {
            if let Some(s) = i.get_attribute("semantic", None) {
                s == semantic
            } else {
                false
            }
        }));
        try_some!(input.get_attribute("offset", None)).parse::<usize>().ok()
    }

    fn get_input<'a>(&'a self, parent: &'a Element, semantic : &str) -> Option<&'a Element> {
        let inputs = parent.get_children("input", self.get_ns());
        match inputs.iter().find( |i| {
            if let Some(s) = i.get_attribute("semantic", None) { s == semantic } else { false }
        })
        {
            Some(e) => Some(*e),
            None => None,
        }
    }

    fn get_input_source<'a>(&'a self, parent_element: &'a xml::Element, input_element: &'a xml::Element) -> Option<&'a xml::Element> {
        let source_id = try_some!(input_element.get_attribute("source", None));

        if let Some(element) = parent_element.children.iter()
            .filter_map(|node| { if let &xml::Xml::ElementNode(ref e) = node { Some(e) } else { None } })
            .find(|e| {
                if let Some(id) = e.get_attribute("id", None) {
                    let id = "#".to_string() + id;
                    id == source_id
                } else {
                    false
                }
            })
        {
            if element.name == "source" {
                return Some(element);
            } else {
                let input = try_some!(element.get_child("input", self.get_ns()));
                return self.get_input_source(parent_element, input);
            }
        }
        None
    }

    fn get_coords_array<T, F>(&self, mesh_element: &xml::Element, stride: usize, input_semantic: &str, constructor: F) -> Vec<T>
        where F: Fn(&[f64]) -> T
    {
        if let Some(float_array) = self.get_float_array(mesh_element, input_semantic) {
            return float_array.chunks(stride).map(constructor).collect();
        }
        Vec::new()
    }

    fn get_float_array(&self, mesh_element: &xml::Element, input_semantic: &str) -> Option<Vec<f64>> {
        let polylist_element = try_some!(mesh_element.get_child("polylist", self.get_ns()));
        let input = try_some!(self.get_input(polylist_element, input_semantic));
        let source = try_some!(self.get_input_source(mesh_element, input));
        self.get_array_from_source(source)
    }

    fn get_array_from_source<'a>(&'a self, source_element: &'a xml::Element) -> Option<Vec<f64>> {
        let array_element = try_some!(source_element.get_child("float_array", self.get_ns()));
        get_array_content(array_element)
    }

    fn get_name_array(&self, parent: &Element, input: &Element) -> Option<Vec<String>> {
        let source = try_some!(self.get_input_source(parent, input));
        let array_element = try_some!(source.get_child("Name_array", self.get_ns()));
        get_array_content(array_element)
    }

    fn get_vtn_indices(&self, mesh_element: &xml::Element) -> Option<Vec<VTNIndex>> {

        let polylist_element = try_some!(mesh_element.get_child("polylist", self.get_ns()));
        let p_element = try_some!(polylist_element.get_child("p", self.get_ns()));
        let indices: Vec<usize> = try_some!(get_array_content(p_element));

        let input_count = polylist_element.get_children("input", self.get_ns()).len();

        let position_offset = try_some!(self.get_input_offset(polylist_element, "VERTEX"));

        let normal_offset_opt = self.get_input_offset(polylist_element, "NORMAL");
        let texcoord_offset_opt = self.get_input_offset(polylist_element, "TEXCOORD");

        let vtn_indices: Vec<VTNIndex> = indices.chunks(input_count).map( |indices| {
            let position_index = indices[position_offset];

            let normal_index_opt = match normal_offset_opt {
                Some(normal_offset) => Some(indices[normal_offset]),
                None => None
            };

            let texcoord_index_opt = match texcoord_offset_opt {
                Some(texcoord_offset) => Some(indices[texcoord_offset]),
                None => None
            };

            (position_index, texcoord_index_opt, normal_index_opt)
        }).collect();

        Some(vtn_indices)
    }

    fn get_shapes(&self, mesh_element: &xml::Element) -> Option<Vec<Shape>> {

        let vtn_indices = try_some!(self.get_vtn_indices(mesh_element));

        let polylist_element = try_some!(mesh_element.get_child("polylist", self.get_ns()));
        let vcount_element = try_some!(polylist_element.get_child("vcount", self.get_ns()));
        let vertex_counts: Vec<usize> = try_some!(get_array_content(vcount_element));

        let mut vtn_iter = vtn_indices.iter();
        let shapes = vertex_counts.iter().map(|vertex_count| {
            match *vertex_count {
                1 => Shape::Point(*vtn_iter.next().unwrap()),
                2 => Shape::Line(*vtn_iter.next().unwrap(), *vtn_iter.next().unwrap()),
                3 => Shape::Triangle(*vtn_iter.next().unwrap(), *vtn_iter.next().unwrap(), *vtn_iter.next().unwrap()),
                n => {
                    // Not supported - try to advance and continue
                    for _ in range(0, n) { vtn_iter.next(); };
                    Shape::Point((0, None, None))
                }
            }
        }).collect();

        Some(shapes)
    }
}

fn parse_string_to_vector<T: FromStr>(string: &str) -> Vec<T> {
    string.trim()
        .split_str(" ")
        .map(|s| s.parse().ok().expect("Error parsing array in COLLADA file"))
        .collect()
}

fn get_array_content<T: FromStr>(element: &xml::Element) -> Option<Vec<T>> {
    match element.children[0] {
        xml::Xml::CharacterNode(ref contents) => Some(parse_string_to_vector(contents)),
        _ => None,
    }
}

/*
#[test]
fn test_get_obj_set() {
    let collada_document = ColladaDocument::from_path(&Path::new("test_assets/test.dae")).unwrap();
    let obj_set = collada_document.get_obj_set().unwrap();
    assert_eq!(obj_set.objects.len(), 1);

    let ref object = obj_set.objects[0];
    assert_eq!(object.name, "BoxyWorm-mesh");
    assert_eq!(object.vertices.len(), 16);
    //assert_eq!(object.tex_vertices.len(), 84);
    assert_eq!(object.normals.len(), 28);
    assert_eq!(object.geometry.len(), 1);

    let ref geometry = object.geometry[0];
    assert_eq!(geometry.shapes.len(), 28);

    let ref shape = geometry.shapes[1];
    //if let &Shape::Triangle((position_index, Some(texture_index), Some(normal_index)), _, _) = shape {
    if let &Shape::Triangle((position_index, None, Some(normal_index)), _, _) = shape {
        assert_eq!(position_index, 7);
        //assert_eq!(texture_index, 3);
        assert_eq!(normal_index, 1);
    } else {
        assert!(false);
    }
}
*/

#[test]
fn test_get_bind_data_set() {
    let collada_document = ColladaDocument::from_path(&Path::new("test_assets/test.dae")).unwrap();
    let bind_data_set = collada_document.get_bind_data_set().unwrap();
    println!("{:?}", bind_data_set);
}
