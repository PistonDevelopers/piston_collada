use std::str::FromStr;
use std::old_io::{File};

use wavefront_obj::obj::*;
use xml;
use xml::Element;
use xml::Xml::{ElementNode, CharacterNode};

use {BindDataSet, BindData, Skeleton, Joint, VertexWeight, JointIndex, ROOT_JOINT_PARENT_INDEX, mat4_id};
use utils::*;

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

    ///
    ///
    ///
    pub fn get_skeletons(&self) -> Option<Vec<Skeleton>> {
        let library_visual_scenes = try_some!(self.root_element.get_child("library_visual_scenes", self.get_ns()));
        let visual_scene = try_some!(library_visual_scenes.get_child("visual_scene", self.get_ns()));

        let bind_data_set = try_some!(self.get_bind_data_set());

        let skeleton_ids: Vec<&str> = pre_order_iter(visual_scene)
            .filter(|e| e.name == "skeleton")
            .filter_map(|s| if let CharacterNode(ref id) = s.children[0] { Some(&id[..]) } else { None })
            .map(|id| id.trim_left_matches('#'))
            .collect();

        let skeletons = pre_order_iter(visual_scene)
            .filter(|e| e.name == "node")
            .filter(|e| has_attribute_with_value(e, "id", skeleton_ids[0]))
            .filter_map(|e| self.get_skeleton(e, &bind_data_set.bind_data[0]))
            .collect();

        Some(skeletons)
    }

    fn get_skeleton(&self, root_element: &Element, bind_data: &BindData) -> Option<Skeleton> {

        let mut parent_index_stack: Vec<JointIndex> = vec![ROOT_JOINT_PARENT_INDEX];
        let mut last_depth = -1;
        let mut joints = Vec::new();
        let mut bind_poses = Vec::new();

        for (joint_index, (joint_element, depth)) in pre_order_with_depth_iter(root_element)
            .filter(|&(e, _)| e.name == "node" && has_attribute_with_value(e, "type", "JOINT"))
            .enumerate()
        {
            let depth = depth as i32;
            if depth <= last_depth { parent_index_stack.pop(); }
            if depth < last_depth { parent_index_stack.pop(); }

            let joint_name = joint_element.get_attribute("id", None).unwrap().to_string();

            let mut joint_names_with_bind_pose = bind_data.joint_names.iter().zip(bind_data.inverse_bind_poses.iter());
            let inverse_bind_pose = match joint_names_with_bind_pose.find(|&(name, _)| *name == joint_name) {
                Some((_, pose))  => *pose,
                _                   => mat4_id(),
            };

            joints.push(Joint {
                inverse_bind_pose: inverse_bind_pose,
                name: joint_name,
                parent_index: *parent_index_stack.last().unwrap(),
            });

            let pose_matrix_element = try_some!(joint_element.get_child("matrix", self.get_ns()));
            let pose_matrix_array = try_some!(get_array_content(pose_matrix_element));
            let mut pose_matrix = mat4_id();
            for (&array_value, matrix_value) in pose_matrix_array.iter().zip(pose_matrix.iter_mut().flat_map(|n| n.iter_mut())) {
                *matrix_value = array_value;
            }
            bind_poses.push(pose_matrix);

            parent_index_stack.push(joint_index as JointIndex);

            last_depth = depth;
        }

        Some(Skeleton {
            joints: joints,
            bind_poses: bind_poses,
        })
    }

    fn get_bind_data(&self, controller_element: &xml::Element) -> Option<BindData> {

        let skeleton_name = try_some!(controller_element.get_attribute("name", None));
        let skin_element = try_some!(controller_element.get_child("skin", self.get_ns()));
        let object_name = try_some!(skin_element.get_attribute("source", None)); // FIXME includes "#" ..

        let vertex_weights_element = try_some!(skin_element.get_child("vertex_weights", self.get_ns()));
        let vertex_weights = try_some!(self.get_vertex_weights(vertex_weights_element));

        let joints_element = try_some!(skin_element.get_child("joints", self.get_ns()));

        let joint_input = try_some!(self.get_input(joints_element, "JOINT"));
        let joint_names = try_some!(self.get_array_for_input(skin_element, joint_input));

        let weights_input = try_some!(self.get_input(vertex_weights_element, "WEIGHT"));
        let weights = try_some!(self.get_array_for_input(skin_element, weights_input));

        let inv_bind_matrix_input = try_some!(self.get_input(joints_element, "INV_BIND_MATRIX"));
        let inverse_bind_poses = try_some!(self.get_array_for_input(skin_element, inv_bind_matrix_input))
            .chunks(16).map(|chunk| {
                let mut matrix = [[0f32; 4]; 4];
                for (&chunk_value, matrix_value) in chunk.iter().zip(matrix.iter_mut().flat_map(|n| n.iter_mut())) {
                    *matrix_value = chunk_value;
                }
                matrix
            }).collect();

        Some(BindData{
            object_name: object_name.to_string(),
            skeleton_name: skeleton_name.to_string(),
            joint_names: joint_names,
            inverse_bind_poses: inverse_bind_poses,
            weights: weights,
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
        for n in weights_per_vertex.iter() {
            for _ in range(0, *n) {
                vertex_indices.push(*n);
            }
        }

        let vertex_weights = vertex_indices.iter().filter_map( |vertex_index| {
            match joint_weight_iter.next() {
                Some(joint_weight) => {
                    Some(VertexWeight {
                        vertex: *vertex_index,
                        joint: joint_weight[joint_index_offset] as JointIndex,
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

        let polylist_element = try_some!(mesh_element.get_child("polylist", self.get_ns()));

        let positions_input = try_some!(self.get_input(polylist_element, "VERTEX"));
        let positions_array = try_some!(self.get_array_for_input(mesh_element, positions_input));
        let positions = positions_array.chunks(3).map(|coords| {
            Vertex {
                x: coords[0],
                y: coords[1],
                z: coords[2],
            }
        }).collect();

        let normals = {
            match self.get_input(polylist_element, "NORMAL") {
                Some(normals_input) => {
                    let normals_array = try_some!(self.get_array_for_input(mesh_element, normals_input));
                    normals_array.chunks(3).map(|coords| {
                        Normal {
                            x: coords[0],
                            y: coords[1],
                            z: coords[2],
                        }
                    }).collect()
                }
                None => Vec::new()
            }
        };

        let texcoords = {
            match self.get_input(polylist_element, "TEXCOORD") {
                Some(texcoords_input) => {
                    let texcoords_array = try_some!(self.get_array_for_input(mesh_element, texcoords_input));
                    texcoords_array.chunks(2).map(|coords| {
                        TVertex {
                            x: coords[0],
                            y: coords[1],
                        }
                    }).collect()
                }
                None => Vec::new()
            }
        };

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
            Some(ref ns) => Some(&ns[..]),
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

    fn get_array_for_input<T: FromStr>(&self, parent: &Element, input: &Element) -> Option<Vec<T>> {
        let source = try_some!(self.get_input_source(parent, input));
        let array_element = try_some!{
            if let Some(float_array) = source.get_child("float_array", self.get_ns()) {
                Some(float_array)
            } else if let Some(name_array) = source.get_child("Name_array", self.get_ns()) {
                Some(name_array)
            } else {
                None
            }
        };
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
                    // Polys with more than 3 vertices not supported - try to advance and continue
                    // TODO attempt to triangle-fy? (take a look at wavefront_obj)
                    for _ in range(0, n) { vtn_iter.next(); };
                    Shape::Point((0, None, None))
                }
            }
        }).collect();


        Some(shapes)
    }
}

#[test]
fn test_get_obj_set() {
    let collada_document = ColladaDocument::from_path(&Path::new("test_assets/test.dae")).unwrap();
    let obj_set = collada_document.get_obj_set().unwrap();
    assert_eq!(obj_set.objects.len(), 1);

    let ref object = obj_set.objects[0];
    assert_eq!(object.name, "BoxyWorm-mesh");
    assert_eq!(object.vertices.len(), 16);
    assert_eq!(object.tex_vertices.len(), 84);
    assert_eq!(object.normals.len(), 28);
    assert_eq!(object.geometry.len(), 1);

    let ref geometry = object.geometry[0];
    assert_eq!(geometry.shapes.len(), 28);

    let ref shape = geometry.shapes[1];
    if let &Shape::Triangle((position_index, Some(texture_index), Some(normal_index)), _, _) = shape {
        assert_eq!(position_index, 7);
        assert_eq!(texture_index, 3);
        assert_eq!(normal_index, 1);
    } else {
        assert!(false);
    }
}

#[test]
fn test_get_bind_data_set() {
    let collada_document = ColladaDocument::from_path(&Path::new("test_assets/test.dae")).unwrap();
    let bind_data_set = collada_document.get_bind_data_set().unwrap();
    let bind_data = &bind_data_set.bind_data[0];

    assert_eq!(bind_data.object_name, "#BoxyWorm-mesh"); // FIXME
    assert_eq!(bind_data.skeleton_name, "BoxWormRoot"); // FIXME
    assert_eq!(bind_data.joint_names, ["Root", "UpperArm", "LowerArm"]);
    assert_eq!(bind_data.vertex_weights.len(), 29);
    assert_eq!(bind_data.weights.len(), 29);
    assert_eq!(bind_data.inverse_bind_poses.len(), 3);
}

#[test]
fn test_get_skeletons() {
    let collada_document = ColladaDocument::from_path(&Path::new("test_assets/test.dae")).unwrap();
    let skeletons = collada_document.get_skeletons().unwrap();
    assert_eq!(skeletons.len(), 1);

    let skeleton = &skeletons[0];
    assert_eq!(skeleton.joints.len(), 4);
    assert_eq!(skeleton.bind_poses.len(), 4);

    assert_eq!(skeleton.joints[0].name, "Root");
    assert_eq!(skeleton.joints[0].parent_index, (0 as JointIndex) - 1);
    assert!(skeleton.joints[0].inverse_bind_pose != mat4_id());

    assert_eq!(skeleton.joints[1].name, "UpperArm");
    assert_eq!(skeleton.joints[1].parent_index, 0);
    assert!(skeleton.joints[1].inverse_bind_pose != mat4_id());

    assert_eq!(skeleton.joints[2].name, "UpperArmDanglyBit");
    assert_eq!(skeleton.joints[2].parent_index, 1);
    assert_eq!(skeleton.joints[2].inverse_bind_pose, mat4_id());

    assert_eq!(skeleton.joints[3].name, "LowerArm");
    assert_eq!(skeleton.joints[3].parent_index, 0);
    assert!(skeleton.joints[3].inverse_bind_pose != mat4_id());
}
