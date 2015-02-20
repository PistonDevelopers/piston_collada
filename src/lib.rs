#![feature(old_io)]
#![feature(old_path)]
#![feature(core)]
#![feature(collections)]

extern crate wavefront_obj;
extern crate xml;

use wavefront_obj::obj::*;

pub type Matrix4 = [[f32; 4]; 4];

#[derive(Debug)]
pub struct Skeleton {
    pub joints: Vec<Joint>
}

#[derive(Debug)]
pub struct Joint {
    pub inverse_bind_pose: Matrix4,
    pub name: String,
    pub parent_index: usize,
}

#[derive(Debug)]
pub struct BindDataSet {
    pub bind_data: Vec<BindData>,
}

#[derive(Debug)]
pub struct BindData {
    pub object_name: String,
    pub skeleton_name: String,
    pub joint_names: Vec<String>,
    pub vertex_weights: Vec<VertexWeight>,
    pub inverse_bind_poses: Vec<Matrix4>,
}

#[derive(Debug)]
pub struct VertexWeight {
    pub vertex: VertexIndex,
    pub joint: JointIndex,
    pub weight: WeightIndex,
}

pub type WeightIndex = usize;
pub type JointIndex = usize;

pub mod document;
