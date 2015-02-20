#![feature(old_io)]
#![feature(old_path)]
#![feature(core)]
#![feature(collections)]

extern crate wavefront_obj;
extern crate xml;

use wavefront_obj::obj::*;

#[derive(Debug)]
pub struct BindDataSet {
    pub bind_data: Vec<BindData>,
}

#[derive(Debug)]
pub struct BindData {
    pub object_name: String,
    pub skeleton_name: String,
    pub vertex_weights: Vec<VertexWeight>,
}

#[derive(Debug)]
pub struct VertexWeight {
    pub vertex: VertexIndex,
    pub joint: JointIndex,
    pub weight: WeightIndex,
}

pub type JointIndex = usize;

pub mod document;
