#![feature(std_misc)]
#![feature(old_io)]
#![feature(old_path)]
#![feature(core)]
#![feature(collections)]

extern crate wavefront_obj;
extern crate xml;

use wavefront_obj::obj::*;
use std::num::{Float};

// Necessary?
// pub type Vector3<T> = [T; 3];
// pub type Quaternion<T> = [T; 4];

pub type Matrix4<T> = [[T; 4]; 4];
pub fn mat4_id<T: Float>() -> Matrix4<T> {
    let _1 = Float::one();
    let _0 = Float::zero();
    [
        [_1, _0, _0, _0],
        [_0, _1, _0, _0],
        [_0, _0, _1, _0],
        [_0, _0, _0, _1],
    ]
}

#[derive(Debug)]
pub struct Skeleton {
    ///
    /// All joints in the skeleton
    ///
    pub joints: Vec<Joint>,

    ///
    /// Default parent-relative transforms for each joint (at time of vertex binding)
    ///
    pub bind_poses: Vec<Matrix4<f32>>,
}

#[derive(Debug)]
pub struct Joint {
    ///
    /// Name of joint
    ///
    pub name: String,

    ///
    /// Index of parent joint in Skeleton's 'joints' vector
    ///
    pub parent_index: JointIndex,

    ///
    /// Matrix transforming vertex coordinates from model-space to joint-space
    ///
    pub inverse_bind_pose: Matrix4<f32>,
}

///
/// Skeleton-Mesh Binding Data
///

#[derive(Debug)]
pub struct BindDataSet {
    pub bind_data: Vec<BindData>,
}

#[derive(Debug)]
pub struct BindData {
    pub object_name: String,
    pub skeleton_name: String,
    pub joint_names: Vec<String>,

    /// Vertex weights, for vertex by index in mesh and joint by index in 'joint_names'
    /// and weight by index in 'weights'
    pub vertex_weights: Vec<VertexWeight>,

    /// Weight values that are indexed by VertexWeights
    pub weights: Vec<f32>,

    /// Inverse bind pose matrices listed in order of joint_names
    pub inverse_bind_poses: Vec<Matrix4<f32>>,
}

#[derive(Debug)]
pub struct VertexWeight {
    pub vertex: VertexIndex,
    pub joint: JointIndex,
    pub weight: WeightIndex,
}

pub type WeightIndex = usize;

pub type JointIndex = u8;
pub const ROOT_JOINT_PARENT_INDEX: JointIndex  = 0 as JointIndex - 1;

pub mod document;
mod utils;
